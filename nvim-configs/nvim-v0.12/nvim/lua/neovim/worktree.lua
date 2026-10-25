-- Git worktrees: list / switch / create / remove via Snacks pickers.
-- All git calls are async; switching keeps the previous worktree's LSP
-- clients alive so jumping back is instant.
local M = {}

local uv = vim.uv

M._items = {} -- last parsed `git worktree list`
M._prev = nil -- worktree we last switched away from

local function notify(msg, level, opts)
    Snacks.notify(msg, vim.tbl_extend("force", { id = "worktree", title = "Worktree", level = level }, opts or {}))
end

---@param cb fun(ok: boolean, stdout: string, stderr: string)
local function git(args, cwd, cb)
    vim.system(vim.list_extend({ "git" }, args), { cwd = cwd, text = true }, vim.schedule_wrap(function(r)
        cb(r.code == 0, vim.trim(r.stdout or ""), vim.trim(r.stderr or ""))
    end))
end

-- Root of the worktree containing `path` (default: cwd)
local function toplevel(path)
    local root = vim.fs.root(path or uv.cwd(), ".git")
    return root and (uv.fs_realpath(root) or root)
end

-- Common git dir if `root` is a linked worktree, nil otherwise
local function linked(root)
    local st = uv.fs_stat(root .. "/.git")
    if not (st and st.type == "file") then return end
    local f = io.open(root .. "/.git")
    local line = f and f:read("*l")
    if f then f:close() end
    local common = line and line:match("^gitdir: (.*)/worktrees/[^/]+$")
    if not common then return end -- e.g. a submodule
    if not common:match("^/") then common = root .. "/" .. common end
    return uv.fs_realpath(common) or common
end

local function under(path, root)
    return path == root or vim.startswith(path, root .. "/")
end

-- Worktree (from M._items) that owns `path`
local function owner(path)
    local best
    for _, wt in ipairs(M._items) do
        if under(path, wt.path) and (not best or #wt.path > #best) then best = wt.path end
    end
    return best
end

-- Path of a file buffer inside `root`, relative to it
local function relpath(buf, root)
    if vim.bo[buf].buftype ~= "" then return end
    local name = vim.api.nvim_buf_get_name(buf)
    if name == "" then return end
    name = uv.fs_realpath(name) or name
    if vim.startswith(name, root .. "/") then return name:sub(#root + 2) end
end

local function parse(out)
    local items, cur = {}, nil
    for line in vim.gsplit(out, "\n", { plain = true }) do
        local k, v = line:match("^(%S+) ?(.*)$")
        if k == "worktree" then
            cur = { path = v, main = #items == 0 }
            items[#items + 1] = cur
        elseif cur and k == "HEAD" then
            cur.head = v
        elseif cur and k == "branch" then
            cur.branch = (v:gsub("^refs/heads/", ""))
        elseif cur and k then
            cur[k] = true -- bare / detached / locked / prunable
        end
    end
    return items
end

local function list(root, cb)
    git({ "worktree", "list", "--porcelain" }, root, function(ok, out, err)
        if not ok then return notify(err, "error") end
        M._items = parse(out)
        cb(M._items)
    end)
end

-- Stop clients of worktrees not in `keep` that have nothing attached
local function prune_lsp(keep)
    for _, client in ipairs(vim.lsp.get_clients()) do
        local wt = client.root_dir and owner(client.root_dir)
        if wt and not keep[wt] and next(client.attached_buffers) == nil then client:stop() end
    end
end

-- Forget everything Neovim holds for the worktree at `path`
local function wipe(path)
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if relpath(buf, path) then pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
    end
    for _, client in ipairs(vim.lsp.get_clients()) do
        if client.root_dir and under(client.root_dir, path) then client:stop() end
    end
    if M._prev == path then M._prev = nil end
end

function M.switch(path)
    path = uv.fs_realpath(path)
    if not path then return notify("Worktree directory is missing", "warn") end
    local old = toplevel()
    if path == old then return end

    if old then
        local modified = vim.tbl_filter(function(buf)
            return vim.bo[buf].modified and relpath(buf, old) ~= nil
        end, vim.api.nvim_list_bufs())
        if #modified > 0 then
            return Snacks.picker.util.confirm(("Save %d modified buffer(s) and switch?"):format(#modified), function()
                for _, buf in ipairs(modified) do
                    vim.api.nvim_buf_call(buf, function() vim.cmd("silent update") end)
                end
                M.switch(path)
            end)
        end
    end

    -- Stay in the same subdirectory when the new worktree has it
    local sub = old and uv.cwd():sub(#old + 2) or ""
    local dir = sub ~= "" and vim.fs.joinpath(path, sub) or path
    vim.cmd.cd(vim.fn.fnameescape(uv.fs_stat(dir) and dir or path))

    if old then
        -- old buffer -> new buffer (false when the file is gone). New buffers are
        -- only registered, so LSP attaches just to the ones shown in a window.
        local map = {}
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            local rel = relpath(buf, old)
            if rel then
                local file = vim.fs.joinpath(path, rel)
                map[buf] = vim.bo[buf].buflisted and uv.fs_stat(file) and vim.fn.bufadd(file) or false
                if map[buf] then vim.bo[map[buf]].buflisted = true end
            end
        end

        local empty
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            local new = map[vim.api.nvim_win_get_buf(win)]
            if new then
                local cursor = vim.api.nvim_win_get_cursor(win)
                vim.api.nvim_win_set_buf(win, new)
                cursor[1] = math.min(cursor[1], vim.api.nvim_buf_line_count(new))
                pcall(vim.api.nvim_win_set_cursor, win, cursor)
            elseif new == false then
                empty = empty or vim.api.nvim_create_buf(true, false)
                vim.api.nvim_win_set_buf(win, empty)
            end
        end

        for buf in pairs(map) do
            pcall(vim.api.nvim_buf_delete, buf, {})
        end
    end

    M._prev = old
    prune_lsp({ [path] = true, [old or ""] = true })
    notify("Switched to " .. vim.fs.basename(path), "info")
end

function M.add(base, name, root)
    root = root or toplevel()
    list(root, function(wts)
        for _, wt in ipairs(wts) do
            if wt.branch == name then return M.switch(wt.path) end
        end

        local main = wts[1].path
        local repo = vim.fs.basename(main):gsub("%.git$", "")
        local path = vim.fs.dirname(main) .. "/" .. repo .. "@" .. (name:gsub("/", "-"))
        if uv.fs_stat(path) then return notify(path .. " already exists", "error") end

        git({ "show-ref", "--verify", "--quiet", "refs/heads/" .. name }, root, function(exists)
            local args = exists and { "worktree", "add", path, name } or { "worktree", "add", "-b", name, path, base }
            notify("Creating " .. vim.fs.basename(path) .. "…", "info", { timeout = false })
            git(args, root, function(ok, _, err)
                if not ok then return notify(err, "error") end
                list(root, function() M.switch(path) end)
            end)
        end)
    end)
end

function M.create()
    local root = toplevel()
    if not root then return notify("Not in a git repository", "warn") end

    local picker
    git({ "fetch", "--all", "--prune", "--quiet" }, root, function(ok)
        if ok and picker and not picker.closed then picker:refresh() end
    end)

    picker = Snacks.picker.pick({
        title = "Base branch",
        layout = { preset = "select" },
        -- for-each-ref only reads ref names (no commit objects), so huge repos list instantly
        finder = function(_, ctx)
            return require("snacks.picker.source.proc").proc(ctx:opts({
                cmd = "git",
                args = { "for-each-ref", "--format=%(HEAD)%(refname)", "refs/heads", "refs/remotes" },
                cwd = root,
                transform = function(item)
                    local ref = item.text:sub(2)
                    local remote, short = ref:match("^refs/remotes/([^/]+)/(.+)$")
                    if short == "HEAD" then return false end
                    item.current = item.text:sub(1, 1) == "*"
                    item.remote, item.short = remote, short
                    item.branch = remote and (remote .. "/" .. short) or ref:sub(#"refs/heads/" + 1)
                    item.text = item.branch
                end,
            }), ctx)
        end,
        format = function(item)
            local ret = { item.current and { "● ", "SnacksPickerGitBranchCurrent" } or { "  " } }
            if item.remote then ret[#ret + 1] = { item.remote .. "/", "SnacksPickerDir" } end
            ret[#ret + 1] = { item.short or item.branch, "SnacksPickerGitBranch" }
            return ret
        end,
        on_show = function(p)
            for i, item in ipairs(p:items()) do
                if item.current then return p.list:view(i) end
            end
        end,
        confirm = function(p, item)
            p:close()
            if not item then return end
            Snacks.input({ prompt = "Branch (from " .. item.branch .. ")", default = item.short }, function(name)
                name = vim.trim(name or "")
                if name ~= "" then M.add(item.branch, name, root) end
            end)
        end,
    })
end

local function to_items(wts, root)
    local items, width = {}, 0
    for _, wt in ipairs(wts) do
        if not wt.bare then
            local label = wt.branch or ("@" .. wt.head:sub(1, 7))
            width = math.max(width, vim.api.nvim_strwidth(label))
            items[#items + 1] = vim.tbl_extend("force", wt, {
                text = label .. " " .. wt.path,
                label = label,
                current = wt.path == root,
            })
        end
    end
    for _, item in ipairs(items) do
        item.width = math.min(width, 40)
    end
    return items
end

local function format(item, picker)
    local util = Snacks.picker.util
    local tag = item.prunable and "  prunable" or item.locked and "  locked" or ""
    local win = picker.list.win
    local room = (win:valid() and vim.api.nvim_win_get_width(win.win) or 80) - item.width - #tag - 5
    local ret = {}
    ret[#ret + 1] = item.current and { "● ", "SnacksPickerGitBranchCurrent" } or { "  " }
    ret[#ret + 1] = { util.align(item.label, item.width, { truncate = true }), item.branch and "SnacksPickerGitBranch" or "SnacksPickerGitDetached" }
    ret[#ret + 1] = { "  " .. util.truncate(vim.fn.fnamemodify(item.path, ":~"), room, true), "SnacksPickerDir" }
    if tag ~= "" then ret[#ret + 1] = { tag, "SnacksPickerComment" } end
    return ret
end

local function remove(picker, item, root)
    if not item then return end
    if item.main or item.current then return notify("Can't remove the main or current worktree", "warn") end

    local function relist()
        list(root, function(wts)
            picker.opts.items = to_items(wts, root)
            picker:refresh()
        end)
    end

    local function done(delete_branch)
        wipe(item.path)
        if not delete_branch then return relist() end
        git({ "branch", "-D", item.branch }, root, function(ok, _, err)
            if not ok then notify(err, "error") end
            relist()
        end)
    end

    local choices = { "Remove worktree" }
    if item.branch then choices[2] = "Remove worktree + branch " .. item.branch end
    vim.ui.select(choices, { prompt = "Remove " .. vim.fs.basename(item.path) .. "?" }, function(_, idx)
        if not idx then return end
        if item.prunable then
            return git({ "worktree", "prune" }, root, function() done(idx == 2) end)
        end
        git({ "worktree", "remove", item.path }, root, function(ok, _, err)
            if ok then return done(idx == 2) end
            if not err:find("modified or untracked", 1, true) then return notify(err, "error") end
            Snacks.picker.util.confirm("Worktree has changes. Force remove?", function()
                git({ "worktree", "remove", "--force", item.path }, root, function(ok2, _, err2)
                    if not ok2 then return notify(err2, "error") end
                    done(idx == 2)
                end)
            end)
        end)
    end)
end

function M.pick()
    local root = toplevel()
    if not root then return notify("Not in a git repository", "warn") end

    list(root, function(wts)
        Snacks.picker.pick({
            title = "Worktrees",
            items = to_items(wts, root),
            format = format,
            layout = {
                preset = "select",
                config = function(layout)
                    for _, box in ipairs(layout.layout) do
                        if box.win == "list" and not box.height then
                            box.height = math.max(math.min(#M._items, vim.o.lines * 0.8 - 10), 2)
                        end
                    end
                end,
            },
            -- Land on the previous worktree: <leader>gw<CR> toggles like `cd -`
            on_show = function(picker)
                for i, item in ipairs(picker:items()) do
                    if item.path == M._prev then return picker.list:view(i) end
                end
            end,
            confirm = function(picker, item)
                picker:close()
                if item then M.switch(item.path) end
            end,
            actions = {
                worktree_create = function(picker)
                    picker:close()
                    M.create()
                end,
                worktree_remove = function(picker, item) remove(picker, item, root) end,
            },
            win = {
                input = {
                    keys = {
                        ["<c-o>"] = { "worktree_create", mode = { "n", "i" } },
                        ["<c-x>"] = { "worktree_remove", mode = { "n", "i" } },
                    },
                },
            },
        })
    end)
end

-- Statusline: cached, recomputed on DirChanged (filesystem only, no git spawn)
local status = ""

function M.refresh_status()
    local root = toplevel()
    status = root and linked(root) and "󰙅" or ""
end

function M.statusline()
    return status
end

-- rust-analyzer target dir shared by every worktree of a repo:
-- <main worktree>/<path of dir within its worktree>/target/rust-analyzer
function M.shared_target_dir(dir)
    local top = dir and toplevel(dir)
    if not top then return end
    local common = linked(top)
    local main = common and common:match("^(.*)/%.git$") or (not common and top)
    if not main then return end -- bare repo
    dir = uv.fs_realpath(dir) or dir
    return vim.fs.joinpath(main, dir:sub(#top + 2), "target", "rust-analyzer")
end

M.refresh_status()

return M
