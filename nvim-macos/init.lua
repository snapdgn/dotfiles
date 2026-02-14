local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

--vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  --pattern = { "*" }, -- Apply to all file types
  --callback = function(ev)
    --local curpos = vim.api.nvim_win_get_cursor(0) -- Save cursor position
    --vim.cmd([[keeppatterns %s/\s\+$//e]]) -- Remove trailing whitespaces
    --vim.api.nvim_win_set_cursor(0, curpos) -- Restore cursor position
  --end,
--})

require("neovim.set")
require("neovim.remap")
require("neovim.globals")
require("neovim.autocmd")

require("lazy").setup({
    spec = "plugins",
    change_detection = { notify = false }
})


--// insert uuidv4

local function generate_uuid()
    local handle = io.popen("uuidgen | tr A-F a-f | tr -d '\\n'")
    if not handle then return '' end
    local uuid = handle:read("*a")
    handle:close()
    return uuid or ''
end

vim.keymap.set("n", "<leader>gu", function()
    local uuid = generate_uuid()
    vim.api.nvim_put({ uuid }, 'c', true, true)
end, { desc = "Insert lowercase UUIDv4 from uuidgen" })
