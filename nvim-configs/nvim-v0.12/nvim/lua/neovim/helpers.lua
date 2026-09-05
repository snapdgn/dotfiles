-- UUID helper
local function generate_uuid()
    local handle = io.popen("uuidgen | tr A-F a-f | tr -d '\\n'")
    if not handle then return '' end
    local uuid = handle:read("*a")
    handle:close()
    return uuid or ''
end

-- generate uuid
vim.keymap.set("n", "<leader>gu", function()
    local uuid = generate_uuid()
    vim.api.nvim_put({ uuid }, 'c', true, true)
end, { desc = "Insert lowercase UUIDv4 from uuidgen" })

-- copy filepath
vim.keymap.set("n", "<leader>yf", function()
    vim.fn.setreg("+", vim.fn.expand("%:p"))
end, { desc = "Yank file path" })
