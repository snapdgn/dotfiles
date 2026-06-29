require("nvim-autopairs").setup({ map_cr = false })
--require("nvim-surround").setup()
-- nerdcommenter works out of the box via its plugin/ files

-- Integrated <CR>: confirm completion when popup is visible, otherwise autopairs CR
-- replace_keycodes=false because autopairs_cr() returns pre-escaped keys
local function t(keys)
    return vim.api.nvim_replace_termcodes(keys, true, true, true)
end
vim.keymap.set('i', '<CR>', function()
    if vim.fn.pumvisible() ~= 0 then
        local info = vim.fn.complete_info({ 'selected' })
        if info.selected == -1 then
            return t('<C-n><C-y>')
        end
        return t('<C-y>')
    end
    return require('nvim-autopairs').autopairs_cr()
end, { expr = true, replace_keycodes = false })
