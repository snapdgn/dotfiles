-- Auto-enable treesitter highlighting for any filetype with an installed parser
vim.api.nvim_create_autocmd('FileType', {
    callback = function(ev)
        local lang = vim.treesitter.language.get_lang(ev.match)
        if lang and pcall(vim.treesitter.language.inspect, lang) then
            vim.treesitter.start(ev.buf, lang)
        end
    end,
})

vim.filetype.add({
    pattern = {
        ["*%.bf"] = "brainfuck",
        [".*/hypr/.*%.conf"] = "hyprlang",
    },
})

-- Treesitter context
require("treesitter-context").setup({
    enable = true,
    mode = "cursor",
    max_lines = 3,
    trim_scope = "inner",
})

vim.keymap.set("n", "<leader>ut", function()
    require("treesitter-context").toggle()
end, { desc = "Toggle Treesitter Context" })
