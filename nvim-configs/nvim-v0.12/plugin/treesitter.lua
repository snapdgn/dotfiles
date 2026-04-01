-- nvim 0.12: highlight/indent are built-in, nvim-treesitter just manages parser installs
-- Ensure key parsers are installed on first run
vim.api.nvim_create_autocmd("VimEnter", {
    once = true,
    callback = function()
        local installed = require("nvim-treesitter.config").get_installed("parsers")
        local wanted = { "lua", "go", "rust" }
        local missing = {}
        for _, lang in ipairs(wanted) do
            if not vim.tbl_contains(installed, lang) then
                table.insert(missing, lang)
            end
        end
        if #missing > 0 then
            vim.cmd("TSInstall " .. table.concat(missing, " "))
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
