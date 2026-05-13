-- Typst compile on save
vim.api.nvim_create_autocmd("BufWritePost", {
    pattern = "*.typ",
    callback = function()
        local file = vim.fn.expand("%:p")
        local pdf = vim.fn.expand("%:p:r") .. ".pdf"
        vim.fn.jobstart({ "typst", "compile", "--synctex", file, pdf })
    end,
})

-- Open PDF preview
vim.keymap.set("n", "<leader>tp", function()
    local pdf = vim.fn.expand("%:p:r") .. ".pdf"
    vim.fn.jobstart({ "open", pdf })
end, { desc = "Open Typst PDF" })
