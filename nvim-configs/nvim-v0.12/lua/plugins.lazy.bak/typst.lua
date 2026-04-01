return {

    ------------------------------------------------
    -- Typst LSP (Tinymist)
    ------------------------------------------------
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                tinymist = {},
            },
        },
    },

    ------------------------------------------------
    -- Typst syntax highlighting
    ------------------------------------------------
    {
        "kaarmu/typst.vim",
        ft = "typst",
    },

    ------------------------------------------------
    -- Typst workflow
    ------------------------------------------------
    {
        "nvim-lua/plenary.nvim",
        ft = "typst",

        config = function()
            ------------------------------------------------
            -- Compile Typst automatically on save
            ------------------------------------------------
            vim.api.nvim_create_autocmd("BufWritePost", {
                pattern = "*.typ",
                callback = function()
                    local file = vim.fn.expand("%:p")
                    local pdf = vim.fn.expand("%:p:r") .. ".pdf"

                    vim.fn.jobstart({
                        "typst",
                        "compile",
                        "--synctex",
                        file,
                        pdf,
                    })
                end,
            })

            ------------------------------------------------
            -- Open PDF preview
            ------------------------------------------------
            vim.keymap.set("n", "<leader>tp", function()
                local pdf = vim.fn.expand("%:p:r") .. ".pdf"
                vim.fn.jobstart({ "open", pdf })
            end, { desc = "Open Typst PDF" })
        end,
    },
}
