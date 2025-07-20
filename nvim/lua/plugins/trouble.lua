return {
    "folke/trouble.nvim",
    opts = {}, -- for default options, refer to the configuration section for custom setup.
    cmd = "Trouble",
    keys = {
        {
            "<leader>xx",
            "<cmd>Trouble diagnostics toggle<cr>",
            desc = "Diagnostics (Trouble)",
        },
        {
            "<leader>xX",
            "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
            desc = "Buffer Diagnostics (Trouble)",
        },
        {
            "<leader>cs",
            "<cmd>Trouble symbols toggle focus=false<cr>",
            desc = "Symbols (Trouble)",
        },
        {
            "<leader>cl",
            "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
            desc = "LSP Definitions / references / ... (Trouble)",
        },
        {
            "<leader>xL",
            "<cmd>Trouble loclist toggle<cr>",
            desc = "Location List (Trouble)",
        },
        {
            "<leader>xQ",
            "<cmd>Trouble qflist toggle<cr>",
            desc = "Quickfix List (Trouble)",
        },
    },
}
--return {
--"folke/trouble.nvim", version="*",
--dependencies = { "nvim-tree/nvim-web-devicons" },
--config = function()
--local trouble = require("trouble")
--local telescope = require("telescope")

---- Optional: Telescope integration for Trouble
--local trouble_telescope = require("trouble.sources.telescope")

--telescope.setup({
--defaults = {
--mappings = {
--i = { ["<c-t>"] = trouble_telescope.open },
--n = { ["<c-t>"] = trouble_telescope.open },
--},
--},
--})

---- Update these keymaps to use `open` with `mode` explicitly
--vim.keymap.set("n", "<leader>xx", function() trouble.toggle({ mode = "Diagnostics" }) end, { desc = "Trouble Toggle" })
--vim.keymap.set("n", "<leader>xw", function() trouble.toggle({ mode = "workspace_diagnostics" }) end, { desc = "Workspace Diagnostics" })
--vim.keymap.set("n", "<leader>xd", function() trouble.toggle({ mode = "document_diagnostics" }) end, { desc = "Document Diagnostics" })
--vim.keymap.set("n", "<leader>xq", function() trouble.toggle({ mode = "quickfix" }) end, { desc = "Quickfix List" })
--vim.keymap.set("n", "<leader>xl", function() trouble.toggle({ mode = "loclist" }) end, { desc = "Location List" })
--vim.keymap.set("n", "gR", function() trouble.toggle({ mode = "lsp_references" }) end, { desc = "LSP References" })
--end,
--}

--return {
--"folke/trouble.nvim",
--dependencies = { "nvim-tree/nvim-web-devicons" },
--config = function()
--local trouble = require("trouble.sources.telescope")
--local telescope = require("telescope")

--telescope.setup({
--defaults = {
--mappings = {
--i = { ["<c-t>"] = trouble.open },
--n = { ["<c-t>"] = trouble.open },
--},
--},
--})

--vim.keymap.set("n", "<leader>xx", function() require("trouble").toggle() end, { desc = "Trouble" })
--vim.keymap.set("n", "<leader>xw", function() require("trouble").toggle("workspace_diagnostics") end, { desc = "Trouble (Workspace Diagnostics)" })
--vim.keymap.set("n", "<leader>xd", function() require("trouble").toggle("document_diagnostics") end, { desc = "Trouble (Document Diagnostics)" })
--vim.keymap.set("n", "<leader>xq", function() require("trouble").toggle("quickfix") end, { desc = "Trouble (Quickfix)" })
--vim.keymap.set("n", "<leader>xl", function() require("trouble").toggle("loclist") end, { desc = "Trouble (Location List)" })
--vim.keymap.set("n", "gR", function() require("trouble").toggle("lsp_references") end, { desc = "Trouble (LSP References)" })
--end,
--}
