return {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.6",
    dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-telescope/telescope-live-grep-args.nvim",
    },
    -- Use 'keys' to lazy-load Telescope when these keymaps are pressed
    keys = {
        { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find files" },
        { "<leader>fg", "<cmd>Telescope live_grep_args<cr>", desc = "Live grep (args)" },
        { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
        { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Help tags" },
    },
    config = function()
        -- You don't need to 'require' builtin here if you're using <cmd>Telescope ...
        -- However, you still need to set up Telescope and load its extension.
        -- This part will run when one of the 'keys' is pressed for the first time.
        local telescope = require("telescope")
        telescope.setup({})
        telescope.load_extension("live_grep_args")
    end,
}
--return {
    --"nvim-telescope/telescope.nvim",
    --tag = "0.1.8",
    --dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope-live-grep-args.nvim",
    --},
    --config = function()
        --local builtin = require("telescope.builtin")
        --local telescope = require("telescope")

        --vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Find files" })
        ----vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Live grep" })
        --vim.keymap.set("n", "<leader>fg", ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>")
        --vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Buffers" })
        --vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Help tags" })

        --telescope.setup({})
        --telescope.load_extension("live_grep_args")
    --end,
--}
