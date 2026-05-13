return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  opts = {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
}

--return {
    --"folke/which-key.nvim",
    --opts = {},
    --config = function(_, opts)
        --require("which-key").setup(opts)

        --local wk = require("which-key")
        --wk.register({
            --f = { name = "Find", },
            --d = { name = "Debug", },
            --g = { name = "Git", },
            --n = { name = "Neogen", },
            --t = { name = "Testing", },
            --v = { name = "Lsp", },
            --m = { name = "Misc", },
        --}, {
            --prefix = "<leader>",
        --})

        --vim.keymap.set("n", "<C-h>", "<cmd>:WhichKey<CR>", { silent = true, desc = "Show which-key help" })
    --end,
--}
