return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons", -- For icons
  },
  config = function()
    require("nvim-tree").setup {
      -- You can add more options here if you need
    }

    -- Keybinding for toggling NvimTree
    vim.api.nvim_set_keymap('n', '<leader>t', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
  end,
}
