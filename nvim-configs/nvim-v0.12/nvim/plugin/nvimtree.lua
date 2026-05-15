require("nvim-tree").setup()
--vim.keymap.set('n', '<leader>t', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.keymap.set("n", "<leader>t", function()
  require("nvim-tree.api").tree.toggle({
    path = vim.fn.expand("%:p:h"),
  })
end)
