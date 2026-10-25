vim.keymap.set("n", "<leader>gw", function() require("neovim.worktree").pick() end, { desc = "Git Worktrees" })
vim.keymap.set("n", "<leader>gW", function() require("neovim.worktree").create() end, { desc = "Git Worktree Create" })

vim.api.nvim_create_autocmd("DirChanged", {
    group = vim.api.nvim_create_augroup("Group_Worktree", {}),
    callback = function() require("neovim.worktree").refresh_status() end,
})
