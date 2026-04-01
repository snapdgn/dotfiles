local harpoon = require("harpoon")

harpoon:setup({
    menu = {
        width = vim.api.nvim_win_get_width(0) - 4,
    },
})

vim.keymap.set("n", "<leader>hx", function() harpoon:list():append() end, { desc = "Harpoon file" })
vim.keymap.set("n", "<leader>hh", function()
    harpoon.ui:toggle_quick_menu(harpoon:list())
end, { desc = "Harpoon quick menu" })
vim.keymap.set("n", "<leader>1", function() harpoon:list():select(1) end, { desc = "Harpoon to file 1" })
vim.keymap.set("n", "<leader>2", function() harpoon:list():select(2) end, { desc = "Harpoon to file 2" })
vim.keymap.set("n", "<leader>3", function() harpoon:list():select(3) end, { desc = "Harpoon to file 3" })
vim.keymap.set("n", "<leader>4", function() harpoon:list():select(4) end, { desc = "Harpoon to file 4" })
vim.keymap.set("n", "<leader>5", function() harpoon:list():select(5) end, { desc = "Harpoon to file 5" })
