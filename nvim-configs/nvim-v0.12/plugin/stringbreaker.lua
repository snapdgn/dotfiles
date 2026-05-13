local sb = require("string-breaker")
sb.setup()

vim.keymap.set({ "n", "v" }, "<leader>fe", "<cmd>StringEscape<cr>", { desc = "String: escape" })
vim.keymap.set({ "n", "v" }, "<leader>fu", "<cmd>StringUnescape<cr>", { desc = "String: unescape" })
