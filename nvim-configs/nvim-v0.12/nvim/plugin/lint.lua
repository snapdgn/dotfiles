local lint = require("lint")

lint.linters_by_ft = {
    go       = { "golangcilint" },
    rust     = { "cargo" },
    zig      = { "zig" },
    c        = { "cppcheck" },
    cpp      = { "cppcheck" },
}

lint.linters.golangcilint.ignore_exitcode = true

vim.keymap.set("n", "<leader>ll", lint.try_lint, { desc = "Lint current buffer" })
