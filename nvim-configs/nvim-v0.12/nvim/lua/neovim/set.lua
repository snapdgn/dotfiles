vim.opt.guicursor = ""

vim.opt.nu = true
vim.opt.relativenumber = true
vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"

vim.opt.errorbells = false

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoread = true


vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.cmdheight = 1
vim.opt.updatetime = 50
vim.opt.shortmess:append("c")


vim.opt.list = true
vim.opt.listchars = {
    tab = "→ ",
    trail = "•",
    space = "·",
}

--vim.opt.colorcolumn = "80"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- autoformat with current LSP
--vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldlevel = 99

-- copy filepath
--vim.keymap.set("n", "<leader>cp", function()
    --local path = vim.fn.expand("%:p")
    --vim.fn.setreg("+", path)
    --print("Copied: " .. path)
--end, { desc = "Copy file path" })
