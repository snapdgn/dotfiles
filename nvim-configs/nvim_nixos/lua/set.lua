vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.wrap = true
vim.opt.textwidth = 80
vim.opt.linebreak = true
--vim.opt.spell = true

--vim.g.loaded_netrw = 1
--vim.g.loaded_netrwPlugin = 1

-- optionally enable 24-bit colour
vim.opt.termguicolors = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

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

vim.opt.updatetime = 50
-- Set the runtime path in Neovim using Lua
vim.api.nvim_set_option('runtimepath',
    vim.api.nvim_get_option('runtimepath') .. ',/home/snapdgn/.opam/default/share/ocp-indent/vim')

--vim.opt.colorcolumn = "80"

-- autoformat with current LSP
--vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

