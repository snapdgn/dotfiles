local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
    -- see :help lsp-zero-keybindings
    -- to learn the available actions
    lsp.default_keymaps({ buffer = bufnr })
end)

-- When you don't have mason.nvim installed
-- You'll need to list the servers installed in your system
lsp.setup_servers({ 'tsserver', 'eslint', 'lua_ls', 'rust_analyzer', 'clangd', 'pyright', 'r_language_server', 'bashls' })

-- (Optional) Configure lua language server for neovim
require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
require 'lspconfig'.pyright.setup {}
require 'lspconfig'.clangd.setup {}
require 'lspconfig'.bashls.setup {}
require 'lspconfig'.r_language_server.setup {}

vim.diagnostic.config({
    virtual_text = false
})

-- Show line diagnostics automatically in hover window
vim.o.updatetime = 250
vim.cmd [[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]
