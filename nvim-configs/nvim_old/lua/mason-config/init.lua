require("mason").setup()
require("mason-lspconfig").setup()


-- Language Server Support
require 'lspconfig'.rust_analyzer.setup {}
require 'lspconfig'.clangd.setup {}
--require'lspconfig'.black.setup{}
require 'lspconfig'.gopls.setup {}
require 'lspconfig'.tsserver.setup {}
require 'lspconfig'.ruby_ls.setup {}
require 'lspconfig'.asm_lsp.setup {}
require 'lspconfig'.volar.setup {
  filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'json' }
}
require 'lspconfig'.zls.setup {}
require 'lspconfig'.lua_ls.setup {}
require 'lspconfig'.spectral.setup {}
require 'lspconfig'.pylsp.setup {}
require 'lspconfig'.hls.setup {}

local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.stylua,
    --null_ls.builtins.diagnostics.eslint,
    null_ls.builtins.formatting.eslint_d,
    null_ls.builtins.completion.spell,
    -- below: haskell formatter, duplicate: fourmolu
    --null_ls.builtins.formatting.brittany,
    null_ls.builtins.diagnostics.mypy,
    null_ls.builtins.code_actions.refactoring,
    null_ls.builtins.completion.luasnip,
    null_ls.builtins.completion.tags,
    null_ls.builtins.formatting.fourmolu,
  },
})
