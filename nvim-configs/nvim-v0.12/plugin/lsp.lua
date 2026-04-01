-- Mason for LSP server management
require("mason").setup()
require("mason-lspconfig").setup({
    ensure_installed = { "lua_ls", "gopls", "rust_analyzer" },
})

-- Diagnostic signs
local signs = {
    Error = "✖",
    Warn = "⚠",
    Hint = "»",
    Info = "►"
}

for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

vim.diagnostic.config({
    virtual_text = false,
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = signs.Error,
            [vim.diagnostic.severity.WARN] = signs.Warn,
            [vim.diagnostic.severity.HINT] = signs.Hint,
            [vim.diagnostic.severity.INFO] = signs.Info,
        }
    },
    update_in_insert = false,
    underline = true,
    severity_sort = true,
    float = {
        border = 'rounded',
        source = 'always',
        header = '',
        prefix = '',
    },
})

-- LSP keybindings on attach
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local bufnr = event.buf
        local bmap = function(mode, lhs, rhs, desc)
            vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
        end

        bmap("n", "K", vim.lsp.buf.hover, "LSP Hover")
        bmap("n", "<leader>vws", vim.lsp.buf.workspace_symbol, "LSP Workspace Symbol")
        bmap("n", "<leader>vd", vim.diagnostic.open_float, "LSP Diagnostic")
        bmap("n", "[d", vim.diagnostic.goto_prev, "LSP Diagnostic Previous")
        bmap("n", "]d", vim.diagnostic.goto_next, "LSP Diagnostic Next")
        bmap("n", "<leader>vca", vim.lsp.buf.code_action, "LSP Code Action")
        bmap("n", "<leader>vcr", vim.lsp.codelens.refresh, "LSP Code Lens Refresh")
        bmap("n", "<leader>vcc", vim.lsp.codelens.run, "LSP Code Lens Run")
        bmap("n", "<leader>vrr", vim.lsp.buf.references, "LSP References")
        bmap("n", "<leader>vrn", vim.lsp.buf.rename, "LSP Rename")
        bmap("i", "<C-h>", vim.lsp.buf.signature_help, "LSP Signature Help")
        bmap("n", "<leader>vmt", function()
            vim.lsp.buf.format({ async = true })
        end, "LSP Format")
    end,
})

-- Enable language servers (configs live in lsp/ directory)
vim.lsp.enable('lua_ls')
vim.lsp.enable('gopls')
vim.lsp.enable('rust_analyzer')

-- Fidget for LSP progress
require("fidget").setup()

-- Tiny inline diagnostics
require('tiny-inline-diagnostic').setup({
    signs = {
        left = "",
        right = "",
        diag = "●",
        arrow = "    ",
        up_arrow = "    ",
        vertical = " │",
        vertical_end = " └"
    },
    options = {
        show_source = false,
        throttle = 20,
        multiple_diag_under_cursor = false,
    }
})
