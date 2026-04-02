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

-- Kind icons and highlight groups for completion popup
local kind_icons = {
    Text = '󰉿', Method = '󰆧', Function = '󰊕', Constructor = '',
    Field = '󰜢', Variable = '󰀫', Class = '󰠱', Interface = '',
    Module = '', Property = '󰜢', Unit = '󰑭', Value = '󰎠',
    Enum = '', Keyword = '󰌋', Snippet = '', Color = '󰏘',
    File = '󰈙', Reference = '󰈇', Folder = '󰉋', EnumMember = '',
    Constant = '󰏿', Struct = '󰙅', Event = '', Operator = '󰆕',
    TypeParameter = '',
}

local kind_hl = {
    Text = 'String', Method = 'Function', Function = 'Function', Constructor = 'Special',
    Field = '@property', Variable = '@variable', Class = 'Type', Interface = 'Type',
    Module = '@module', Property = '@property', Unit = 'Number', Value = 'Number',
    Enum = 'Type', Keyword = 'Keyword', Snippet = 'Special', Color = 'Special',
    File = 'Directory', Reference = 'Identifier', Folder = 'Directory',
    EnumMember = 'Constant', Constant = 'Constant', Struct = 'Structure',
    Event = 'Special', Operator = 'Operator', TypeParameter = 'Type',
}

-- LSP keybindings on attach
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local bufnr = event.buf
        local client = vim.lsp.get_client_by_id(event.data.client_id)
        if not client then return end

        -- Extend triggerCharacters to include all word chars so completion
        -- fires on every keystroke, not just on '.' or ':' (like nvim-cmp did)
        local caps = client.server_capabilities.completionProvider
        if caps then
            local chars = caps.triggerCharacters or {}
            for c = string.byte('a'), string.byte('z') do chars[#chars + 1] = string.char(c) end
            for c = string.byte('A'), string.byte('Z') do chars[#chars + 1] = string.char(c) end
            for c = string.byte('0'), string.byte('9') do chars[#chars + 1] = string.char(c) end
            chars[#chars + 1] = '_'
            caps.triggerCharacters = chars
        end

        vim.lsp.completion.enable(true, event.data.client_id, bufnr, {
            autotrigger = true,
            convert = function(item)
                local kind_name = vim.lsp.protocol.CompletionItemKind[item.kind] or 'Unknown'
                local icon = kind_icons[kind_name] or ''
                return {
                    kind = icon .. ' ' .. kind_name,
                    kind_hlgroup = kind_hl[kind_name] or 'PmenuKind',
                }
            end,
        })

        local bmap = function(mode, lhs, rhs, desc)
            vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
        end

        bmap("n", "gd", vim.lsp.buf.definition, "LSP Go to Definition")
        bmap("n", "gi", vim.lsp.buf.implementation, "LSP Go to Implementation")
        bmap("n", "gI", vim.lsp.buf.type_definition, "LSP Go to Type Definition")
        bmap("n", "gr", vim.lsp.buf.references, "LSP Go to References")
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
