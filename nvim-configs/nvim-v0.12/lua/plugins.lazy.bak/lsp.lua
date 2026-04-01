return {
    -- Mason for managing LSP server installations
    {
        "williamboman/mason.nvim",
        config = function()
            require("mason").setup({})
        end
    },
    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = { "williamboman/mason.nvim" },
        config = function()
            require("mason-lspconfig").setup({
                ensure_installed = {
                    "lua_ls",
                    "gopls",
                },
            })
        end
    },
    -- nvim-lspconfig for LSP configurations (provides defaults in lsp/ directory)
    {
        "neovim/nvim-lspconfig",
        dependencies = { "williamboman/mason-lspconfig.nvim" },
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            -- Configure diagnostic signs (these will show in the gutter/sign column)
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
            
            -- Configure diagnostic display
            -- Note: virtual_text might be disabled by tiny-inline-diagnostic plugin
            vim.diagnostic.config({
                virtual_text = false, -- Will be handled by tiny-inline-diagnostic
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
            
            -- Set up LspAttach autocommand for keybindings
            vim.api.nvim_create_autocmd('LspAttach', {
                desc = 'LSP actions',
                callback = function(event)
                    local bufnr = event.buf
                    
                    vim.keymap.set("n", "gd", vim.lsp.buf.definition,
                        { buffer = bufnr, desc = "LSP Go to Definition" })
                    vim.keymap.set("n", "gi", vim.lsp.buf.implementation,
                        { buffer = bufnr, desc = "LSP Go to Implementation" })
                    vim.keymap.set("n", "gI", vim.lsp.buf.type_definition,
                        { buffer = bufnr, desc = "LSP Go to Type Definition" })
                    vim.keymap.set("n", "gr", vim.lsp.buf.references,
                        { buffer = bufnr, desc = "LSP Go to References" })
                    vim.keymap.set("n", "K", vim.lsp.buf.hover,
                        { buffer = bufnr, desc = "LSP Hover" })
                    vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol,
                        { buffer = bufnr, desc = "LSP Workspace Symbol" })
                    vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float,
                        { buffer = bufnr, desc = "LSP Diagnostic" })
                    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev,
                        { buffer = bufnr, desc = "LSP Diagnostic Previous" })
                    vim.keymap.set("n", "]d", vim.diagnostic.goto_next,
                        { buffer = bufnr, desc = "LSP Diagnostic Next" })
                    vim.keymap.set("n", "<leader>vca", vim.lsp.buf.code_action,
                        { buffer = bufnr, desc = "LSP Code Action" })
                    vim.keymap.set("n", "<leader>vcr", vim.lsp.codelens.refresh,
                        { buffer = bufnr, desc = "LSP Code Lens Refresh" })
                    vim.keymap.set("n", "<leader>vcc", vim.lsp.codelens.run,
                        { buffer = bufnr, desc = "LSP Code Lens Run" })
                    vim.keymap.set("n", "<leader>vrr", vim.lsp.buf.references,
                        { buffer = bufnr, desc = "LSP References" })
                    vim.keymap.set("n", "<leader>vrn", vim.lsp.buf.rename,
                        { buffer = bufnr, desc = "LSP Rename" })
                    vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help,
                        { buffer = bufnr, desc = "LSP Signature Help" })
                    vim.keymap.set("n", "<leader>vmt", function()
                        vim.lsp.buf.format({ async = true })
                    end, { buffer = bufnr, desc = "LSP Format" })
                end,
            })

            -- Enable language servers using nvim-lspconfig's defaults
            vim.lsp.enable('lua_ls')
            vim.lsp.enable('gopls')
            vim.lsp.enable('rust_analyzer')
        end
    },
    -- Autocompletion
    {
        "hrsh7th/nvim-cmp",
        event = "InsertEnter",
        dependencies = {
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-path" },
            { "hrsh7th/cmp-nvim-lsp" },
            { "hrsh7th/cmp-nvim-lua" },
            { "hrsh7th/cmp-cmdline" },
            { "saadparwaiz1/cmp_luasnip" },
            { "L3MON4D3/LuaSnip" },
            { "rafamadriz/friendly-snippets" },
        },
        config = function()
            local cmp = require("cmp")
            local cmp_select = { behavior = cmp.SelectBehavior.Select }
            
            -- Load snippets from friendly-snippets
            require('luasnip.loaders.from_vscode').lazy_load()

            cmp.setup({
                snippet = {
                    expand = function(args)
                        require('luasnip').lsp_expand(args.body)
                    end,
                },
                sources = {
                    { name = 'path' },
                    { name = 'nvim_lsp' },
                    { name = 'nvim_lua' },
                    { name = 'luasnip', keyword_length = 2 },
                    { name = 'buffer', keyword_length = 3 },
                },
                mapping = cmp.mapping.preset.insert({
                    ['<CR>'] = cmp.mapping.confirm({ select = true }),
                    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-d>"] = cmp.mapping.scroll_docs(4),
                    ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
                    ["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                    ["<C-Space>"] = cmp.mapping.complete(),
                }),
                formatting = {
                    fields = { 'abbr', 'kind', 'menu' },
                    format = function(entry, item)
                        local menu_icon = {
                            nvim_lsp = '[LSP]',
                            luasnip = '[Snippet]',
                            buffer = '[Buffer]',
                            path = '[Path]',
                            nvim_lua = '[Lua]',
                        }
                        item.menu = menu_icon[entry.source.name]
                        return item
                    end,
                },
            })
        end
    },
}
