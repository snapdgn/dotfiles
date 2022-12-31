-- Gitsigns
local present, gitsigns = pcall(require, "gitsigns")
if present then
  gitsigns.setup {
    signs = {
      add = { hl = "DiffAdd", text = "│", numhl = "GitSignsAddNr" },
      change = { hl = "DiffChange", text = "│", numhl = "GitSignsChangeNr" },
      delete = { hl = "DiffDelete", text = "│", numhl = "GitSignsDeleteNr" },
      topdelete = { hl = "DiffDelete", text = "│", numhl = "GitSignsDeleteNr" },
      changedelete = { hl = "DiffChangeDelete", text = "│", numhl = "GitSignsChangeNr" },
    },
  }
end

-- Telescope
local present, telescope = pcall(require, "telescope")
if present then
  telescope.setup {
    defaults = {
      file_ignore_patterns = {
        "%.jpg",
        "%.jpeg",
        "%.png",
        "%.otf",
        "%.ttf",
        "node_modules",
        ".git",
      },
      prompt_prefix = "   ",
      layout_config = {
      horizontal = {
        prompt_position = "top",
        preview_width = 0.55,
        results_width = 0.8,
      },
      vertical = {
        mirror = false,
      },
      width = 0.87,
      height = 0.80,
      preview_cutoff = 120,
    },
      pickers = {
        find_files = {
          theme = "ivy",
        }
      },
      file_sorter = require("telescope.sorters").get_fuzzy_file,
      file_ignore_patterns = { "node_modules" },
      generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
      path_display = { "truncate" },
      color_devicons = true,
      highlight = 'Search',
      selection_caret = "  ",
      entry_prefix = "  ",
      layout_strategy = "flex",
      border = {},
      -- borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
      borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    },
      extensions_list = { "themes", "terms" },
  }
end

-- Nvim tree
local present, nvim_tree = pcall(require, "nvim-tree")
if present then
  nvim_tree.setup {
    view = {
      width = 30,
      side = "left",
      hide_root_folder = true,
    },
    disable_netrw = true,
    hijack_cursor = true,
    update_cwd = true,
--    update_to_buf_dir = {
--      auto_open = false,
--    },
  }
  vim.g.nvim_tree_indent_markers = 1
end

-- Lua Snip
local present, luasnip = pcall(require, 'luasnip')
if present then
  local present, friendly_snippets = pcall(require, 'luasnip.loaders.from_vscode')
  if present then 
    friendly_snippets.load()
  end
end

-- Rust analyzer setup
local nvim_lsp = require'lspconfig'

local opts = {
    tools = { -- rust-tools options
        autoSetHints = true,
        RustHoverActions = true,
        --hover_with_actions = true,
        inlay_hints = {
            show_parameter_hints = false,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ["rust-analyzer"] = {
                -- enable clippy on save
                checkOnSave = {
                    command = "clippy"
                },
            }
        }
    },
}

require('rust-tools').setup(opts)
require('rust-tools').inlay_hints.enable()
require('rust-tools').runnables.runnables()
-- Command:
-- RustHoverActions 
require'rust-tools'.hover_actions.hover_actions()

-- Cmp Completion
local present, cmp = pcall(require, 'cmp')
if present then
  cmp.setup({
    -- Enable LSP snippets
    snippet = {
      expand = function(args)
          vim.fn["vsnip#anonymous"](args.body)
      end,
    },
    mapping = {
      ['<C-p>'] = cmp.mapping.select_prev_item(),
      ['<C-n>'] = cmp.mapping.select_next_item(),
      ['<C-d>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.close(),
      ['<CR>'] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      },
      ['<Tab>'] = function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        else
          fallback()
        end
      end,
      ['<S-Tab>'] = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        else
          fallback()
        end
      end,
    },
    sources = cmp.config.sources({
      { name = 'luasnip' },
      { name = 'nvim_lsp', keyword_length = 3 },
      { name = 'nvim_lua', keyword_length = 2 },
      { name = 'vsnip' },
      { name = 'path' },
      { name = 'buffer' },
    })
  })
end

--autoformat with current active LSP
vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]
-- rystfmt on save

----------------------------LSP UI START-----------------------------

-- /BORDER CUSTOMIZATION START / --
vim.cmd [[autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]]
vim.cmd [[autocmd! ColorScheme * highlight FloatBorder guifg=#white guibg=#1f2335]]
vim.cmd [[autocmd! ColorScheme * highlight TelescopeSelection guifg=#D79921 gui=bold]]

--local border_vertical   = "║"
--local border_horizontal = "═"
--local border_topleft    = "╔"
--local border_topright   = "╗"
--local border_botleft    = "╚"
--local border_botright   = "╝"
--local border_juncleft   = "╠"
--local border_juncright  = "╣"


local border = {
      {"🭽", "FloatBorder"},
      {"▔", "FloatBorder"},
      {"🭾", "FloatBorder"},
      {"▕", "FloatBorder"},
      {"🭿", "FloatBorder"},
      {"▁", "FloatBorder"},
      {"🭼", "FloatBorder"},
      {"▏", "FloatBorder"},
}

-- LSP settings (for overriding per client)
local handlers =  {
  ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, {border = border}),
  ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, {border = border }),
}

-- Do not forget to use the on_attach function
--require 'lspconfig'.myserver.setup { handlers=handlers }

-- To instead override globally
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or border
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

--require 'lspconfig'.myservertwo.setup {}

-- /BORDER CUSTOMIZATION END / --

-- change diagnostic symbols in the sign column(gutter)
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end


vim.diagnostic.config({
  virtual_text = false,
  signs = true,
  underline = false,
  update_in_insert = false,
  severity_sort = false,
})

-- Show line diagnostics automatically in hover window
vim.o.updatetime = 250
vim.cmd [[autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]


-- go to definations in split window
local function goto_definition(split_cmd)
  local util = vim.lsp.util
  local log = require("vim.lsp.log")
  local api = vim.api

  -- note, this handler style is for neovim 0.5.1/0.6, if on 0.5, call with function(_, method, result)
  local handler = function(_, result, ctx)
    if result == nil or vim.tbl_isempty(result) then
      local _ = log.info() and log.info(ctx.method, "No location found")
      return nil
    end

    if split_cmd then
      vim.cmd(split_cmd)
    end

    if vim.tbl_islist(result) then
      util.jump_to_location(result[1])

      if #result > 1 then
        util.set_qflist(util.locations_to_items(result))
        api.nvim_command("copen")
        api.nvim_command("wincmd p")
      end
    else
      util.jump_to_location(result)
    end
  end

  return handler
end

-- #TODO: doesn't work
-- goto_definition in a vsplit window
--vim.api.nvim_set_keymap("n", "gd", ":only<bar>vsplit<CR>gd", { noremap = true, })

vim.lsp.handlers["textDocument/definition"] = goto_definition('split')

-- uncomment to display inline errors
--
--vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  --virtual_text = {
    --prefix = '●', -- Could be '●', '▎', 'x'
  --}
--})


----------------------------LSP UI END------------------------------------
--
-- Language Server Support
require'lspconfig'.rust_analyzer.setup{}
require'lspconfig'.clangd.setup{}
require'lspconfig'.pyright.setup{}
require'lspconfig'.gopls.setup{}
require'lspconfig'.tsserver.setup{}
require'lspconfig'.ruby_ls.setup{}

-- statusline setup
--require('lualine').setup() {
    --options = { theme = 'gruvbox' }
--}

--OrgMode Setup
--
-- init.lua

-- Load custom tree-sitter grammar for org filetype
require('orgmode').setup_ts_grammar()

-- Tree-sitter configuration
require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = {'org'}, -- Required for spellcheck, some LaTex highlights and code block highlights that do not have ts grammar
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
  org_default_notes_file = '~/Dropbox/org/refile.org',
})

-- Quickly insert empty lines
vim.api.nvim_set_keymap("n", "[<space>", ":<c-u>put! =repeat(nr2char(10), v:count1)<cr>'[", { noremap = true, })
vim.api.nvim_set_keymap("n", "]<space>", ":<c-u>put =repeat(nr2char(10), v:count1)<cr>", { noremap = true, })


