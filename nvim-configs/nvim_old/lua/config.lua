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
local nvim_lsp = require 'lspconfig'

local opts = {
  tools = {
    -- rust-tools options
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
require 'rust-tools'.hover_actions.hover_actions()

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

--autoformat with current active LSP (old)
--vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]

--vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()]]

--  new way for autoformatting
-- START COPYPASTA https://github.com/neovim/neovim/commit/5b04e46d23b65413d934d812d61d8720b815eb1c
local util = require 'vim.lsp.util'
--- Formats a buffer using the attached (and optionally filtered) language
--- server clients.
---
--- @param options table|nil Optional table which holds the following optional fields:
---     - formatting_options (table|nil):
---         Can be used to specify FormattingOptions. Some unspecified options will be
---         automatically derived from the current Neovim options.
---         @see https://microsoft.github.io/language-server-protocol/specification#textDocument_formatting
---     - timeout_ms (integer|nil, default 1000):
---         Time in milliseconds to block for formatting requests. Formatting requests are current
---         synchronous to prevent editing of the buffer.
---     - bufnr (number|nil):
---         Restrict formatting to the clients attached to the given buffer, defaults to the current
---         buffer (0).
---     - filter (function|nil):
---         Predicate to filter clients used for formatting. Receives the list of clients attached
---         to bufnr as the argument and must return the list of clients on which to request
---         formatting. Example:
---
---         <pre>
---         -- Never request typescript-language-server for formatting
---         vim.lsp.buf.format {
---           filter = function(clients)
---             return vim.tbl_filter(
---               function(client) return client.name ~= "tsserver" end,
---               clients
---             )
---           end
---         }
---         </pre>
---
---     - id (number|nil):
---         Restrict formatting to the client with ID (client.id) matching this field.
---     - name (string|nil):
---         Restrict formatting to the client with name (client.name) matching this field.
vim.lsp.buf.format = function(options)
  options = options or {}
  local bufnr = options.bufnr or vim.api.nvim_get_current_buf()
  local clients = vim.lsp.buf_get_clients(bufnr)

  if options.filter then
    clients = options.filter(clients)
  elseif options.id then
    clients = vim.tbl_filter(
      function(client) return client.id == options.id end,
      clients
    )
  elseif options.name then
    clients = vim.tbl_filter(
      function(client) return client.name == options.name end,
      clients
    )
  end

  clients = vim.tbl_filter(
    function(client) return client.supports_method 'textDocument/formatting' end,
    clients
  )

  if #clients == 0 then
    vim.notify '[LSP] Format request failed, no matching language servers.'
  end

  local timeout_ms = options.timeout_ms or 1000
  for _, client in pairs(clients) do
    local params = util.make_formatting_params(options.formatting_options)
    local result, err = client.request_sync('textDocument/formatting', params, timeout_ms, bufnr)
    if result and result.result then
      util.apply_text_edits(result.result, bufnr, client.offset_encoding)
    elseif err then
      vim.notify(string.format('[LSP][%s] %s', client.name, err), vim.log.levels.WARN)
    end
  end
end
-- END COPYPASTA


vim.api.nvim_create_augroup('LspFormatting', { clear = true })
vim.api.nvim_create_autocmd('BufWritePre', {
  pattern = '*',
  group = 'LspFormatting',
  callback = function()
    vim.lsp.buf.format {
      timeout_ms = 2000,
      filter = function(clients)
        return vim.tbl_filter(function(client)
          return pcall(function(_client)
            return _client.config.settings.autoFixOnSave or false
          end, client) or false
        end, clients)
      end
    }
  end
})

-- rystfmt on save
--
-- start nerdtree automatically
-- Start NERDTree and put the cursor back in the other window.

--autocmd VimEnter * NERDTree | wincmd p
--vim.cmd [[ autocmd! VimEnter * NERDTree]]
--
--NVIMTREE configs
require("nvim-tree").setup { -- BEGIN_DEFAULT_OPTS
  on_attach = "enable",
}                            -- END_DEFAULT_OPTS


-- NVIMTREE setup ends
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
  { "🭽", "FloatBorder" },
  { "▔",  "FloatBorder" },
  { "🭾", "FloatBorder" },
  { "▕",  "FloatBorder" },
  { "🭿", "FloatBorder" },
  { "▁",  "FloatBorder" },
  { "🭼", "FloatBorder" },
  { "▏",  "FloatBorder" },
}

-- LSP settings (for overriding per client)
local handlers = {
  ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
  ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
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
require("toggleterm").setup {
  -- size can be a number or function which is passed the current terminal
  open_mapping = [[<c-\>]],
  hide_numbers = true,     -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  autochdir = false,       -- when neovim changes it current directory the terminal will change it's own when next it's opened
  shade_terminals = false, -- NOTE: this option takes priority over highlights specified so if you specify Normal highlights you should set this to false
  --shading_factor = '<number>', -- the percentage by which to lighten terminal background, default: -30 (gets multiplied by -3 if background is light)
  start_in_insert = true,
  insert_mappings = true,   -- whether or not the open mapping applies in insert mode
  terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
  persist_size = true,
  persist_mode = true,      -- if set to true (default) the previous terminal mode will be remembered
  direction = 'float',
  close_on_exit = true,     -- close the terminal window when the process exits
  shell = vim.o.shell,      -- change the default shell
  auto_scroll = false,      -- automatically scroll to the bottom on terminal output
  -- This field is only relevant if direction is set to 'float'
  winbar = {
    enabled = false,
    name_formatter = function(term) --  term: Terminal
      return term.name
    end
  }
}





require("nvim-tree").setup()

--require("null-ls").setup()
require("bufferline").setup {}

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
require 'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = { 'org' }, -- Required for spellcheck, some LaTex highlights and code block highlights that do not have ts grammar
    indent = true,
  },
}

require('orgmode').setup({
  org_agenda_files = { '~/Dropbox/org/*', '~/my-orgs/**/*' },
  org_default_notes_file = '~/Dropbox/org/refile.org',
})

-- Quickly insert empty lines
vim.api.nvim_set_keymap("n", "[<space>", ":<c-u>put! =repeat(nr2char(10), v:count1)<cr>'[", { noremap = true, })
vim.api.nvim_set_keymap("n", "]<space>", ":<c-u>put =repeat(nr2char(10), v:count1)<cr>", { noremap = true, })
