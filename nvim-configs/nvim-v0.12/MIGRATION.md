# lazy.nvim to vim.pack Migration (April 2026)

## Overview

Migrated from **lazy.nvim** to **vim.pack** (nvim 0.12 built-in plugin manager).
Also adopted nvim 0.12 built-in autocompletion (`vim.o.autocomplete`), replacing nvim-cmp.

## New structure

```
~/.config/nvim/
├── init.lua                    # vim.loader.enable() + vim.pack.add() for all plugins
├── plugin/                     # Plugin configurations (auto-sourced after init.lua)
│   ├── snacks.lua              # Snacks setup + all picker/toggle keymaps
│   ├── lsp.lua                 # Mason + diagnostics + LspAttach keymaps + fidget + tiny-inline-diagnostic
│   ├── completion.lua          # Built-in nvim 0.12 autocompletion settings
│   ├── treesitter.lua          # Treesitter + context + custom parsers
│   ├── gitsigns.lua            # Git signs + hunk navigation
│   ├── fugitive.lua            # Git push/pull + merge conflict helpers
│   ├── neogit.lua              # Neogit setup
│   ├── harpoon.lua             # Quick file navigation
│   ├── nvimtree.lua            # File tree
│   ├── trouble.lua             # Diagnostics list
│   ├── multicursor.lua         # Multiple cursors
│   ├── editing.lua             # Autopairs + surround
│   ├── ui.lua                  # Lualine + bufferline + which-key + undotree + zen + todo-comments
│   ├── octo.lua                # GitHub integration (snacks picker)
│   └── typst.lua               # Typst compile-on-save + PDF preview
├── lsp/                        # Native nvim 0.12 LSP configs (unchanged)
│   ├── lua_ls.lua
│   └── rust_analyzer.lua
├── lua/
│   ├── neovim/                 # Core settings (unchanged)
│   │   ├── set.lua
│   │   ├── remap.lua
│   │   ├── autocmd.lua
│   │   ├── globals.lua
│   │   └── util.lua
│   └── plugins.lazy.bak/       # Archived lazy.nvim specs (safe to delete)
└── syntax/
    └── cools.vim               # Custom COOL language syntax
```

## What changed

### Plugin manager: lazy.nvim -> vim.pack
- No bootstrap code needed (vim.pack is built into nvim 0.12)
- Single `vim.pack.add({...})` call in init.lua installs all plugins
- Plugin configs live in `plugin/*.lua` (auto-sourced by nvim after init.lua)
- Lockfile: `nvim-pack-lock.json` (track in git)
- TSUpdate handled via `PackChanged` autocmd

### Completion: nvim-cmp -> built-in autocompletion
- `vim.o.autocomplete = true` enables native auto-triggered completion
- Sources: current buffer, windows, buffers, unloaded buffers, tags, spelling, LSP omnifunc
- Removed 9 plugins: nvim-cmp, cmp-buffer, cmp-path, cmp-nvim-lsp, cmp-nvim-lua, cmp-cmdline, cmp_luasnip, LuaSnip, friendly-snippets

### Startup performance
- `vim.loader.enable()` adds bytecode caching (~30% faster startup)

### Plugins dropped
- **telescope.nvim** - replaced by snacks.nvim pickers
- **nvim-treesitter/playground** - built into nvim 0.12 as `:InspectTree`
- **nvim-cmp + ecosystem** - replaced by built-in autocompletion

### Plugins updated
- **octo.nvim** - switched picker from telescope to snacks
- **todo-comments** - `TodoTelescope` keymaps changed to `TodoTrouble`

## Managing plugins

```lua
-- Update all plugins
:lua vim.pack.update()

-- Update specific plugin
:lua vim.pack.update({ 'snacks.nvim' })

-- Revert to lockfile state
:lua vim.pack.update(nil, { target = 'lockfile' })

-- Delete a plugin (first remove from init.lua, then):
:lua vim.pack.del({ 'plugin-name' })

-- Health check
:checkhealth vim.pack
```

## First launch

On first launch after migration, vim.pack shows an install confirmation for all plugins.
Press `a` to accept all. Subsequent launches are instant.

## Cleanup

After confirming everything works:
1. Delete `lua/plugins.lazy.bak/` (archived lazy.nvim specs)
2. Delete `lazy-lock.json` (lazy.nvim lockfile)
3. Optionally delete `~/.local/share/nvim/lazy/` (lazy.nvim plugin cache)
