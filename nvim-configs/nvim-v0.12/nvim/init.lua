vim.loader.enable()

require("neovim.set")
require("neovim.remap")
require("neovim.globals")
require("neovim.autocmd")
require("neovim.configs")

-- Post-install/update hooks (must be before any vim.pack.add call)
vim.api.nvim_create_autocmd('PackChanged', {
    callback = function(ev)
        local name, kind = ev.data.spec.name, ev.data.kind
        if name == 'nvim-treesitter' and (kind == 'update' or kind == 'install') then
            if not ev.data.active then vim.cmd.packadd('nvim-treesitter') end
            vim.cmd('TSUpdate')
        end
    end
})

-- All plugins (dependencies listed before dependents)
vim.pack.add({
    -- Shared dependencies
    'https://github.com/nvim-lua/plenary.nvim',
    'https://github.com/nvim-tree/nvim-web-devicons',

    -- Colorschemes
    'https://github.com/ellisonleao/gruvbox.nvim',
    'https://github.com/ray-x/starry.nvim',
    'https://github.com/folke/tokyonight.nvim',
    'https://github.com/EdenEast/nightfox.nvim',
    { src = 'https://github.com/catppuccin/nvim',  name = 'catppuccin' },
    { src = 'https://github.com/rose-pine/neovim', name = 'rose-pine' },
    'https://github.com/rebelot/kanagawa.nvim',
    'https://github.com/sainnhe/everforest',
    'https://github.com/sonph/onehalf',
    'https://github.com/craftzdog/solarized-osaka.nvim',
    'https://github.com/navarasu/onedark.nvim',
    'https://github.com/sainnhe/gruvbox-material',
    'https://github.com/savq/melange-nvim',
    'https://github.com/rmehri01/onenord.nvim',
    'https://github.com/shaunsingh/nord.nvim',

    -- Core UI
    'https://github.com/folke/snacks.nvim',
    'https://github.com/nvim-lualine/lualine.nvim',
    'https://github.com/akinsho/bufferline.nvim',

    -- LSP
    'https://github.com/williamboman/mason.nvim',
    'https://github.com/williamboman/mason-lspconfig.nvim',
    'https://github.com/neovim/nvim-lspconfig',
    'https://github.com/j-hui/fidget.nvim',
    'https://github.com/rachartier/tiny-inline-diagnostic.nvim',

    -- Treesitter
    'https://github.com/nvim-treesitter/nvim-treesitter',
    'https://github.com/nvim-treesitter/nvim-treesitter-context',

    -- Git
    'https://github.com/lewis6991/gitsigns.nvim',
    'https://github.com/tpope/vim-fugitive',
    'https://github.com/sindrets/diffview.nvim',
    'https://github.com/ThePrimeagen/git-worktree.nvim',

    -- Navigation
    { src = 'https://github.com/ThePrimeagen/harpoon',    version = 'harpoon2' },
    'https://github.com/nvim-tree/nvim-tree.lua',
    'https://github.com/folke/trouble.nvim',

    -- Editing
    'https://github.com/windwp/nvim-autopairs',
    --'https://github.com/kylechui/nvim-surround',
    { src = 'https://github.com/nvim-mini/mini.surround', version = 'stable' },
    'https://github.com/preservim/nerdcommenter',
    --{ src = 'https://github.com/jake-stewart/multicursor.nvim', version = '1.0' },
    'https://github.com/duqcyxwd/stringbreaker.nvim',

    -- UI / Tools
    'https://github.com/folke/which-key.nvim',
    'https://github.com/mbbill/undotree',
    'https://github.com/folke/todo-comments.nvim',
    'https://github.com/pwntester/octo.nvim',

    -- Lint
    'https://github.com/mfussenegger/nvim-lint',

    -- Typst
    --'https://github.com/kaarmu/typst.vim',
})

-- Colorscheme (immediately after plugins loaded) — IST day/night auto-switch
local IST_OFFSET = 5.5 * 60 * 60
local ist_hour = tonumber(os.date("!%H", os.time() + IST_OFFSET))
if ist_hour >= 7 and ist_hour < 19 then
    vim.o.background = "dark"
    --vim.cmd.colorscheme("catppuccin-frappe")
    vim.cmd.colorscheme("onenord")
else
    vim.o.background = "dark"
    vim.cmd.colorscheme("onenord")
end

-- Onehalf needs its vim/ subdirectory on rtp
local onehalf_path = vim.fn.stdpath('data') .. '/site/pack/core/opt/onehalf/vim'
if vim.fn.isdirectory(onehalf_path) == 1 then
    vim.opt.rtp:append(onehalf_path)
end
