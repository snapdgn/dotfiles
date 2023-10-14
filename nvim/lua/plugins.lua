vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.2',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }

    use({ 'rose-pine/neovim', as = 'rose-pine' })
    use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
    use('nvim-treesitter/playground')
    use('nvim-lua/plenary.nvim')
    use('ThePrimeagen/harpoon')
    use('mbbill/undotree')
    use('tpope/vim-fugitive')

    -- LSP plugins
    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x',
        requires = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' }, -- Required

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },     -- Required
            { 'hrsh7th/cmp-nvim-lsp' }, -- Required
            { 'L3MON4D3/LuaSnip' },     -- Required
        }
    }

    use('machakann/vim-sandwich')

    use('preservim/nerdcommenter')
    use('machakann/vim-highlightedyank')

    use('sbdchd/neoformat')
    use {
        "windwp/nvim-autopairs",
        config = function() require("nvim-autopairs").setup {} end
    }

    use('airblade/vim-rooter')

    use('lewis6991/gitsigns.nvim')

    use {
        'folke/trouble.nvim',
        requires = {
            -- LSP Support
            { 'nvim-tree/nvim-web-devicons' },
        },
        opts = {

        },
    }
    use('neovim/nvim-lspconfig')
    use('simrat39/rust-tools.nvim')
    use('lukas-reineke/indent-blankline.nvim')
    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'nvim-tree/nvim-web-devicons', opt = true }
    }
    use('navarasu/onedark.nvim')
    use("rebelot/kanagawa.nvim")
    use('nvim-tree/nvim-web-devicons')
end)
