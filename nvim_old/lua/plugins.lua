local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.cmd [[ packadd packer.nvim ]]

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'
  use 'norcalli/nvim-base16.lua'
  use {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup()
    end
  }
  --use 'airblade/vim-gitgutter'
  use { 'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim' } } -- Just something one might use
  use 'lukas-reineke/indent-blankline.nvim'
  use 'numToStr/Comment.nvim'
  use 'tpope/vim-surround'
  use 'scrooloose/syntastic'
  use 'scrooloose/nerdcommenter'
  use 'machakann/vim-highlightedyank'
  use 'nvim-treesitter/nvim-treesitter'
  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly'                  -- optional, updated every week. (see issue #1193)
  }
  use { 'mhartington/formatter.nvim' }
  use 'jose-elias-alvarez/null-ls.nvim'
  use {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
  }
  require("mason").setup()

  -- zig
  use 'ziglang/zig.vim'

  -- vuejs
  use 'vuejs/vetur'

  -- using packer.nvim
  use { 'akinsho/bufferline.nvim', tag = "v3.*", requires = 'nvim-tree/nvim-web-devicons' } -- toggle-term
  use 'akinsho/toggleterm.nvim'
  --use {"akinsho/toggleterm.nvim", tag = '*', config = function()
  --require("toggleterm").setup()
  --end}

  -- Completion
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-vsnip'
  use 'simrat39/rust-tools.nvim'
  use 'saadparwaiz1/cmp_luasnip'
  use 'rafamadriz/friendly-snippets'
  --use({
  --"L3MON4D3/LuaSnip",
  ---- follow latest release.
  --tag = "v<CurrentMajor>.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
  ---- install jsregexp (optional!:).
  --run = "make install_jsregexp"
  --})
  --use 'kyazdani42/nvim-tree.lua'
  use 'thugcee/nvim-map-to-lua'

  -- git integrations
  use 'tpope/vim-fugitive'

  -- Debugging
  use 'mfussenegger/nvim-dap'

  -- auto detect filetypes
  --use("nathom/filetype.nvim")
  --themes
  use 'sainnhe/gruvbox-material'
  use 'sainnhe/everforest'
  --use { "catppuccin/nvim", as = "catppuccin" }
  use 'NLKNguyen/papercolor-theme'

  -- auto-pairs
  --use {
  --"windwp/nvim-autopairs",
  --config = function() require("nvim-autopairs").setup {} end
  --}
  --use 'cohama/lexima.vim'
  use 'jiangmiao/auto-pairs'

  -- vim rooter
  use 'airblade/vim-rooter'

  -- sniprun
  use 'michaelb/sniprun'

  -- nvim orgmode
  use 'nvim-orgmode/orgmode'
  -- Rust things
  use 'rust-lang/rust-clippy'

  -- colorschemes
  use 'folke/tokyonight.nvim'

  use({
    'glepnir/zephyr-nvim',
    requires = { 'nvim-treesitter/nvim-treesitter', opt = true },
  })
  -- Using Packer
  use 'navarasu/onedark.nvim'

  -- luasnip
  --use 'L3MON4D3/LuaSnip'

  -- markdowns
  use 'preservim/vim-markdown'
  -- markdown-preview
  use {
    "iamcco/markdown-preview.nvim",
    run = function()
      vim.fn["mkdp#util#install"]()
    end,
    ft = "markdown",
    cmd = { "MarkdownPreview" },
  }
  -- vimwiki
  use 'vimwiki/vimwiki'

  -- devicons
  use {
    "kyazdani42/nvim-web-devicons",
    module = "nvim-web-devicons",
    config = function()
      require("nvim-web-devicons").setup { default = true }
    end,
  }
  use 'ryanoasis/vim-devicons'
end)
