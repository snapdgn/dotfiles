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
  use { 'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim' } } -- Just something one might use
  use 'lukas-reineke/indent-blankline.nvim'
  use 'numToStr/Comment.nvim' 
  use 'neovim/nvim-lspconfig'
  use 'tpope/vim-surround'
  use 'scrooloose/syntastic'
  use 'scrooloose/nerdcommenter'
  use 'machakann/vim-highlightedyank'
  use 'nvim-treesitter/nvim-treesitter'
  use { 'mhartington/formatter.nvim' }
 
  -- Completion
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-vsnip'
  use 'simrat39/rust-tools.nvim'
  use 'saadparwaiz1/cmp_luasnip'
  use 'rafamadriz/friendly-snippets'
  use 'L3MON4D3/LuaSnip'
  use 'kyazdani42/nvim-tree.lua'
  use 'thugcee/nvim-map-to-lua'

  -- git integrations
  use 'tpope/vim-fugitive'

  -- Debugging
  use 'mfussenegger/nvim-dap'

  -- auto detect filetypes
  use("nathom/filetype.nvim")
  --themes
  use 'sainnhe/gruvbox-material'
  use { "catppuccin/nvim", as = "catppuccin" }

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
  use({
  'glepnir/zephyr-nvim',
  requires = { 'nvim-treesitter/nvim-treesitter', opt = true },
})
-- Using Packer
use 'navarasu/onedark.nvim'

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


