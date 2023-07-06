-- Automatically build
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use {'nvim-lualine/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true }}
  use 'folke/tokyonight.nvim'
  use 'tomasiser/vim-code-dark'
  use 'kongo2002/fsharp-vim'
  use {"peitalin/vim-jsx-typescript", ft={'typescriptreact'}}
  use {"iamcco/markdown-preview.nvim", ft={'markdown'}}
  use 'nvim-treesitter/nvim-treesitter'
  use 'kdheepak/lazygit.nvim'
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use 'junegunn/vim-easy-align'
  use 'qpkorr/vim-bufkill'
  use 'junegunn/fzf'
  use 'junegunn/fzf.vim'
  use 'David-Kunz/jester'
  use 'folke/which-key.nvim'
  use 'scrooloose/nerdcommenter'
  use {
    'kyazdani42/nvim-tree.lua',
    requires = {
      'kyazdani42/nvim-web-devicons', -- optional, for file icons
    },
    command = ':NvimTreeToggle',
    config = function()
      require('nvim-tree').setup()
    end,
    tag = 'nightly'
  }
  -- LSP configuration
  use {
	  'VonHeikemen/lsp-zero.nvim',
	  branch = 'v1.x',
	  requires = {
		  -- LSP Support
		  {'neovim/nvim-lspconfig'},
		  {'williamboman/mason.nvim'},
		  {'williamboman/mason-lspconfig.nvim'},

		  -- Autocompletion
		  {'hrsh7th/nvim-cmp'},
		  {'hrsh7th/cmp-buffer'},
		  {'hrsh7th/cmp-path'},
		  {'saadparwaiz1/cmp_luasnip'},
		  {'hrsh7th/cmp-nvim-lsp'},
		  {'hrsh7th/cmp-nvim-lua'},

		  -- Snippets
		  {'L3MON4D3/LuaSnip'},
		  {'rafamadriz/friendly-snippets'},
	  }
  }
  use { 'kylechui/nvim-surround',
	  config = function()
		  require('nvim-surround').setup()
	  end
  }

end)

