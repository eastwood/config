-- Automatically build
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons' },
    config = function()
      require("lualine").setup({
        options = {
          theme = "everforest"
        }
      })
    end}
  use({
    "neanias/everforest-nvim",
    -- Optional; default configuration will be used if setup isn't called.
    config = function()
      require("everforest").load()
    end,
  })
  use {"peitalin/vim-jsx-typescript", ft={'typescriptreact'}}
  use {"iamcco/markdown-preview.nvim", ft={'markdown'}}
  use 'nvim-treesitter/nvim-treesitter'
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use 'junegunn/vim-easy-align'
  use {'ionide/Ionide-vim', config = function()
    vim.cmd[[let g:fsharp#fsi_keymap = "vim-fsharp"]]
  end}
  use 'qpkorr/vim-bufkill'
  use 'junegunn/fzf'
  use 'junegunn/fzf.vim'
  use 'David-Kunz/jester'
  use 'scrooloose/nerdcommenter'
  use {
    'kyazdani42/nvim-tree.lua',
    requires = {
      'kyazdani42/nvim-web-devicons'
    },
    cmd = { 'NvimTreeToggle', 'NvimTreeFocus', 'NvimTreeFindFile' },
    opt = true,
    config = function()
      require('nvim-tree').setup({
        view = {
          width = 50,
          side = "right"
        }
      })
    end
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
  use {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup()
    end
  }
end)

