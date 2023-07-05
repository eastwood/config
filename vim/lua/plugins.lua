-- Automatically build
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

require('packer').startup(function()
  use 'wbthomason/packer.nvim'
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  }

  use 'folke/tokyonight.nvim'
  use 'tomasiser/vim-code-dark'

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

  use {"ionide/Ionide-vim"}
  use {"peitalin/vim-jsx-typescript", ft={'typescriptreact'}}
  use {"iamcco/markdown-preview.nvim", ft={'markdown'}}
  use {
    'vimwiki/vimwiki', 
    config = function()
      vim.cmd[[
        let g:vimwiki_key_mappings =
        \ {
          \   'all_maps': 1,
          \   'global': 1,
          \   'headers': 1,
          \   'text_objs': 1,
          \   'table_format': 1,
          \   'table_mappings': 1,
          \   'lists': 1,
          \   'links': 1,
          \   'html': 0,
          \   'mouse': 1,
          \ }
          let g:vimwiki_list = [{'path': '~/Workspace/github.com/eastwood/notes', 'syntax': 'markdown', 'ext': '.md'}]
      ]]
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

  use {
    'nvim-treesitter/nvim-treesitter', 
    run = ':TSUpdate',
    config = function() 
      local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
      parser_config.fsharp = {
        install_info = {
          url = "/home/eastwd/treesit/tree-sitter-fsharp",
          files = {"src/scanner.cc", "src/parser.c" },
          generate_requires_npm = false,
          requires_generate_from_grammar = false
        },
        filetype = "fs",
      }
    end
  }

  use {
	  'kylechui/nvim-surround', 
	  config = function()
		  require('nvim-surround').setup()
	  end
  }

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

end)

