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
    'neovim/nvim-lspconfig', 
    config = function() 
      require('plugins.lsp').setup()
    end
  }
  use 'hrsh7th/cmp-nvim-lsp' -- LSP source for nvim-cmp
  use 'saadparwaiz1/cmp_luasnip' -- Snippets source for nvim-cmp
  use 'L3MON4D3/LuaSnip' -- Snippets plugin
  use { 
    'hrsh7th/nvim-cmp', 
    config = function() 
      require('plugins.cmp').setup()
    end
  }

  -- DAP configuration
  use {
    'mfussenegger/nvim-dap', 
    config = function() 
      require('plugins.dap').setup()
    end
  }

  use { "rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"}, config = function() 
    require('plugins.dap').setup_ui()
  end}

  use {
    'mxsdev/nvim-dap-vscode-js', 
    requires = {"mfussenegger/nvim-dap"} 
  }

  use {
    'microsoft/vscode-js-debug',
    opt = true,
    run = 'npm install --legacy-peer-deps && npm run compile' 
  }

  use {
    'nvim-treesitter/nvim-treesitter', 
    run = ':TSUpdate'
  }

  use {
	  'kylechui/nvim-surround', 
	  config = function()
		  require('nvim-surround').setup()
	  end
  }

  use 'kdheepak/lazygit.nvim'
  use {'tpope/vim-fugitive' }
  use 'tpope/vim-rhubarb'
  use 'junegunn/vim-easy-align'
  use 'qpkorr/vim-bufkill'
  use 'junegunn/fzf'
  use 'junegunn/fzf.vim'
  use 'David-Kunz/jester'
  use 'folke/which-key.nvim'
  use 'scrooloose/nerdcommenter'

end)
