-- Automatically build
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'
  use 'itchyny/lightline.vim'
  use 'rakr/vim-one'
  use 'romainl/flattened'
  use 'NLKNguyen/papercolor-theme'
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
  use 'scrooloose/nerdcommenter'
  use {"iamcco/markdown-preview.nvim", ft={'markdown'}}
  use {'vimwiki/vimwiki', config = function()
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
    ]]
  end}
  use {'neoclide/coc.nvim', branch = 'release'}
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use {
	  'kylechui/nvim-surround', 
	  config = function()
		  require('nvim-surround').setup()
	  end
  }

  use {'tpope/vim-fugitive', cmd = 'Git'}
  use 'tpope/vim-rhubarb'
  use 'junegunn/vim-easy-align'
  use 'qpkorr/vim-bufkill'

  use 'junegunn/fzf'
  use 'junegunn/fzf.vim'
end)
