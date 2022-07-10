local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.config/nvim/plugged')

-- UI
Plug 'romainl/flattened'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

-- Code
Plug 'leafgarland/typescript-vim'
Plug 'ianks/vim-tsx'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'vim-ruby/vim-ruby'
Plug 'fatih/vim-go'
Plug 'vimwiki/vimwiki'
Plug 'vim-scripts/nginx.vim'
Plug 'scrooloose/nerdcommenter'

Plug ('neoclide/coc.nvim', {branch = 'release'})
Plug ('pangloss/vim-javascript', {['for'] = 'javascript'})
Plug ('iamcco/markdown-preview.nvim', { ['do'] = 'cd app & yarn install'  })

-- Utility
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'aklt/plantuml-syntax'
Plug 'junegunn/vim-easy-align'
Plug 'qpkorr/vim-bufkill'

-- Search
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'


vim.call('plug#end')
