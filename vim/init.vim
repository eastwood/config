""""""""""""""""""
" NeoVim Configuration
" Version: 0.2.0
" Created By: Clint Ryan @eastwood
"""""""""""""""""
" Install packages

call plug#begin()
Plug 'junegunn/fzf'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'vim-ruby/vim-ruby'
Plug 'fatih/vim-go'
Plug 'tpope/vim-surround'
" Plug 'altercation/vim-colors-solarized'
" Plug 'zchee/deoplete-go'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install' }
" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'
" Plug 'tpope/vim-fugitive'
" Plug 'airblade/vim-gitgutter'
" Plug 'neomake/neomake'
" Plug 'benjie/neomake-local-eslint.vim'
call plug#end()

filetype plugin indent on

" Settings
  set spell
  set hidden
  set clipboard+=unnamedplus
  " General Settings
  syntax on
  let $TMUX_TUI_ENABLE_CURSOR_SHAPE=1
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
  let mapleader = " "
  set cursorline
  set wildignore=.svn,.git
  set splitbelow splitright
  set undodir=/tmp/vim
  set undofile

  " UI
  set bg=light
  colorscheme default

  set title
  set colorcolumn=120
  set completeopt-=preview
  set number
  set relativenumber

  set fillchars=vert:\ 

  " Text line
  set expandtab
  set smarttab
  set shiftwidth=2
  set tabstop=2

" Keybindings

  " General Keybindings
  imap jk <ESC>
  imap <C-s> <C-o>:w!<CR>
  imap <C-v> <ESC>"+gP
  imap <M-backspace> <C-w>
  imap <M-del> <ESC>dwi

  nnoremap <backspace> <C-^>
  nnoremap ` '
  nnoremap ' `
  nnoremap / /\v
  nnoremap n nzz
  nnoremap N Nzz
  nnoremap <leader><leader> :
  map \ :NERDTreeToggle<cr>
  noremap <F12> :sp term://bash<cr>
  vmap <C-c> "+y

  " Window bindings
  nnoremap <leader>wh <C-w>h
  nnoremap <leader>wj <C-w>j
  nnoremap <leader>wk <C-w>k
  nnoremap <leader>wl <C-w>l
  nnoremap <leader>wo <C-w>o
  nnoremap <leader>wc <C-w>c
  nnoremap <leader>ws <C-w>s
  nnoremap <leader>wv <C-w>v

  " Buffer bindings
  map <tab> :bn!<CR>
  map <S-tab> :bp!<CR>
  nmap <leader>bd :bp\|bd #<cr>
  nmap <leader>bb :buffers<cr>
  nmap <leader>bn :bn!<CR>
  nmap <leader>bp :bp!<CR>

  " Save and quit
  nmap <leader>qq :wqa!<CR>
  nmap <leader>/ :noh<CR>

  " Project management
  nmap <leader>pf :FZF<CR>
  nmap <leader>ft :NERDTreeFind<cr>
  nmap <leader>pt :NERDTreeCWD<cr>
  nmap <leader>pp :source ~/.config/nvim/sessions/
  nmap <leader>ps :mksession! ~/.config/nvim/sessions/

  " Todo List Management
  nnoremap <leader>at :edit ~/Dropbox/nib/log/todo.md

  " File Bindings
  nmap <leader>ff :edit 
  nmap <leader>fs :w!<CR>
  nmap <leader>feR :source ~/.config/nvim/init.vim<CR>
  nmap <leader>fed :edit ~/.config/nvim/init.vim<CR>

  " Toggles
  nnoremap <leader>tl :set nowrap<cr>

  " Terminal keybindings
  tnoremap <ESC> <C-\><C-n>
  tnoremap jk <C-\><C-n>

  " Git/Fugitive keybindings
  nnoremap <leader>gs :Gstatus<cr>
  nnoremap <leader>gf :Gfetch<cr>
  nnoremap <leader>gF :!git pull --rebase<cr>
  nnoremap <leader>gP :Gpush
  nnoremap <leader>gc :Gcommit<cr>
  nnoremap <leader>gC :Git checkout

  " CTags related
  nnoremap <leader>ct :TagbarToggle<cr>
  noremap <leader>cg :terminal ctags -R .<cr>

  " Grepping
  nnoremap H :grep -R <C-R><C-W>

" Plugin Settings

  " UltiSnips settings
  inoremap <silent><expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
  inoremap <silent><expr><S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
  let g:UltiSnipsExpandTrigger="<C-j>"
  let g:UltiSnipsJumpForwardTrigger="<C-j>"
  let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" Miscellaneous Hacks

  " Save when focus is gone
  autocmd BufLeave,FocusLost * silent! wall
  " Don't unload terminal
  autocmd TermOpen * set bufhidden=hide

  " FZF functions
  function! s:buflist()
    redir => ls
    silent ls
    redir END
    return split(ls, '\n')
  endfunction

  function! s:bufopen(e)
    execute 'buffer' matchstr(a:e, '^[ 0-9]*')
  endfunction

  nnoremap <silent> <leader>bb :call fzf#run({
  \   'source':  reverse(<sid>buflist()),
  \   'sink':    function('<sid>bufopen'),
  \   'options': '+m',
  \   'down':    len(<sid>buflist()) + 2
  \ })<CR>
