-- Setup plugins

local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.config/nvim/plugged')

Plug 'tpope/vim-surround'
Plug 'morhetz/gruvbox'
Plug 'rakr/vim-one'
Plug 'romainl/flattened'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'leafgarland/typescript-vim'
Plug 'ianks/vim-tsx'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'vim-ruby/vim-ruby'
Plug 'fatih/vim-go'
Plug 'scrooloose/nerdcommenter'
Plug 'aklt/plantuml-syntax'
Plug 'junegunn/vim-easy-align'
Plug 'vim-scripts/nginx.vim'
Plug 'qpkorr/vim-bufkill'
Plug 'vimwiki/vimwiki'

-- With effects
Plug ('dracula/vim', { as = 'dracula' })
Plug ('pangloss/vim-javascript', {['for'] = 'javascript'})
Plug ('neoclide/coc.nvim', {branch = 'release'})
Plug ('iamcco/markdown-preview.nvim', { ['do'] = 'cd app & yarn install'  })

-- FZF
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

vim.call('plug#end')

local set = vim.opt

-- Set leader key
vim.g.mapleader = " " 

-- Basic settings
set.clipboard = 'unnamedplus'
set.completeopt = { 'menu', 'preview', 'longest', 'noinsert'}
set.modelines = 0
set.history = 500
set.autoread = true
set.showmatch = true
set.splitbelow = true
set.splitright = true 
set.mouse = 'a'

-- User Interface
set.bg = 'dark'
set.colorcolumn = '120'
set.cursorline = true
set.title = true
set.number = true
set.lazyredraw = true
set.signcolumn = 'yes'
set.hidden = true
set.showmode = true
set.updatetime = 300
set.shortmess = set.shortmess + 'c'
set.showcmd = true
set.statusline = '%f%=%l%L:%c'

if (vim.fn.has('gui_running') == 1) then
  set.guioptions= set.guioptions - 'm' --no menu
  set.guioptions= set.guioptions - 'T' --no toolbar
  set.guioptions= set.guioptions - 'r' --no scrollbar on the right
  set.guioptions= set.guioptions - 'L' --no scrollbar on the right
  set.guioptions= set.guioptions - 'b' --no scrollbar on the bottom
end

vim.cmd [[
  syntax enable
  colorscheme flattened_dark
  hi EndOfBuffer ctermfg=bg guifg=bg
  hi Cursor ctermbg=black
  hi VertSplit ctermbg=NONE
  hi StatusLineNC ctermbg=4 ctermfg=black
  hi StatusLine ctermbg=2 ctermfg=black
  hi LineNr ctermbg=NONE
]]

-- Files and Backups
set.writebackup = true
set.swapfile = false
set.undofile = true
set.backupdir = '.,/tmp'
set.undodir = '/tmp'
set.wildignore = '.svn,CVS,.git,*.swp,*.jpg,*.png,*.gif,*.pdf,*.bak'
set.suffixes = set.suffixes + '.bak'

-- Text Formatting
set.smartcase = true
set.expandtab = true
set.tabstop = 2
set.shiftwidth = 2
set.softtabstop = 2
set.ignorecase = true
set.hlsearch = true
set.wrap = false

-- Keybindings
-- General

vim.keymap.set('n', '<leader><leader>', ':Commands<CR>')
vim.keymap.set('i', 'jk', '<ESC>')

vim.keymap.set('n', '<backspace>', '<C-^>')
vim.keymap.set('c', '<A-BS>', '<c-w>')
vim.keymap.set('i', '<A-BS>', '<c-w>')
vim.keymap.set('n', '`', "'")
vim.keymap.set('n', "'", '`')
vim.keymap.set('i', '<C-s>', '<C-o>:w!<CR>')

vim.keymap.set('i', '<Tab>', function()
  return vim.fn.pumvisible() == 1 and '<C-N>' or '<Tab>'
end, {expr = true})

vim.keymap.set('i', '<S-Tab>', function()
  return vim.fn.pumvisible() == 1 and '<C-p>' or '<C-h>'
end, {expr = true})

vim.keymap.set('i', '<cr>', function()
  return vim.fn.pumvisible() == 1 and '<C-y>' or '<C-g>u<CR>'
end, {expr = true})

-- Project management
vim.keymap.set('n', '<leader>pf',  ':Files<CR>')
vim.keymap.set('n', '<leader>pp',  ':source ~/.vim/sessions/')
vim.keymap.set('n', '<leader>ps',  ':mksession! ~/.vim/sessions/')
vim.keymap.set('n', '<leader>pt',  ':NERDTreeToggle<CR>')


-- Git/Fugitive keybindings
vim.keymap.set('n', '<leader>gf',  ':Git pull')
vim.keymap.set('n', '<leader>gp',  ':Git push')
vim.keymap.set('n', '<leader>gs',  ':Git<CR>')

-- Window bindings
vim.keymap.set('n', '<leader>wh',  '<C-w>h')
vim.keymap.set('n', '<leader>wj',  '<C-w>j')
vim.keymap.set('n', '<leader>wk',  '<C-w>k')
vim.keymap.set('n', '<leader>wl',  '<C-w>l')
vim.keymap.set('n', '<leader>wo',  '<C-w>o')
vim.keymap.set('n', '<leader>wc',  '<C-w>c')
vim.keymap.set('n', '<leader>ws',  '<C-w>s')
vim.keymap.set('n', '<leader>wv',  '<C-w>v')
vim.keymap.set('n', '<f12>', ':10split term://zsh<cr>')
vim.keymap.set('n', '\\',  ':NERDTreeToggle<CR>')

-- Buffer bindings
vim.keymap.set('n', '<leader>d',  ':BD<CR>')
vim.keymap.set('n', '<leader>bd',  ':BD!<CR>')
vim.keymap.set('n', '<leader>bb',  ':Buffers<CR>')
vim.keymap.set('n', '<leader>bn',  ':bn!<CR>')
vim.keymap.set('n', '<leader>bp',  ':bp!<CR>')

vim.keymap.set('n', '<leader>qq',  ':wqa!<CR>')
vim.keymap.set('n', '<leader><Esc>',  ':q!<CR>')

-- File based 
vim.keymap.set('n', '<leader>ff',  ':edit ')
vim.keymap.set('n', '<leader>fs',  ':w!<CR>')
vim.keymap.set('n', '<leader>ft',  ':NERDTreeFind<CR>')
vim.keymap.set('n', '<leader>feR',  ':source ~/.vimrc<CR>')
vim.keymap.set('n', '<leader>fed',  ':edit ~/.config/nvim/init.lua<CR>')
vim.keymap.set('n', '<leader>fo',  ':edit ~/Documents/notes/backlog.md<CR>')

-- Searching
vim.keymap.set('n', '<Leader>sw',  ':Rg <C-r><C-w>')
vim.keymap.set('n', '<Leader>sg',  ':Rg<CR>')
vim.keymap.set('n', '<Leader>sb',  ':BLines<CR>')
vim.keymap.set('n', '<leader>/',  ':noh<CR>')
vim.keymap.set('t', '<Esc>',  '<C-\\><C-n>')

-- Other
vim.keymap.set('x', 'ga', '<Plug>(EasyAlign)')
vim.keymap.set('n', 'ga', '<Plug>(EasyAlign)')

-- Coding

vim.keymap.set('i', '<expr><c-space>', 'coc#refresh()', { silent = true, noremap = true })
vim.keymap.set('n', '[c', '<Plug>(coc-diagnostic-prev)', { silent = true })
vim.keymap.set('n', ']c', '<Plug>(coc-diagnostic-next)', { silent = true })
vim.keymap.set('n', 'gd', '<Plug>(coc-definition)', { silent = true })
vim.keymap.set('n', 'gh', '<Plug>(coc-type-definition)', { silent = true })
vim.keymap.set('n', 'gi', '<Plug>(coc-implementation)', { silent = true })
vim.keymap.set('n', 'gr', '<Plug>(coc-references)', { silent = true })
vim.keymap.set('n', '<leader>ac', '<Plug>(coc-codeaction)')
vim.keymap.set('n', '<leader>qa', '<Plug>(coc-codelens-action)')
vim.keymap.set('n', '<leader>qf', '<Plug>(coc-fix-current)')
vim.keymap.set('n', '<F2>', '<Plug>(coc-rename)', { silent = true })
vim.keymap.set('n', 'K', ":call CocActionAsync('doHover')<CR>", { silent = true })

-- Command Configuration

-- Coc
vim.g.coc_global_extensions = {'coc-json', 'coc-solargraph', 'coc-jest', 'coc-eslint', 'coc-tsserver', 'coc-python'}
vim.api.nvim_create_autocmd('CursorHold', {
  pattern = '*',
  command = "call CocActionAsync('highlight')"
})

-- FZF
vim.env.FZF_DEFAULT_COMMAND= 'rg --hidden --files'

-- Git
vim.api.nvim_create_user_command('GitPushNoVerify', ':Git push --no-verify', { nargs = 1})

-- NERDTree
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'nerdtree',
  command = 'map <buffer> <tab> <CR>'
})
vim.g.NERDTreeShowHidden = 1
vim.g.NERDTreeWinSize = 50

-- Formatters
vim.api.nvim_create_user_command('FormatXML', ':%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"', { bang = false})
vim.api.nvim_create_user_command('FormatJSON', ':%!jq', { bang = false })

-- Terminal
local augroup = vim.api.nvim_create_augroup('neovim_terminal', {clear = true})
vim.api.nvim_create_autocmd('TermOpen', {
  pattern = '*',
  group = augroup,
  command = 'setlocal nonumber' -- change this to func later
})
vim.api.nvim_create_autocmd('TermOpen', {
  pattern = '*',
  group = augroup,
  command = 'startinsert' -- change this to func later
})
vim.api.nvim_create_autocmd('TermOpen', {
  pattern = '*',
  group = augroup,
  command = 'nnoremap <buffer> <C-c> i<C-c>' -- change this to func later
})

-- Jest

vim.api.nvim_create_user_command('JestCurrent', ":call CocAction('runCommand', 'jest.fileTest', ['%'])", { nargs = 0})
vim.api.nvim_create_user_command('JestSingle',  ":call CocAction('runCommand', 'jest.singleTest')", { nargs = 0})

-- Useful functions

-- Create scratch buffer from R input
-- Use like :R echo 'hello'
vim.cmd [[
  command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>
  command! -nargs=0 FilePath :!echo $(pwd)/% | pbcopy
]]

