local bind = vim.keymap.set
local normal_mode = 'n'
local insert_mode = 'i'
local visual_mode = 'v'
local terminal_mode = 't'
local command_mode = 'c'

-- Keybindings

-- General
bind(normal_mode,  '<leader><leader>', ':Commands<CR>')
bind(insert_mode,  'jk',               '<ESC>')
bind(normal_mode,  '<backspace>',      '<C-^>')
bind(command_mode, '<A-BS>',           '<c-w>')
bind(insert_mode,  '<A-BS>',           '<c-w>')
bind(normal_mode,  '`',                "'")
bind(normal_mode,  "'",                '`')
bind(insert_mode,  '<C-s>',            '<C-o>:w!<CR>')
bind(normal_mode,  '<leader>qq',       ':wqa!<CR>')
bind(normal_mode,  '<leader><Esc>',    ':q!<CR>')

-- Project management
bind(normal_mode, '<leader>pf', ':Files<CR>')
bind(normal_mode, '<leader>pp', ':source ~/.vim/sessions/')
bind(normal_mode, '<leader>ps', ':mksession! ~/.vim/sessions/')
bind(normal_mode, '<leader>pt', ':NvimTreeToggle<CR>')

-- Git/Fugitive keybindings
bind(normal_mode, '<leader>gf', ':Git pull')
bind(normal_mode, '<leader>gp', ':Git push')
bind(normal_mode, '<leader>gs', ':Git<CR>')

-- Window bindings
bind(normal_mode, '<leader>wh',  '<C-w>h')
bind(normal_mode, '<leader>wj',  '<C-w>j')
bind(normal_mode, '<leader>wk',  '<C-w>k')
bind(normal_mode, '<leader>wl',  '<C-w>l')
bind(normal_mode, '<leader>wo',  '<C-w>o')
bind(normal_mode, '<leader>wc',  '<C-w>c')
bind(normal_mode, '<leader>ws',  '<C-w>s')
bind(normal_mode, '<leader>wv',  '<C-w>v')

-- Buffer bindings
bind(normal_mode, '<leader>d',  ':BD<CR>')
bind(normal_mode, '<leader>bd', ':BD!<CR>')
bind(normal_mode, '<leader>bb', ':Buffers<CR>')
bind(normal_mode, '<leader>bn', ':bn!<CR>')
bind(normal_mode, '<leader>bp', ':bp!<CR>')


-- File based 
bind(normal_mode, '<leader>ff',  ':edit ')
bind(normal_mode, '<leader>fs',  ':w!<CR>')
bind(normal_mode, '<leader>ft',  ':NvimTreeFindFile<CR>')
bind(normal_mode, '<leader>feR', ':source ~/.config/nvim/init.lua<CR>')
bind(normal_mode, '<leader>fed', ':FZF ~/.config/nvim/lua/<CR>')
bind(normal_mode, '<leader>fo',  ':edit ~/Documents/notes/backlog.md<CR>')

-- Searching
bind(normal_mode, '<Leader>sw', ':Rg <C-r><C-w>')
bind(normal_mode, '<Leader>sg', ':Rg<CR>')
bind(normal_mode, '<Leader>sb', ':BLines<CR>')
bind(normal_mode, '<leader>/',  ':noh<CR>')
bind(terminal_mode, '<Esc>',      '<C-\\><C-n>')

-- Other
bind(visual_mode, 'ga',    '<Plug>(EasyAlign)')
bind(normal_mode, 'ga',    '<Plug>(EasyAlign)')
bind(normal_mode, '<f12>', ':10split term://zsh<cr>')
bind(normal_mode, '<leader>tt', ':10split term://zsh<cr>')
bind(normal_mode, '\\',    ':NvimTreeToggle<CR>')

-- Coding
bind(normal_mode, '[c',         '<Plug>(coc-diagnostic-prev)',         { silent = true })
bind(normal_mode, ']c',         '<Plug>(coc-diagnostic-next)',         { silent = true })
bind(normal_mode, 'gd',         '<Plug>(coc-definition)',              { silent = true })
bind(normal_mode, 'gh',         '<Plug>(coc-type-definition)',         { silent = true })
bind(normal_mode, 'gi',         '<Plug>(coc-implementation)',          { silent = true })
bind(normal_mode, 'gr',         '<Plug>(coc-references)',              { silent = true })
bind(normal_mode, '<leader>cr', '<Plug>(coc-rename)',                  { silent = true })
bind(normal_mode, 'K',          ":call CocActionAsync('doHover')<CR>", { silent = true })
bind(normal_mode, '<leader>ca', '<Plug>(coc-codeaction)',              { silent = true, noremap = true })
bind(normal_mode, '<leader>cc', '<Plug>NERDCommenterToggle',           { silent = true, noremap = true })
bind(normal_mode, '<leader>cl', '<Plug>(coc-codelens-action)',         { silent = true, noremap = true })

bind(insert_mode, '<c-x><c-f>', '<Plug>(fzf-complete-path)',           { silent = true, noremap = true })

bind(insert_mode, '<Tab>', function()
  return vim.fn.pumvisible() == 1 and '<C-N>' or '<Tab>'
end, {expr = true})

bind(insert_mode, '<S-Tab>', function()
  return vim.fn.pumvisible() == 1 and '<C-p>' or '<C-h>'
end, {expr = true})

bind(insert_mode, '<cr>', function()
  return vim.fn.pumvisible() == 1 and '<C-y>' or '<C-g>u<CR>'
end, {expr = true})
