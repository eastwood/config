local bind = vim.keymap.set
local normal_mode = 'n'
local insert_mode = 'i'
local visual_mode = 'v'
local terminal_mode = 't'
local command_mode = 'c'

local opts = { noremap=true, silent=true }
local bufopts = { noremap=true, silent=true, buffer=bufnr }

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
bind(normal_mode, '<leader>cc', '<Plug>NERDCommenterToggle', opts)
bind(insert_mode, '<c-x><c-f>', '<Plug>(fzf-complete-path)', opts)


-- LSP 
bind(normal_mode, '<space>e', vim.diagnostic.open_float, opts)
bind(normal_mode, '[d', vim.diagnostic.goto_prev, opts)
bind(normal_mode, ']d', vim.diagnostic.goto_next, opts)
bind(normal_mode, 'gD', vim.lsp.buf.declaration, bufopts)
bind(normal_mode, 'gd', vim.lsp.buf.definition, bufopts)
bind(normal_mode, 'K', vim.lsp.buf.hover, bufopts)
bind(normal_mode, 'gi', vim.lsp.buf.implementation, bufopts)
bind(normal_mode, '<C-k>', vim.lsp.buf.signature_help, bufopts)
bind(normal_mode, '<space>D', vim.lsp.buf.type_definition, bufopts)
bind(normal_mode, '<space>rn', vim.lsp.buf.rename, bufopts)
bind(normal_mode, '<space>ca', vim.lsp.buf.code_action, bufopts)
bind(normal_mode, 'gr', vim.lsp.buf.references, bufopts)

-- Jest
bind(normal_mode, '<space>jt', function() require('jester').run() end, opts)

-- DAP

bind(normal_mode, '<leader><Up>', "<Cmd>lua require'dap'.continue()<CR>")
bind(normal_mode, '<leader><Down>', "<Cmd>lua require'dap'.step_over()<CR>")
bind(normal_mode, '<leader><Right>', "<Cmd>lua require'dap'.step_into()<CR>")
bind(normal_mode, '<leader><Left>', "<Cmd>lua require'dap'.step_out()<CR>")
bind(normal_mode, '<Leader>b', "<Cmd>lua require'dap'.toggle_breakpoint()<CR>")
bind(normal_mode, '<Leader>B', "<Cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
bind(normal_mode, '<Leader>lp', "<Cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>")
bind(normal_mode, '<Leader>dr', "<Cmd>lua require'dap'.repl.open()<CR>")
bind(normal_mode, '<Leader>dl', ":LoadLaunchJson")
bind(normal_mode, '<Leader>dui', function() 
  require("dapui").toggle()
end)
