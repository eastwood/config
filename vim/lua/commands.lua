-- Command Configuration

-- FZF
vim.env.FZF_DEFAULT_COMMAND= 'rg --hidden --files'

-- Git
vim.api.nvim_create_user_command('GitPushNoVerify', ':Git push --no-verify', { nargs = 1})

-- NERDTree

vim.g.NERDCreateDefaultMappings = 0

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

-- Useful functions

-- Create scratch buffer from R input
-- Use like :R echo 'hello'
vim.cmd [[
  command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>
  command! -nargs=0 FilePath :!echo $(pwd)/% | pbcopy
]]

