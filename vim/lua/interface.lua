local set = vim.opt

-- User Interface
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
set.statusline = '%f%=%l:%c'
set.modelines = 0
set.history = 500
set.showmatch = true
set.splitbelow = true
set.splitright = true 
set.termguicolors = true

if (vim.fn.has('gui_running') == 1) then
  set.guioptions= set.guioptions - 'm' --no menu
  set.guioptions= set.guioptions - 'T' --no toolbar
  set.guioptions= set.guioptions - 'r' --no scrollbar on the right
  set.guioptions= set.guioptions - 'L' --no scrollbar on the right
  set.guioptions= set.guioptions - 'b' --no scrollbar on the bottom
end

vim.cmd [[
  syntax enable
]]

require('onedark').load()
require('lualine').setup()
