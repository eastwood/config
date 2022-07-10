local set = vim.opt

-- Set leader key
vim.g.mapleader = " " 

-- Basic settings
set.clipboard = 'unnamedplus'
set.completeopt = { 'menu', 'preview', 'longest', 'noinsert'}
set.autoread = true
set.mouse = 'a'

-- Text Formatting
set.smartcase = true
set.expandtab = true
set.tabstop = 2
set.shiftwidth = 2
set.softtabstop = 2
set.ignorecase = true
set.hlsearch = true
set.wrap = false
