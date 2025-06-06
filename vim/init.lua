-- Basic settings
vim.cmd.colorscheme("retrobox")

-- AI? What does this mean?
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Basic options
vim.opt.autoread = true
vim.opt.breakindent = true
vim.opt.clipboard = "unnamedplus"
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.hlsearch = true
vim.opt.inccommand = "split"
vim.opt.laststatus = 1
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.number = true
vim.opt.scrolloff = 10
vim.opt.shiftwidth = 2
vim.opt.signcolumn = "yes"
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.softtabstop = 2
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabstop = 2
vim.opt.undofile = true

local vimrc_path = vim.fn.stdpath("config") .. "/init.lua"
local code_path = "~/Workspace/github.com/eastwood/"

-- Insert mode keymaps
vim.keymap.set("i", "<A-BS>", "<C-w>", { desc = "Delete back word"} )
vim.keymap.set("i", "<A-o>", "<C-o><C-w>w", { desc = "Switch window"} )

-- Normal mode keymaps
vim.keymap.set("n", "<leader><leader>", ":", { desc = "Enter command mode" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("n", "<leader>fc", ":e " .. vimrc_path .. "<CR>", { desc = "Open vim config" })
vim.keymap.set("n", "<leader>bb", ":Buffers<CR>", { desc = "List [B]uffers" })
vim.keymap.set("n", "<leader>bd", ":bd!<CR>", { desc = "[B]uffer Delete" })
vim.keymap.set("n", "<leader>fe", ":e ", { desc = "[F]ind files" })
vim.keymap.set("n", "<leader>fs", ":w!<CR>", { desc = "[S]ave file" })
vim.keymap.set("n", "<leader>qq", ":wqall!<CR>", { desc = "[Q]uit" })
vim.keymap.set("n", "<leader>ll", ":vsplit | terminal aider --no-auto-commits --dark-mode", { desc = "New [L]LM Conversation" })
vim.keymap.set("n", "<leader>tt", ":split | terminal<CR>", { desc = "Open terminal" })
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

-- Code maps
vim.keymap.set('n', 'gD', function()
  vim.cmd('vsplit')  -- or 'split' for horizontal
  vim.lsp.buf.definition()
end, { desc = "Go to definition in split" })
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { desc = "Go to definition" })
vim.keymap.set("n", "gh", ":lua vim.diagnostic.open_float()<CR>", { desc = "Show line diagnostic" })
vim.keymap.set("n", "gb", ":lua vim.diagnostic.setloclist()<CR>", { desc = "Show buffer diagnostic" })
-- rename
vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, { desc = "[R]e[n]ame" })
vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, { desc = "[C]ode [A]ction" })

-- Terminal commands
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", { noremap = true, silent = true })

vim.diagnostic.config({
  virtual_text = true,
  float = { border = 'rounded', source = 'if_many' },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--branch=stable",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  "github/copilot.vim",
  "tpope/vim-sleuth",
  "folke/which-key.nvim",
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require('gitsigns').setup()
    end
  },
  "junegunn/fzf",
  {
    "tpope/vim-fugitive",
    config = function()
      vim.keymap.set("n", "<leader>g", ":Git<CR>", { desc = "Git status" })
    end
  },
  {
    "junegunn/fzf.vim",
    init = function()
      vim.keymap.set("n", "<leader>fn", ":Files " .. code_path .. "notes/<CR>", { desc = "Find in notes" })
      vim.keymap.set("n", "<leader>", ":Buffers<CR>", { desc = "Find in notes" })
      vim.keymap.set("n", "<leader>ff", ":Files<CR>", { desc = "Find files" })
      vim.keymap.set("n", "<leader>pf", ":GitFiles<CR>", { desc = "Search files in project" })
      vim.keymap.set("n", "<leader>pg", ":RG<CR>", { desc = "Grep in project" })
    end
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require('lspconfig')
      local on_attach = function(_, bufnr)
      end
      local servers = { "gopls", "ts_ls", "lua_ls" }
      for _, server in ipairs(servers) do
        lspconfig[server].setup({
          on_attach = on_attach
        })
      end
    end
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "lua", "vim", "javascript", "typescript", "tsx", "go", "markdown", "json" },
        highlight = { enable = true },
        indent = { enable = true },
      })
    end
  },
})

local gitlink = require("plugins.gitlink")

-- Create command
vim.api.nvim_create_user_command("GitLink", function(opts)
  gitlink.generate(opts.line1, opts.line2)
end, {
  desc = "Generate GitHub link for the current file and copy to clipboard",
  range = true,
})

-- Key mappings
vim.keymap.set('n', '<leader>gl', ':GitLink<CR>', { noremap = true, silent = true })
vim.keymap.set('v', '<leader>gl', ':GitLink<CR>', { noremap = true, silent = true })

vim.api.nvim_create_user_command('RunCmd', function(opts)
  local cmd = opts.args ~= "" and opts.args or "eslint -f unix ."
  vim.cmd("cexpr system('" .. cmd .. "')")
  vim.cmd("copen")
end, {
  nargs = "*", -- Accept any number of arguments
})
