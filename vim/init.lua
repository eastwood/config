-- Basic settings
vim.cmd.colorscheme("retrobox")
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Basic options
vim.opt.breakindent = true
vim.opt.clipboard = "unnamedplus"
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.hlsearch = true
vim.opt.inccommand = "split"
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

-- Basic keymaps
vim.keymap.set("i", "<A-BS>", "<C-w>", { desc = "Delete back word"} )
vim.keymap.set("i", "<A-o>", "<C-o><C-w>w", { desc = "Switch window"} )

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("n", "<leader>bd", ":bd!<CR>", { desc = "[B]uffer Delete" })
vim.keymap.set("n", "<leader>ff", ":e ", { desc = "[F]ind files" })
vim.keymap.set("n", "<leader>fs", ":w!<CR>", { desc = "[S]ave file" })
vim.keymap.set("n", "<leader>qq", ":wqall!<CR>", { desc = "[Q]uit" })
vim.keymap.set("n", "<leader>lc", ":%!llm -c<CR>", { desc = "[C]ontinue Conversation" })
vim.keymap.set("n", "<leader>ll", ":%!llm<CR>", { desc = "New [L]LM Conversation" })

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tt", ":split | terminal<CR>", { desc = "Open terminal" })

-- Add new keybindings here
vim.keymap.set("n", "<leader><leader>", ":", { desc = "Enter command mode" })
vim.keymap.set("n", "<leader>.c", ":e " .. vimrc_path .. "<CR>", { desc = "Open vim config" })

-- Window navigation
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

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
      vim.keymap.set("n", "<leader>.n", ":Files " .. code_path .. "notes/<CR>", { desc = "Find in notes" })
      vim.keymap.set("n", "<leader>", ":Buffers<CR>", { desc = "Find in notes" })
      vim.keymap.set("n", "<leader>fg", ":Rg<CR>", { desc = "Grep files" })
      vim.keymap.set("n", "<leader>pf", ":Files<CR>", { desc = "Search files in project" })
      vim.keymap.set("n", "<leader>pg", ":Rg<CR>", { desc = "Grep in project" })
    end
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require('lspconfig')
      local on_attach = function(_, bufnr)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr })
      end
      local servers = { "gopls", "ts_ls", "lua_ls" }
      for _, server in ipairs(servers) do
        lspconfig[server].setup({
          on_attach = on_attach,
        })
      end
    end
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "lua", "vim", "javascript", "typescript", "go", "markdown", "json" },
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
