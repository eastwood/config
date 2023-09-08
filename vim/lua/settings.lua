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

local in_wsl = os.getenv('WSL_DISTRO_NAME') ~= nil

if in_wsl then
  local paste_command = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))'
    vim.g.clipboard = {
        name = 'wsl clipboard',
        copy =  { ["+"] = { "clip.exe" },   ["*"] = { "clip.exe" } },
        paste = { ["+"] = { paste_command }, ["*"] = { paste_command } },
        cache_enabled = true
    }
end
