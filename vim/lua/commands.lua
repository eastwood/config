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

-- Add additional capabilities supported by nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

local lspconfig = require('lspconfig')

-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
local servers = { 'rust_analyzer', 'pyright', 'tsserver', 'eslint' }
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    -- on_attach = my_custom_on_attach,
    capabilities = capabilities,
  }
end

-- luasnip setup
local luasnip = require 'luasnip'

-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  },
}

local dap = require('dap')
dap.defaults.fallback.terminal_win_cmd = '10split new'
vim.api.nvim_create_user_command('LoadLaunchJson', function()
  require('dap.ext.vscode').load_launchjs(nil, {['pwa-node'] = {'typescript', 'javascript'}})
end, { bang = false })

require("dap-vscode-js").setup({
  node_path = "/home/eastwd/.nvm/versions/node/v14.17.6/bin/node", -- Path of node executable. Defaults to $NODE_PATH, and then "node"
  -- debugger_path = "(runtimedir)/site/pack/packer/opt/vscode-js-debug", -- Path to vscode-js-debug installation.
  -- debugger_cmd = { "js-debug-adapter" }, -- Command to use to launch the debug server. Takes precedence over `node_path` and `debugger_path`.
  adapters = { 'node', 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost' }, -- which adapters to register in nvim-dap
})

for _, language in ipairs({ "typescript", "javascript" }) do
  require("dap").configurations[language] = {
    {
      type = "pwa-node",
      request = "launch",
      name = "Join API",
      program = "${workspaceFolder}/dist/server.js",
      cwd = "${workspaceFolder}",
      console = "integratedTerminal"
    }
  }
end

local widgets = require('dap.ui.widgets')
local my_sidebar = widgets.sidebar(widgets.scopes)
-- my_sidebar.open()
