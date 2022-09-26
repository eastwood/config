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
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

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
