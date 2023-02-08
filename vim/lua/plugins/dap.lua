local M = {}

M.setup = function()
  local dap = require('dap')

  dap.defaults.fallback.terminal_win_cmd = '10split new'
  vim.api.nvim_create_user_command('LoadLaunchJson', function()
    require('dap.ext.vscode').load_launchjs(nil, {['pwa-node'] = {'typescript', 'javascript', 'typescriptreact' }})
  end, { bang = false })

  require("dap-vscode-js").setup({
    node_path = "/home/eastwd/.nvm/versions/node/v16.17.0/bin/node", -- Path of node executable. Defaults to $NODE_PATH, and then "node"
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

  local dapui = require('dapui')
  dapui.setup()

end

M.widget = function()
  local widgets = require('dap.ui.widgets')
  local my_sidebar = widgets.sidebar(widgets.scopes)
  my_sidebar.open()
end

M.setup_ui = function() 
  local dap, dapui = require("dap"), require("dapui")
  dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
  end
  dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
  end
  dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
  end
end

return M
