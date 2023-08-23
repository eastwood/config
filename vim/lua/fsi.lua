local channel_id = -1

local function fsi_open()
  vim.cmd[[botright 12 new]]
  vim.cmd[[terminal dotnet fsi]]
  vim.cmd[[normal! G]]
  vim.cmd("startinsert") channel_id = vim.b.terminal_job_id
  vim.cmd[[wincmd p]]
end

local function fsi_send()
  local cmd = vim.api.nvim_get_current_line()
  if (channel_id == -1) then
    fsi_open()
  else
    vim.api.nvim_chan_send(channel_id, cmd .. ";;" .. "\n")
  end
end

local function fsi_lines()
  local start_pos, _ = unpack(vim.api.nvim_buf_get_mark(0, "<"))
  local end_pos, _ = unpack(vim.api.nvim_buf_get_mark(0, ">"))
  local lines = vim.api.nvim_buf_get_lines(0, start_pos - 1, end_pos, 0)
  local output = ""
  for _,v in pairs(lines) do
    output = output .. v .. "\n"
  end
  if (channel_id == -1) then
    fsi_open()
  else
    vim.api.nvim_chan_send(channel_id, output .. ";;" .. "\n")
  end
end

vim.api.nvim_create_user_command("FsiSend", function() fsi_send() end, { nargs = 0, range = 1 })
vim.api.nvim_create_user_command("FsiLines", function() fsi_lines() end, { nargs = 0, range = 1 })
vim.api.nvim_create_user_command("FsiOpen", function() fsi_open() end, { nargs = 0, range = 1 })

local bind = vim.keymap.set
bind('v', '<leader>rr', ':FsiLines<CR>')
bind('n', '<leader>rr', ':FsiSend<CR>')

return {
  fsi_open = fsi_open,
  fsi_send = fsi_send,
  channel_id = channel_id
}

