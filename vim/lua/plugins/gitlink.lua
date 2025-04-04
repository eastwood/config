local M = {}

local function get_remote_origin_url()
  local handle = io.popen("git config --get remote.origin.url")
  if not handle then
    print("Error: Unable to get remote origin URL.")
    return nil
  end
  local remote_url = handle:read("*a"):gsub("%s+", "")
  handle:close()

  if remote_url:match("git@") then
    remote_url = remote_url:gsub("git@github.com:", "https://github.com/"):gsub(".git$", "")
  elseif remote_url:match(".git$") then
    remote_url = remote_url:gsub(".git$", "")
  end

  return remote_url
end

local function get_current_branch()
  local handle = io.popen("git rev-parse --abbrev-ref HEAD")
  if not handle then
    print("Error: Unable to get current branch name.")
    return nil
  end
  local branch_name = handle:read("*a"):gsub("%s+", "")
  handle:close()
  return branch_name
end

function M.generate(start_line, end_line)
  local base_url = get_remote_origin_url()
  local branch_name = get_current_branch()

  local bufname = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
  local filename = vim.fn.fnamemodify(bufname, ":~:.")

  local line_parameter = ""
  if start_line and end_line and start_line ~= end_line then
    line_parameter = "#L" .. start_line .. "-L" .. end_line
  elseif start_line then
    line_parameter = "#L" .. start_line
  end

  local github_link = base_url .. "/blob/" .. branch_name .. "/" .. filename .. line_parameter
  vim.fn.setreg('+', github_link)
  vim.notify("GitHub link: " .. github_link .. " (Copied to clipboard)")
end

return M
