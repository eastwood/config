local set = vim.opt

-- Files and Backups
set.writebackup = true
set.swapfile = false
set.undofile = true
set.backupdir = '.,/tmp'
set.undodir = '/tmp'
set.wildignore = '.svn,CVS,.git,*.swp,*.jpg,*.png,*.gif,*.pdf,*.bak'
set.suffixes = set.suffixes + '.bak'

