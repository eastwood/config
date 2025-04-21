# Sets simple prompt
PROMPT='%F{green}%2~%f %# '
# Set history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
# Set PATHS
export MANPATH="/usr/local/man:$MANPATH"
export LD_LIBRARY_PATH=/usr/lib64/:/usr/local/lib/:$LD_LIBRARY_PATH
export GO_BIN=~/go/bin:/usr/local/go/bin
export CUSTOM_SCRIPTS=~/.scripts
export LOCAL_SCRIPTS=~/.local/bin
export DOTNET_TOOLS=~/.dotnet/tools
export PATH=$LOCAL_SCRIPTS:/usr/local/bin:$PATH:$CUSTOM_SCRIPTS:$GO_BIN:$DOTNET_TOOLS
export LUA_LS="/opt/lua-language-server/bin"
export EDITOR='nvim'
# Set aliases
alias ai="aider --no-auto-commit --dark-mode"
alias cdw='cd ~/Workspace/github.com/eastwood && cd $(ls | fzf)'
alias vim="nvim"
alias emacsd="emacs --daemon"
alias ec="emacsclient -t"
alias npmi="npm i --legacy-peer-deps"
# Find project
bindkey -s '^P' 'cdw^M'
# Setup fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# Add C-x C-e to edit the current command line in nvim
autoload -z edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line
# set browser when using WSL
if grep -q Microsoft /proc/version; then
  export BROWSER="wslview"
else
  export BROWSER="xdg-open"
fi
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
