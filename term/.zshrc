# Path to your oh-my-zsh installation.
export LD_LIBRARY_PATH=/usr/local/lib/:$LD_LIBRARY_PATH
export GO_BIN=~/go/bin:/usr/local/go/bin
export CUSTOM_SCRIPTS=~/.scripts
export LOCAL_SCRIPTS=~/.local/bin
export DOTNET_TOOLS="/home/eastwd/.dotnet/tools"
export PATH=$LOCAL_SCRIPTS:/usr/local/bin:$PATH:$CUSTOM_SCRIPTS:$GO_BIN:$DOTNET_TOOLS

# User configuration
export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
export EDITOR='nvim'

alias ai="aider --no-auto-commit --dark-mode"
alias cdw='cd ~/Workspace/github.com/eastwood && $(ls | fzf) && clear'
alias vim="nvim"
alias emacsd="emacs --daemon"
alias ec="emacsclient -t"
alias npmi="npm i --legacy-peer-deps"
bindkey -s '^P' 'cdw^M'
export tmux="tmux"

PROMPT='%F{green}%2~%f %\$ '

# export PYENV_ROOT="$HOME/.pyenv"
# [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
# export BROWSER="/mnt/c/Windows/explorer.exe"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Add C-x C-e to edit the current command line in nvim
autoload -z edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

