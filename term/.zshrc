# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export LD_LIBRARY_PATH=/usr/local/lib/:$LD_LIBRARY_PATH
export GO_BIN=~/go/bin:/usr/local/go/bin
export CUSTOM_SCRIPTS=~/.scripts
export LOCAL_SCRIPTS=~/.local/bin
export RUST_ANALYSER="home/eastwd/.local/bin"
export EMACS="/home/eastwd/emacs"
export DOTNET_TOOLS="/home/eastwd/.dotnet/tools"
export PATH=$EMACS/bin:$LOCAL_SCRIPTS:/usr/local/bin:$PATH:$CUSTOM_SCRIPTS:$NODE_BIN:$RUST_ANALYSER:$GO_BIN:$YT:$DOTNET_TOOLS

ZSH_THEME="nicoulaj"
plugins=(git rbenv)
source $ZSH/oh-my-zsh.sh

# User configuration
export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
export EDITOR='nvim'

alias cdw='cd ~/Workspace/github.com/eastwood && $(ls | fzf) && clear'
alias vim="nvim"
alias emacsd="emacs --daemon"
alias ec="emacsclient -t"
alias npmi="npm i --legacy-peer-deps"
bindkey -s '^P' 'cdw^M'
export tmux="tmux"

# export BROWSER="/mnt/c/Windows/explorer.exe"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
