# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh
# export LD_LIBRARY_PATH=/usr/local/lib/
export CUSTOM_SCRIPTS=~/.scripts
export GOBIN=~/Workspace/github.com/eastwood/go/bin
export LOCAL_SCRIPTS=~/.local/bin
export NODE_BIN="/home/eastwd/.nvm/versions/node/v16.7.0/bin"
export RUST_ANALYSER="home/eastwd/.local/bin"
export EMACS="/home/eastwd/emacs"
export DOTNET_TOOLS=~/.dotnet/tools
export PATH=$EMACS/bin:$LOCAL_SCRIPTS:/usr/local/bin:$PATH:$CUSTOM_SCRIPTS:$NODE_BIN:$RUST_ANALYSER:$GOBIN:$DOTNET_TOOLS

# User configuration
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

alias cdw='cd ~/Workspace/github.com/eastwood && cd $(ls | fzf) && clear'
alias vim="nvim"
alias emacsd="emacs --daemon"
alias ec="emacsclient -t"
alias npmi="npm i --legacy-peer-deps"
bindkey -s '^P' 'cdw^M'
export tmux="tmux"

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

eval "$(starship init zsh)"
source <(fzf --zsh)
