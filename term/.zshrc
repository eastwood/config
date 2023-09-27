# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh
# export LD_LIBRARY_PATH=/usr/local/lib/
export CUSTOM_SCRIPTS=~/.scripts
export GOPATH=~/Workspace/github.com/eastwood/go/
export LOCAL_SCRIPTS=~/.local/bin
export NODE_BIN="/home/eastwd/.nvm/versions/node/v16.7.0/bin"
export RUST_ANALYSER="home/eastwd/.local/bin"
export EMACS="/home/eastwd/emacs"
export PATH=$EMACS/bin:$LOCAL_SCRIPTS:/usr/local/bin:$PATH:$CUSTOM_SCRIPTS:$NODE_BIN:$RUST_ANALYSER

ZSH_THEME="lambda"

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

alias cdw='cd ~/Workspace/github.com/eastwood && $(ls | fzf) && clear'
alias vim="nvim"
alias emacsd="emacs --daemon"
alias ec="emacsclient -t"
alias npmi="npm i --legacy-peer-deps"
bindkey -s '^P' 'cdw^M'
export tmux="tmux"


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Setup fzf (relies on neovim install)
fzfDir=~/.local/share/nvim/site/pack/packer/start/fzf/
# ---------
if [[ ! "$PATH" == *${fzfDir}/bin* ]]; then
  PATH="${PATH:+${PATH}:}${fzfDir}/bin"
fi
# Auto-completion
# ---------------
[[ $- == *i* ]] && source "${fzfDir}/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "${fzfDir}/shell/key-bindings.zsh"

# VTERM for emacs
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

source $ZSH/oh-my-zsh.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
