# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export EDITOR="nvim"
export TEX_PATH="/usr/local/texlive/2018/bin/x86_64-darwin"
export TERM='xterm-256color'
export NIB_PATH=/Users/cryan/Workspace/nib.com.au/cryan/bin
export HOMEBREW_PATH=/usr/local/sbin:/usr/local/bin
export CUSTOM_SCRIPTS=~/.scripts
export NODE_PATH=~/.nvm/versions/node/v12.19.0/bin
export PATH=$HOMEBREW_PATH:$NIB_PATH:$TEX_PATH:$CUSTOM_SCRIPTS:$NODE_PATH:$PATH

eval "$(starship init zsh)"
# source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
bindkey '^Y' autosuggest-accept
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

alias emacsd="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
alias cdw="cd ~/Workspace/github.com/eastwood/"
alias flushdns="mDNSResponder -HUP"
alias vim="nvim"
alias ec="emacsclient -t"
alias em="emacs -nw"
alias ls="ls -lsaG"
alias jcurl="curl -H 'Content-Type: application/json'"
alias grep="grep --color=auto"
alias debug-chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222"
alias create-loopback="sudo ifconfig lo0 alias 10.100.10.1"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH=$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export NVM_DIR="${HOME}/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use
