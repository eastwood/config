# Install git and ruby and brew to get this up and running

# Install workspace
mkdir -p ~/Workspace/github.com/eastwood

# Brew install
brew install tmux
brew install python3
brew install ruby
brew install neovim
brew install nvm
brew install fzf
brew install zsh
brew install starship
brew install reattach-to-user-namespace
brew install rg

# Install config
cd ~/Workspace/github.com/eastwood/
git submodule update --init
git submodule sync
cd ~

# Install tmux
ln -s ~/Workspace/github.com/eastwood/config/term/tmux.conf ~/.tmux.conf
ln -s ~/Workspace/github.com/eastwood/config/term ~/.config/term/

# Install nvim
mkdir -p ~/.config
ln -s ~/Workspace/github.com/eastwood/config/vim ~/.config/nvim

# Install git bash
ln -s ~/Workspace/github.com/eastwood/config/term/.zshrc ~/.zshrc
