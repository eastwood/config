# Install git and ruby and brew to get this up and running

# Install workspace
mkdir -p ~/Workspace/github.com/eastwood
mkdir -p ~/Workspace/nib.com.au/cryan

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


# Install config
cd ~/Workspace/github.com/eastwood/
git submodule update --init
git submodule sync
cd ~

# Install tmux
ln -s ~/Workspace/github.com/eastwood/config/term/tmux.conf ~/.tmux.conf
ln -s ~/Workspace/github.com/eastwood/config/term ~/.config/term/

# Install node and others
nvm install 10
npm install -g typescript
npm install -g javascript-typescript-langserver

# Install nvim
mkdir -p ~/.config/nvim
pip3 neovim
ln -s ~/Workspace/github.com/eastwood/config/vim/init.vim ~/.config/nvim/init.vim
ln -s ~/Workspace/github.com/eastwood/config/vim/vimrc ~/.vimrc

# Install git bash
ln -s ~/Workspace/github.com/eastwood/config/term/.zshrc ~/.zshrc
