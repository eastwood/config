# Install git and ruby to get this up and running

# Install brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install workspace
mkdir -p ~/Workspace/github.com/eastwood
mkdir -p ~/Workspace/nib.com.au/cryan

# Brew install
brew install tmux
brew install python3
brew install python@2
brew install ruby
brew install reattach-to-user-namespace
brew install tmuxinator
brew install nvm
brew install neovim

# Install config
cd ~/Workspace/github.com/eastwood/
git clone https://github.com/eastwood/config
cd config
git submodule update --init
git submodule sync
cd ~

# Install tmux
ln -s ~/Workspace/github.com/eastwood/config/term/tmuxinator ~/.tmuxinator
ln -s ~/Workspace/github.com/eastwood/config/term/tmux.conf ~/.tmux.conf
ln -s ~/Workspace/github.com/eastwood/config/term ~/.config/term/

# Install node and others
nvm install 8
npm install -g typescript
npm install -g javascript-typescript-langserver

# Install nvim
mkdir -p ~/.config/nvim
pip3 install --index-url=http://pypi.python.org/simple/ --trusted-host pypi.python.org neovim
ln -s ~/Workspace/github.com/eastwood/config/vim/init.vim ~/.config/nvim/init.vim

# Install git bash
ln -s ~/Workspace/github.com/eastwood/config/term/.zshenv ~/.zshenv
ln -s ~/Workspace/github.com/eastwood/config/term/.zshrc ~/.zshrc
ln -s ~/Workspace/github.com/eastwood/config/term/.oh-my-zsh ~/.oh-my-zsh
