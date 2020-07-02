# Vim Overview

While the default vim is amazing out of the box, I have created some custom keybindings and
used vim plugins to help improve support for particular languages, plugins include:

- Language server support via coc-nvim (autocomplete, linting etc)
- Useful plugins for making life easier (vim-surround, easy-align, fzf)
- NerdTree for project tree support
- Ultisnips for snippet support
- FZF for project searching
- vim-fugitive for Git support

I also use solarized as my goto colour scheme and for good reason.

Languages that I included support and is subject to change regularly:
- Typescript/Javascript
- Ruby

I am primarily using neovim, but it's backwards compatible with vim8.

## Installation

```
> brew install nvim
> git clone git@github.com:eastwood/config.git
> mkdir -p .vim
> mkdir -p .config/nvim
> cp config/autoload .vim/
> cp config/vimrc ~/.vimrc
> cp config/init.vim ~/.config/nvim 
> nvim
# Run :Plug-Install
```
