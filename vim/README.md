### Vim Configuration Files
Vim is awesome. Once some basic fundamentals are mastered, you'll find yourself editing text
more efficiently, faster and with little context switching. Vim is more a language than a program,
once your learn modal editing, you'll find yourself able to use it in across a wide range of applications,
some applications support vim keybinds already:
- Gmail
- Reddit
- Atlassian Products

You can also get vim support for many other products:

- Sublime Text
- Emacs
- Google Chrome
- Eclipse
- IntelliJ 
- Visual Studio etc.

Essentially, wherever there is a requisite to type, vim support is usually available.

#### Keybindings
While the default vim is amazing out of the box, I have created some custom keybindings and
used vim plugins to help improve support for particular languages, plugins include:

- NeoMake/Syntastic for syntax checking
- SuperTab for fast context completion
- Surround for editing regions
- Airline for nicer styling
- NerdTree for project tree support
- Ultisnips for snippet support
- CtrlP for fast searching

I also use solarized as my goto colour scheme and for good reason.

Languages that I included support and is subject to change regularly:
- Javascript (vim-javascript, tern, eslint)
- Go (vim-go)

### Versions
I have recently switched to neovim in favour of supporting the community and utilising its asynchronous features. As such, I have a 'vanilla' branch that can be used that offers the same functionality, just with a slightly different set of plugins and config.

### Notable Keybindings
Keybindings can be found in the `init.vim` file, but the notable ones are here:

`Space` - Leader

#### Insert Mode
`jk` - Exit insert mode

`Ctrl-S` - Save file

`Ctrl-C` - Copy region

`Ctrl-V` - Paste clipboard

`F12` - Open terminal

### Normal Mode
`Space-H` - Previous buffer

`Space-L` - Next buffer

`Space-T` - Toggle tagbar

`Space-E` - Edit file

`Space-D` - Close buffer

`Space-W` - Save buffer

`Space-Q` - Quit Vim

`Space-P` - Open project tree

`Ctrl-P` - Search project
