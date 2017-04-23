## Keybindings
While the default vim is amazing out of the box, I have created some custom keybindings and
used vim plugins to help improve support for particular languages, plugins include:

- Ale for linter support
- VimCompletesMe for simple context based completion
- Surround for editing regions
- NerdTree for project tree support
- Ultisnips for snippet support
- CtrlP for project searching (I'd recomment FZF if you dont want gui support)
- Fugitive for Git support

I also use solarized as my goto colour scheme and for good reason.

Languages that I included support and is subject to change regularly:
- Javascript (vim-javascript, tern, eslint)

## Versions
I use vim 8. Neovim is a great project, but I prefer not to fragment until it provides significant value

## Notable Keybindings
Keybindings can be found in the `.vimrc` file, but the notable ones are here:

`Space` - Leader

```
" Project management
nmap <leader>pf :CtrlP<CR>
nmap <leader>ft :NERDTreeFind<cr>
nmap <leader>pt :NERDTreeCWD<cr>
nmap <leader>pp :source ~/Dropbox/sessions/
nmap <leader>ps :mksession! ~/Dropbox/sessions/

" Git/Fugitive keybindings
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gf :Gfetch<cr>
nnoremap <leader>gF :!git pull --rebase<cr>
nnoremap <leader>gP :Gpush
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gC :Git checkout

" Window bindings
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l
nnoremap <leader>wo <C-w>o
nnoremap <leader>wc <C-w>c
nnoremap <leader>ws <C-w>s
nnoremap <leader>wv <C-w>v

" Buffer bindings
nmap <leader>d :bd!<CR>
nmap <leader>bd :bp<bar>sp<bar>bn<bar>bd<CR>
nmap <leader>bb :CtrlPBuffer<CR>
nmap <leader>bn :bn!<CR>
nmap <leader>bp :bp!<CR>

nmap <leader>qq :wqa!<CR>
nmap <leader><Esc> :q!<CR>
nmap <leader>/ :noh<CR>

nmap <leader>ff :edit 
nmap <leader>fs :w!<CR>
nmap <leader>feR :source ~/.vimrc
nmap <leader>fed :edit ~/.vimrc<CR>

" Tab Management
nnoremap <leader>tn :tabn<cr>
nnoremap <leader>tp :tabp<cr>

```
