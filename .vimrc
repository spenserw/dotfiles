set dir=/tmp " put all .sw* files in /tmp

set nocompatible              " be iMproved, required
filetype off                  " required

set backspace=indent,eol,start

set t_Co=256

set incsearch
set hls
set nowrap " can't believe I just found you <3

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" " alternatively, pass a path where Vundle should install plugins
" "call vundle#begin('~/some/path/here')
"
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'Valloric/MatchTagAlways'
Plugin 'tpope/vim-commentary'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'heavenshell/vim-jsdoc'
Plugin 'w0rp/ale'
Plugin 'pangloss/vim-javascript'
Plugin 'altercation/vim-colors-solarized'

" Add for vim-airline
" set laststatus=2

call vundle#end()

set runtimepath^=~/.vim/bundle/ctrlp.vim

filetype plugin on
set tabstop=2
set shiftwidth=2
set expandtab
set autoindent

autocmd Filetype go setlocal ts=4 sw=4 sts=0 expandtab

syntax enable
" Add for jinja2 files (LiveStories)
au BufRead,BufNewFile *.jinja2 setfiletype html
au BufRead,BufNewFile *.tpl setfiletype html

" Fix weird file browser behavior (must be as same dir as file??)
autocmd BufEnter * cd %:p:h

" Add for always match tags
let g:mta_filetypes = { 'html' : 1, 'xhtml' : 1, 'xml' : 1, 'jinja' : 1, 'smarty': 1 }

" Add for vim-jsdoc
let g:jsdoc_allow_input_prompt = 1

nnoremap <C-f> <esc>
inoremap <C-f> <esc>

nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>

" Unbind this guy because he seems to crash XTerm...
nnoremap <C-s> <esc>

nnoremap <C-e> :ALENextWrap<CR>

filetype plugin indent on

"set background=dark
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"
colorscheme solarized

" Added for ALE
let g:ale_linters = {
\   'javascript': ['eslint'],
\}
let g:airline#extensions#ale#enabled = 1
let g:ale_lint_on_enter = 1
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
