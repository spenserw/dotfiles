set dir=/tmp " put all .sw* files in /tmp

set nocompatible              " be iMproved, required
filetype off                  " required

set backspace=indent,eol,start

set t_Co=256

set incsearch
set hls
set nowrap " can't believe I just found you <3

set encoding=utf-8

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'Valloric/MatchTagAlways'
Plugin 'tpope/vim-commentary'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'
Plugin 'ctrlpvim/ctrlp.vim'

" Add for vim-airline
 set laststatus=2

call vundle#end()

set runtimepath^=~/.vim/bundle/ctrlp.vim

filetype plugin on
set tabstop=2
set shiftwidth=2
set expandtab
set autoindent

autocmd Filetype go setlocal ts=4 sw=4 sts=0 expandtab
autocmd Filetype cpp setlocal ts=4 sw=4 sts=4 expandtab

syntax enable
" Add for jinja2 files (LiveStories)
au BufRead,BufNewFile *.jinja2 setfiletype html
au BufRead,BufNewFile *.tpl setfiletype html

" Add for always match tags
let g:mta_filetypes = { 'html' : 1, 'xhtml' : 1, 'xml' : 1, 'jinja' : 1, 'smarty': 1 }

nnoremap <C-f> <esc>
inoremap <C-f> <esc>

nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>

" Unbind this guy because he seems to crash XTerm...
nnoremap <C-s> <esc>

" Writing mode
nnoremap <C-z> :setlocal spell spelllang=en_us<CR>:Goyo 100<CR>:Limelight<CR>:setlocal wrap<CR>:set formatoptions=l<CR>:set lbr<CR>

nnoremap <C-e> :ALENextWrap<CR>

filetype plugin indent on

" Disable on Thinkpad
"set background=dark
"let g:solarized_visibility = "high"
"let g:solarized_contrast = "high"
colorscheme desert

" Add for Limelight
let g:limelight_conceal_ctermfg = 'gray'

" Added for ALE
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'cpp': ['g++'],
\}
let g:ale_cpp_gcc_options='-std=c++17 -Wall -Ithirdparty/'
let g:airline#extensions#ale#enabled = 1
let g:ale_lint_on_enter = 1
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
highlight ALEError ctermbg=Red

" Added for vim-markdown
let g:vim_markdown_folding_level = 6
let g:vim_markdown_folding_style_pythonic = 1

set wildignore+=*.o
