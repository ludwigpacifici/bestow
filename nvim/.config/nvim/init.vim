set nocompatible

syntax enable
filetype indent plugin on

set autoindent
set autoread
set background=dark
set backspace=indent,eol,start
set browsedir=buffer
set completeopt=longest,menu,preview
set confirm
set cursorline
set encoding=utf-8
set expandtab
set fileencoding=utf-8
set fileencodings=utf-8
set gdefault
set hidden
set history=2048
set hlsearch
set ignorecase
set incsearch
set mouse=a
set nonumber
set nospell
set nostartofline
set notitle
set nowrap
set ruler
set shiftround
set shiftwidth=4
set showcmd
set showmatch
set showmode
set smartcase
set softtabstop=4
set tabstop=4
set visualbell
set wildmenu
set wildmode=list:longest,full

"Remove trailing whitespaces on save
autocmd BufWritePre * :%s/\s\+$//e

call plug#begin('~/.vim/plugged')
Plug 'altercation/vim-colors-solarized'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
call plug#end()

let g:solarized_bold=1
let g:solarized_underline=0
let g:solarized_italic=1
colorscheme solarized
