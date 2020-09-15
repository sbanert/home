filetype plugin indent on
syntax on

set cmdheight=2
set expandtab
set hidden
set hlsearch
set incsearch
set mouse=a
set nobackup
set nowritebackup
set number
set ruler
set showcmd
set shiftwidth=2
set tabstop=2
set termguicolors
set updatetime=300

hi clear SignColumn

" Assume that *.tex files are LaTeX.
let g:tex_flavor = 'latex'

" Lilypond support.
" FIXME: auto-detect version
set runtimepath+=/usr/share/lilypond/2.20.0/vim/

" Gruvbox colour scheme.
set background=dark
let g:gruvbox_italic = 1
colorscheme gruvbox
" colorscheme nord


