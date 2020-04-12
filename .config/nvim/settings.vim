filetype plugin indent on
syntax on

set number
set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set tabstop=2
set shiftwidth=2
set expandtab
set background=dark
set termguicolors

" Assume that *.tex files are LaTeX.
let g:tex_flavor = 'latex'

" Lilypond support.
" FIXME: auto-detect version
set runtimepath+=/usr/share/lilypond/2.20.0/vim/

" Gruvbox colour scheme.
let g:gruvbox_italic = 1
colorscheme gruvbox


