set nocompatible     " Exclude vi mode

" Download and install vim-plug if not already installed.
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-plug'              " Plugin manager
Plug 'tpope/vim-sensible'             " Sensible defaults
Plug 'tpope/vim-fugitive'             " Git support
Plug 'sheerun/vim-polyglot'           " Support for nix, lilypond, julia etc.
Plug 'tpope/vim-surround'             " Delimiters
Plug 'tpope/vim-commentary'           " Comment handling
Plug 'ntpeters/vim-better-whitespace' " Mark trailing whitespace
Plug 'ludovicchabant/vim-gutentags'   " Collect tags for the project
Plug 'lervag/vimtex'                  " LaTeX support
Plug 'neoclide/coc-vimtex'            " LaTeX autocompletion
Plug 'scrooloose/syntastic'           " Syntax checking
Plug 'morhetz/gruvbox'                " Colour scheme
Plug 'vim-airline/vim-airline'        " Status bar
Plug 'scrooloose/nerdtree'            " Tree explorer
Plug 'airblade/vim-gitgutter'         " diffs at the margin
Plug 'neoclide/coc.nvim', {'branch': 'release'} " autocomplete
call plug#end()
filetype plugin indent on

set number

let g:polyglot_disabled = ['latex']
filetype off
set runtimepath+=${pkgs.lilypond-unstable.outPath}/share/lilypond/${pkgs.lilypond-unstable.version}/vim/
filetype plugin indent on
syntax on
let mapleader = "<space>"
let maplocalleader = ","

set tabstop=2
set shiftwidth=2
set expandtab
set background=dark
set termguicolors

let g:tex_flavor = 'latex'
let g:vimtex_enabled = 1

let g:gruvbox_italic = 1
colorscheme gruvbox

" Start Nerdtree when Vim starts with no files specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Start Nerdtree when Vim starts up opening a directory
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif

" Open/close Nerdtree with Ctrl+n.
map <C-n> :NERDTreeToggle<CR>
