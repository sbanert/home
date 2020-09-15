" Download and install vim-plug if not already installed.
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" General plugins
Plug 'junegunn/vim-plug'              " Plugin manager
Plug 'tpope/vim-sensible'             " Sensible defaults
Plug 'tpope/vim-surround'             " Delimiters
Plug 'tpope/vim-commentary'           " Comment handling
Plug 'ntpeters/vim-better-whitespace' " Mark trailing whitespace
Plug 'scrooloose/nerdtree'            " Tree explorer
Plug 'ryanoasis/vim-devicons'         " Icons for Nerdtree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight' " Colors
Plug 'liuchengxu/vim-which-key'       " Key descriptions

" Git support
Plug 'tpope/vim-fugitive'             " Git support
Plug 'airblade/vim-gitgutter'         " diffs at the margin

" LSP support
Plug 'neoclide/coc.nvim', {'branch': 'release'} " autocomplete
" Plug 'neoclide/coc-vimtex'            " LaTeX autocompletion
Plug 'dense-analysis/ale'             " Asynchronous linting

" Language specific support if no LSP is available.
Plug 'jez/vim-better-sml'             " Standard ML support
Plug 'lervag/vimtex'                  " Additional LaTeX support
Plug 'wlangstroth/vim-racket'         " Racket support
Plug 'JuliaEditorSupport/julia-vim'   " Julia support

" Appearance
Plug 'vim-airline/vim-airline'        " Status bar
Plug 'vim-airline/vim-airline-themes' " Themes for airline
Plug 'morhetz/gruvbox'                " Colour scheme
" Plug 'arcticicestudio/nord-vim'       " Colour scheme

" Plug 'sheerun/vim-polyglot'           " Support for nix, lilypond, julia etc.
" Plug 'ludovicchabant/vim-gutentags'   " Collect tags for the project
" Plug 'scrooloose/syntastic'           " Syntax checking
call plug#end()
