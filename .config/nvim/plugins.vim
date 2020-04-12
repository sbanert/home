" Download and install vim-plug if not already installed.
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" General plugins
Plug 'junegunn/vim-plug'              " Plugin manager
Plug 'tpope/vim-sensible'             " Sensible defaults

" Git support
Plug 'tpope/vim-fugitive'             " Git support
Plug 'airblade/vim-gitgutter'         " diffs at the margin

" LSP support
Plug 'neoclide/coc.nvim', {'branch': 'release'} " autocomplete
Plug 'neoclide/coc-vimtex'            " LaTeX autocompletion
Plug 'dense-analysis/ale'             " Asynchronous linting


Plug 'sheerun/vim-polyglot'           " Support for nix, lilypond, julia etc.
Plug 'tpope/vim-surround'             " Delimiters
Plug 'tpope/vim-commentary'           " Comment handling
Plug 'ntpeters/vim-better-whitespace' " Mark trailing whitespace
Plug 'ludovicchabant/vim-gutentags'   " Collect tags for the project
Plug 'lervag/vimtex'                  " LaTeX support
Plug 'scrooloose/syntastic'           " Syntax checking
Plug 'morhetz/gruvbox'                " Colour scheme
Plug 'vim-airline/vim-airline'        " Status bar
Plug 'scrooloose/nerdtree'            " Tree explorer
Plug 'jez/vim-better-sml'             " Standard ML support
call plug#end()
