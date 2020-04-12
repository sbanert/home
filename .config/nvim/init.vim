" Exclude vi mode
set nocompatible

runtime plugins.vim
runtime settings.vim

let g:polyglot_disabled = ['latex']
filetype off
let mapleader = "<space>"
let maplocalleader = ","


let g:vimtex_enabled = 1
if executable('mupdf')
  let g:vimtex_view_method = 'mupdf'
endif


" Start Nerdtree when Vim starts with no files specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Start Nerdtree when Vim starts up opening a directory
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif

" Open/close Nerdtree with Ctrl+n.
map <C-n> :NERDTreeToggle<CR>

" Set SML/NJ executable.
let g:sml_smlnj_executable = '/usr/bin/smlnj'
