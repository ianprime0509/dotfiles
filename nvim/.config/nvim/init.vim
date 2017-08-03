call plug#begin('~/.local/share/nvim/plugged')

Plug 'chriskempson/base16-vim'
Plug 'fatih/vim-go'
Plug 'rhysd/vim-clang-format'
Plug 'scrooloose/nerdcommenter'
Plug 'Shougo/deoplete.nvim'
Plug 'w0rp/ale'
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-go', {'do': 'make'}

call plug#end()

" Basic
let mapleader = "\<Space>"
nnoremap <leader><space> :noh<return>

" Visuals
set termguicolors
colorscheme base16-mocha
set number

" Indentation
set noexpandtab
set shiftwidth=8
set tabstop=8

" Autocompletion (Deoplete)
let g:deoplete#enable_at_startup = 1
" Let tab key cycle through options
inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
" Disable preview window
set completeopt-=preview

" Go
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1

" C/C++
let g:clang_format#auto_format = 1
let g:deoplete#sources#clang#libclang_path = "/lib/libclang.so"
let g:deoplete#sources#clang#clang_header = "/lib/clang/4.0.1/include"
let g:ale_c_clang_options = "-std=c99 -pedantic -Wall -Wextra"
au BufNewFile,BufRead *.tpp set filetype=cpp
