set termguicolors
" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" Custom highlighting.
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
" Color scheme.
colorscheme base16-default-dark
" Show trailing whitespace.
match ExtraWhitespace /\s\+$/
" Highlight column 79.
set colorcolumn=79
