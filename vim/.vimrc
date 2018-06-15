syntax on

" Custom highlighting.
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd ColorScheme * highlight Nbsp ctermbg=219 guibg=#ffafff

if has('termguicolors')
	set termguicolors
	" set Vim-specific sequences for RGB colors
	let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
	let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
	" Color scheme.
	colorscheme base16-default-dark
endif

" Show trailing whitespace.
autocmd Syntax * syn match ExtraWhitespace /\s\+$/ containedin=ALL
" Highlight column 79.
set colorcolumn=79
" Highlight non-breaking spaces.
autocmd Syntax * syn match Nbsp / / containedin=ALL

augroup filetypedetect
	autocmd BufRead,BufNewFile *mutt-* setfiletype mail
augroup end
