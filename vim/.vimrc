" Custom highlighting.
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

if has('termguicolors')
	set termguicolors
	" set Vim-specific sequences for RGB colors
	let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
	let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
	" Color scheme.
	colorscheme base16-default-dark
endif

" Show trailing whitespace.
match ExtraWhitespace /\s\+$/
" Highlight column 79.
set colorcolumn=79

augroup filetypedetect
	autocmd BufRead,BufNewFile *mutt-* setfiletype mail
augroup end

" Load .vimrc files in directories.
set secure exrc
