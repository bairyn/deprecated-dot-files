" Type zR to open all folds in this file.
" Type za to toggle an individual fold.
" Type zM to close all folds in this file.

" Appearance. "{{{
" ----------------------------------------------------------------

"" Use the solarized color scheme, which requires the console to be configured
"" for solarized.
syntax enable
if has('gui_running')
	set background=light
else
	set background=dark
endif
colorscheme solarized

"" Display line numbers.
set number

"" Display whitespace characters.
"" I also like the configuration in http://vi.stackexchange.com/a/430 .
"" For space, add e.g. "space:\ ".
"set list listchars=tab:▸·,trail:🆇,extends:⇚,precedes:⇛
" Comments need to be at the bottom of this list.
execute 'set list listchars'
	\ .'=extends:↣'
	\ .',precedes:↢'
	\ .',conceal:*'
	\ .',nbsp: '
	\ .',trail:🆇'
	\ .',tab:‣·'
"	\ .',tab:▏·'
"	\ .',tab:»·'
"	\ .',eol:▏'
"	\ .'.tab:▸·'
"	\ .',trail:◦'
"	\ .',extends:⇚'
"	\ .',precedes:⇛'

"" Always display the status bar, even if there is only one window.
set laststatus=2

"" TODO: highlight trailing whitespace.

"" Briefly show the position of a matching opening brace-like symbol when the
"" closing brace-like symbol is written.
set showmatch

"" Show the current line and column number.
set ruler

"" Enable spell checking.
set spell

"" Always show tab line, even if there is only one tab.
set showtabline=2

" https://stackoverflow.com/a/15124717
" vim: set tmux windows.
let g:tmux_original_window_name = system("tmux display-message -p \"#W\"")[:-2]
let g:tmux_window_max_length = 15
function! TmuxRenameWindow(name)
	if len(a:name) <= 0
		let l:name2 = "vim"
	else
		let l:name2 = a:name
	endif

	if g:tmux_window_max_length >= 1 && len(l:name2) <= g:tmux_window_max_length - 1
		let l:truncated_window_name = l:name2
	else
		"let l:truncated_window_name = "…" .  shellescape(l:name2)[0:(g:tmux_window_max_length - 2)]
		let l:truncated_window_name = "…" .  l:name2[(-(g:tmux_window_max_length - 2)):]
	endif
	" automatic-rename is disabled after rename-window is called, so the
	" set-window-option is not needed.
	call system("tmux set-window-option automatic-rename on")
	call system("tmux rename-window " . shellescape(l:truncated_window_name))
endfunction

function! TmuxRenameWindowRestore(tmux_original_window_name)
	" (The first call is not needed.)
	call system("tmux set-window-option automatic-rename on")
	call system("tmux rename-window " . shellescape(a:tmux_original_window_name))
	call system("tmux set-window-option automatic-rename on")
endfunction

augroup tmux
	autocmd!
	"autocmd BufReadPost,FileReadPost,BufNewFile * call system("tmux rename-window " . expand("%"))
	autocmd BufEnter * call TmuxRenameWindow(expand("%"))
	autocmd VimLeave * call TmuxRenameWindowRestore(g:tmux_original_window_name)
augroup END
" }}}

" Indentation and formatting. "{{{
" ----------------------------------------------------------------
"
" This section includes default indentation settings.

" Respect files' vim settings.
set modeline

"" noexpandtab: Don't replace tabs with spaces.
set noet
"" Tabs for indentation, and spaces for alignment.
set ts=4
set sw=4
set sr

"" Wrap lines beyond by default 79 characters.
set wrap
set tw=79

"" If cindent is on, smartindent is ignored.
set nocindent
"" Normally, if smartindent is enabled, then autoindent should also be enabled.
"set smartindent autoindent
" Disable smartindent, so that "#" does not remove all indentation.
set nosmartindent autoindent

"" TODO: document.
set formatoptions=croqtl

"" Thanks, `http://peox.net/articles/vimconfig.html`!
"" Prevent the filetype plugin from overriding certain settings.  (You can
"" verify what the source of a setting is with `:verbose set formatoptions?`)
filetype indent plugin off

"" When opening yaml files, use different indentation and formatting settings.
"" Thanks, https://stackoverflow.com/a/469576
""
"" TODO: this should be integrate with the filetype plugin, as described in
"" `https://stackoverflow.com/a/475167`.
autocmd BufRead,BufNewFile *.yaml setlocal formatoptions=croqtl et ts=2 sts=2 sw=2 sr
" }}}

" Tabs, windows and navigation. "{{{
" ----------------------------------------------------------------

" Note:
" 	3 pairs of keys are configured here, one for tab movement, one for
" 	buffer movement, and another for opening and closing tabs.
"
" 	I wanted the movement keys to be mapped to the arrow keys with conttrol,
" 	horizontally for tabs, and vertically for buffers (c-j, c-k, c-h, c-l), but
" 	c-l is already used.  Perhaps I may find another key for what was
" 	originally c-l, and then two more c-X keys for opening and closing tabs
" 	that are suitable chosen.
"
" TODO: ^^^ Implement what I noted here.  Until then, the mapping will be a bit
"       arbitrary.

" Don't limit tabs to the first 10 files.
set tabpagemax=10000

" M-h and M-t navigates buffers.
nnoremap <C-h> :bn!<CR>
nnoremap <C-t> :bp!<CR>
"nnoremap <M-j> :bn!<CR>
"nnoremap <M-k> :bp!<CR>
"nnoremap <Esc>j :bn!<CR>
"nnoremap <Esc>k :bp!<CR>

" C-d and C-n moves tabs (dvorak).
" TODO: with a count, gt is absolute, not relative, which is inconsistent with
"       the mapping for gT.  Make count with <c-d> relative.
nnoremap <C-d> gT
nnoremap <C-n> gt

" C-k and C-j to open and close tabs.
nnoremap <c-k> :tabnew<cr>:bn<cr>:bd #<cr>
nnoremap <c-j> :tabclose<cr>

"" Don't unload non-displayed buffers.
set hidden

"" Increase default number of tabs (10) that are loaded with `vim -p`.
"" c.f. https://unix.stackexchange.com/questions/30665/gvim-p-limit-of-opened-tabs
set tabpagemax=100
" }}}

" Commands. "{{{
"" Enhance autocompletion.
set wildmenu
" }}}

" Movement. "{{{
"" Minimum number of lines present above or below the cursor.
set scrolloff=8

"" Allow the cursor to move one column beyond the last character in a line.
set virtualedit=onemore,block,insert

"" Control-space exits insert mode.
"" <C-space> doesn't seem to work with vim.
inoremap <C-@> <ESC>l
" }}}

" Search settings. "{{{
" ----------------------------------------------------------------
"" Preview new cursor position while typing a search query.
set incsearch
"" Don't keep the last search highlighted.
set nohlsearch
"" Configure searches to be case-insensitive by default.
set ignorecase
" Search parent directories for tag files.
set tags=tags;
" }}}

" vim:foldmethod=marker:foldlevel=0 tw=79 noet
