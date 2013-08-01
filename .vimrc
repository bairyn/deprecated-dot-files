" dvorak layout before any other key bindings
"set langmap='q,\\,w,.e,pr,yt,fy,gu,ci,ro,lp,/[,=],aa,os,ed,uf,ig,dh,hj,tk,nl,s\\;,-',\\;z,qx,jc,kv,xb,bn,mm,w\,,v.,z/,[-,]=,\"Q,<W,>E,PR,YT,FY,GU,CI,RO,LP,?{,+},AA,OS,ED,UF,IG,DH,HJ,TK,NL,S:,_\",:Z,QX,JC,KV,XB,BN,MM,W<,V>,Z?,[-,]=,{_,}+

set guifont=Consolas:h11

set tw=79

" Turns on highlighted search function. Simply type / followed by search and
" hit enter
set hlsearch

" Set to display line numbers, yay
set number

" Stops the word highlighting from sticking on longer then you need it to
set nohls
" Enables incremenetal search
set incsearch
" Highlights matching brackets while typing
set showmatch

" Set minimium window width to 0
set wmh=0

" enter spaces when tab is pressed:
"set expandtab
" do not break lines when line length increases
set textwidth=0
" user 4 spaces to represent a tab
set tabstop=4
set softtabstop=4
" number of space to use for auto indent
" you can use >> or << keys to indent current line or selection
" in normal mode.
set shiftwidth=4
" Copy indent from current line when starting a new line.
set autoindent
" makes backspace key more powerful.
set backspace=indent,eol,start
" shows the match while typing
set incsearch
" case insensitive search
set ignorecase
" show line and column number
set ruler
" show some autocomplete options in status bar
set wildmenu
" turns off the backup junk files that vim leaves. Files made whle Vim is open
" still stay
set nobackup

" Colors and syntax
"colorscheme ir_black
colorscheme wombat
set background=dark
syntax on

set ignorecase
set hidden
set laststatus=2

"imap <Nul> <Esc>n
"nmap <Nul> <Esc>
"vmap <Nul> <Esc>
"xmap <Nul> <Esc>
"smap <Nul> <Esc>
"omap <Nul> <Esc>
"lmap <Nul> <Esc>
"cmap <Nul> <Esc>

" Two escapes
" the below line works but the positioning is wrong; moved to below window
" mappings
inoremap <Nul> <Esc><Esc>l
"imap <Nul> <Esc><Esc>s
nmap <Nul> <Esc><Esc>
vmap <Nul> <Esc><Esc>
xmap <Nul> <Esc><Esc>
smap <Nul> <Esc><Esc>
omap <Nul> <Esc><Esc>
lmap <Nul> <Esc><Esc>
cmap <Nul> <Esc><Esc>

"" Lakitu7: toggle space
"" dvorak method
"imap <Nul> <Esc>n
"" dvorak method
"nmap <Nul> c
""imap <Nul> <Esc>l
""imap <Nul> <Esc>
""nmap <Nul> i
"" dvorak method
"""nnoremap <c-b> <c-n>
"""nnoremap <c-f> <c-y>
"nnoremap <c-t> Sbn<CR>
"" dvorak method
"nnoremap <c-h> Sbp<CR>
"" dvorak method
"nnoremap <c-p> <c-r>
"" not with dvorak
"nnoremap <c-k> :bn<Esc>
"" not with dvorak
"nnoremap <c-j> :bp<Esc>
set cursorline

" something changed?
"nnoremap <c-f> <c-f>
"nnoremap <c-b> <c-b>
"nnoremap <c-y> <c-y>
"nnoremap <c-d> <c-e>
"nnoremap <c-t> Sbn<CR>
"nnoremap <c-h> Sbp<CR>

" not without dvorak
"nnoremap j ih
" not without dvorak
"nnoremap k it
" not without dvorak
"vnoremap j ih
" not without dvorak
"vnoremap k it

" dvorak windowing
" for some reason, ctrl+w / , has issues in vim, so instead the key below it is used: s (litterally o in dvorak)
"nnoremap  <c-w>

" useful dvorak method of repeating commands on prev or next line with c-q and c-a
"nnoremap  tv
"nnoremap <c-a> hv
" but cursor position
"nnoremap  tv`/
"nnoremap <c-a> hv`/
"nnoremap ' tv
"nnoremap a hv

" dvorak CTRL-G
"nnoremap <c-i> <c-g>
"nnoremap <c-p> <c-r>

" always show tab line
set showtabline=2

" (dvorak) ctrl-d and ctrl-n (left and right) to go through tabs
"nnoremap <c-d> Stabp<Esc>
"nnoremap <c-n> Stabn<Esc>

" (dvorak) set CTRL+K to open new tab and CTRL+J to close tab
" on new tab, make sure no new buffers
"nnoremap <c-k> Stabnew<Esc>Sbn<Esc>Sbd #<Esc>
"nnoremap <c-j> Stabclose<Esc>

" ALT+1-9 for first 9 tabs, ALT-0 for last tab
"nnoremap 1 1iy
"nnoremap 2 2iy
"nnoremap 3 3iy
"nnoremap 4 4iy
"nnoremap 5 5iy
"nnoremap 6 6iy
"nnoremap 7 7iy
"nnoremap 8 8iy
"nnoremap 9 9iy
"nnoremap 0 Stablast<Esc>

set dictionary+=/usr/share/dict/words
set thesaurus+=/usr/share/thes/mthesaur.txt

" spell checking hilight
set spell



"Use TAB to complete when typing words, else inserts TABs as usual.
"Uses dictionary and source files to find matching words to complete.

"See help completion for source,
"Note: usual completion is on <C-n> but more trouble to press all the time.
"Never type the same word twice and maybe learn a new spellings!
"Use the Linux dictionary when spelling is in doubt.
"Window users can copy the file to their machine.
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
"inoremap <Tab> <c-r>]Tab_Or_Complete()<CR>
inoremap <Tab> <c-r>=Tab_Or_Complete()<CR>


"\. to repeat while keeping cursor in same spot
""nnoremap \. .`[
"nnoremap \. v`/









" minimal Dvorak remapping







" map dvorak movement keys and ``domino'' keys
noremap d h
noremap D H
"noremap h j
"noremap t k
" The capitol versions of these are already used for convenient purposes
noremap h gj
noremap t gk
noremap n l
noremap N L

" delete -> junk
noremap j d
noremap J D
"" substitute -> junk and replace
"noremap j s
"noremap j S
" join -> konnect
noremap k j
noremap K J
" next -> last search
noremap l n
noremap L N

" convenience stuff
noremap H 8gj
noremap T 8gk
noremap <m-h> j
noremap <m-t> k
"noremap - $
"noremap _ ^

" windows too
" Use ^U for moving through windows, and ^W for everything else
map <C-u>d <C-w>h
map <C-u>h <C-w>j
map <C-u>t <C-w>k
map <C-u>n <C-w>l
map <C-i>d <C-w>h
map <C-i>h <C-w>j
map <C-i>t <C-w>k
map <C-i>n <C-w>l
map <C-u><C-d> <C-w><C-h>
map <C-u><C-h> <C-w><C-j>
map <C-u><C-t> <C-w><C-k>
map <C-u><C-n> <C-w><C-l>

" for convenience, shortcut for < and > by ^, and ^.
"map <C-w><C-,> <C-w><
"map <C-w><C-.> <C-w>>
"bleh, doesn't work

"\. to repeat last command while keeping cursor in same spot
nnoremap \. .`[

" ALT+1-9 for first 9 tabs, ALT-0 for last tab
nnoremap 1 1gt
nnoremap 2 2gt
nnoremap 3 3gt
nnoremap 4 4gt
nnoremap 5 5gt
nnoremap 6 6gt
nnoremap 7 7gt
nnoremap 8 8gt
nnoremap 9 9gt
nnoremap 0 :tablast<Esc>

" CTRL+k/j to open and close cabs
nnoremap <c-k> :tabnew<Esc>:bn<Esc>:bd #<Esc>
nnoremap <c-m> :enew<Esc>
nnoremap <c-j> :tabclose<Esc>

nnoremap <c-d> :tabp<Esc>
nnoremap <c-n> :tabn<Esc>

nnoremap <c-h> :bp<Esc>
nnoremap <c-t> :bn<Esc>

nnoremap  k.`[
nnoremap <c-a> j.`[
nnoremap <c-u> j.`[
nnoremap ' k.
nnoremap a j.
nnoremap u j.

" ^x decrements; meta-x increments numbers increment number
nnoremap x <c-a>

" Minimum number of lines above or below the cursor
set scrolloff=12

set mouse=
set mousemodel=

" set visualbell ONLY if terminal has bell enabled since beeping in vim is
" pretty much redundant.  Otherwise comment this next line out since visual bells
" cause a delay in vim.
"set visualbell

set cindent
set nosmartindent
set noautoindent

" Read
nnoremap <F3> :r ~/.vim/write_tmp<Esc>

" Write
"vmap <F4> :'<,'>w ~/.vim/tmp<Esc>  " '<,'> is inserted automatically
vmap <F4> :w! ~/.vim/write_tmp<Esc>
"nmap <F4> :.w! ~/.vim/write_tmp<Esc>
nmap <F4> :w! ~/.vim/write_tmp<Esc>

set vb t_vb=

set nocp
filetype plugin on

nm <c-p> :se invpaste paste?<CR>

" ex command for toggling hex mode - define mapping if desired
command! -bar Hexmode call ToggleHex()

" helper function to toggle hex mode
function! ToggleHex()
  " hex mode should be considered a read-only operation
  " save values for modified and read-only for restoration later,
  " and clear the read-only flag for now
  let l:modified=&mod
  let l:oldreadonly=&readonly
  let &readonly=0
  let l:oldmodifiable=&modifiable
  let &modifiable=1
  if !exists("b:editHex") || !b:editHex
    " save old options
    let b:oldft=&ft
    let b:oldbin=&bin
    " set new options
    setlocal binary " make sure it overrides any textwidth, etc.
    let &ft="xxd"
    " set status
    let b:editHex=1
    " switch to hex editor
    "%!xxd -c 16
    %!xxd -c 40
  else
    " restore old options
    let &ft=b:oldft
    if !b:oldbin
      setlocal nobinary
    endif
    " set status
    let b:editHex=0
    " return to normal editing
    "%!xxd -r -c 16
    %!xxd -r -c 40
  endif
  " restore values for modified and read only state
  let &mod=l:modified
  let &readonly=l:oldreadonly
  let &modifiable=l:oldmodifiable
endfunction

"nnoremap <C-m> :Hexmode<CR>
"inoremap <C-m> <Esc>:Hexmode<CR>
"vnoremap <C-m> :<C-U>Hexmode<CR>
"nnoremap <C-semicolon> :Hexmode<CR>
"inoremap <C-semicolon> <Esc>:Hexmode<CR>
"vnoremap <C-semicolon> :<C-U>Hexmode<CR>
"nnoremap <C-;> <c-r>=ToggleHex()<CR>
"inoremap <C-;> <c-r>=ToggleHex()<CR>
"vnoremap <C-;> <c-r>=ToggleHex()<CR>

" vim -b : edit binary using xxd-format!
"augroup Binary
  "au!
"
  "au BufReadPre  *.bin let &bin=1
  "au BufReadPost *.bin if &bin | %!xxd
  "au BufReadPost *.bin set ft=xxd | endif
  "au BufWritePre *.bin if &bin | %!xxd -r
  "au BufWritePre *.bin endif
  "au BufWritePost *.bin if &bin | %!xxd
  "au BufWritePost *.bin set nomod | endif
"
  "au BufReadPre  *.x86 let &bin=1
  "au BufReadPost *.x86 if &bin | %!xxd
  "au BufReadPost *.x86 set ft=xxd | endif
  "au BufWritePre *.x86 if &bin | %!xxd -r
  "au BufWritePre *.x86 endif
  "au BufWritePost *.x86 if &bin | %!xxd
  "au BufWritePost *.x86 set nomod | endif
"
  "au BufReadPre  *.so let &bin=1
  "au BufReadPost *.so if &bin | %!xxd
  "au BufReadPost *.so set ft=xxd | endif
  "au BufWritePre *.so if &bin | %!xxd -r
  "au BufWritePre *.so endif
  "au BufWritePost *.so if &bin | %!xxd
  "au BufWritePost *.so set nomod | endif
"
  "au BufReadPre  *.o let &bin=1
  "au BufReadPost *.o if &bin | %!xxd
  "au BufReadPost *.o set ft=xxd | endif
  "au BufWritePre *.o if &bin | %!xxd -r
  "au BufWritePre *.o endif
  "au BufWritePost *.o if &bin | %!xxd
  "au BufWritePost *.o set nomod | endif
"augroup END
augroup Binary
  au!
 
  au BufReadPre  *.bin,*.x86,*.so,*.o  let &bin=1
  au BufReadPost *.bin,*.x86,*.so,*.o  if &bin | exe '%!xxd' | set ft=xxd | endif
  au BufWritePre *.bin,*.x86,*.so,*.o  if &bin | exe '%!xxd -r' | endif
  au BufWritePost *.bin,*.x86,*.so,*.o if &bin | exe '%!xxd' | set nomod | endif
augroup END

function! Asm()
	set noexpandtab ts=16 sw=16 nocindent nosmartindent autoindent
	map <Tab> <Nop>
endfunction

if !exists("autocommands_fileTypes_loaded")
    let autocommands_fileTypes_loaded = 1
    " for Haskell and python source files, set expand on
    au FileType python,haskell set expandtab ts=4 sw=4 nocindent nosmartindent autoindent
    au FileType asm call Asm()
endif

vmap <F2> :g/^/m 0<Esc>

set tabpagemax=64


" Haskell

" use ghc functionality for haskell files
"au Bufenter *.hs compiler ghc

"" switch on syntax highlighting
"set syntax on
syntax on

"" enable filetype detection, plus loading of filetype plugins
"set filetype plugin on
filetype plugin on

"au! Bufenter *.hs set filetype=haskell
au! BufEnter,BufNewFile,BufReadPre,FileReadPre *.hsc set filetype=haskell
"au! BufEnter,BufNewFile,BufReadPre,FileReadPre *.hsc compiler ghc
au! BufEnter,BufNewFile,BufReadPre,FileReadPre *.hs set filetype=haskell
"au! BufEnter,BufNewFile,BufReadPre,FileReadPre *.hs compiler ghc

" configure browser for haskell_doc.vim
let g:haddock_browser = "/usr/bin/firefox"
"let g:haddock_browser = "C:/Program Files/Opera/Opera.exe"
"let g:haddock_browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
"let g:haddock_browser = "C:/Program Files/Internet Explorer/IEXPLORE.exe"

" avoid hit-enter prompts
"set cmdheight=3

set cmdheight=1

map - ^
map _ $
map __ $




command! -nargs=1 Silent
\ | execute ':silent !'.<q-args>
\ | execute ':redraw!'

set t_Co=256


"colorscheme aqua
"colorscheme fruity
"colorscheme wood

"colorscheme dante
"colorscheme relaxedgreen
"colorscheme wombat
"colorscheme matrix
"colorscheme bairyn
colorscheme wombat
set background=dark

"highlight Pmenu ctermbg=238 gui=bold

" To disable a plugin, add it's bundle name to the following list
"let g:pathogen_disabled = []
let g:pathogen_disabled = []

" for some reason the csscolor plugin is very slow when run on the terminal
" but not in GVim, so disable it if no GUI is running
if !has('gui_running')
    call add(g:pathogen_disabled, 'csscolor')
endif

" Gundo requires at least vim 7.3
if v:version < '703' || !has('python')
    call add(g:pathogen_disabled, 'gundo')
endif

if v:version < '702'
    call add(g:pathogen_disabled, 'autocomplpop')
    call add(g:pathogen_disabled, 'fuzzyfinder')
    call add(g:pathogen_disabled, 'l9')
endif

"call pathogen#infect()

"hdevtools Haskell vim
"au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
"au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>

au BufWritePost *.hs silent !fast-tags -o tags % &
"Problems with indentation / comments?  http://stackoverflow.com/questions/2360249/vim-automatically-removes-indentation-on-python-comments 
set nosmartindent
set cindent

function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

nnoremap <C-g> :call NumberToggle()<cr>

unmap <ENTER>

set tw=79

" # indentation
:inoremap # X<BS>#
set cinkeys-=0#
set indentkeys-=0#

nnoremap <ESC>h <C-w>j
nnoremap <ESC>t <C-w>k
nnoremap <ESC>d <C-w>h
nnoremap <ESC>n <C-w>l
nnoremap <ESC>s <C-w>s
nnoremap <ESC>v <C-w>v
nnoremap <ESC>q <C-w>q
nnoremap <ESC>+ <C-w>+
nnoremap <ESC>- <C-w>-

" See line 74
"imap <Nul> <Esc><Esc>n
