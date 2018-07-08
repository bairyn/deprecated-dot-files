" dvorak layout before any other key bindings
"set langmap='q,\\,w,.e,pr,yt,fy,gu,ci,ro,lp,/[,=],aa,os,ed,uf,ig,dh,hj,tk,nl,s\\;,-',\\;z,qx,jc,kv,xb,bn,mm,w\,,v.,z/,[-,]=,\"Q,<W,>E,PR,YT,FY,GU,CI,RO,LP,?{,+},AA,OS,ED,UF,IG,DH,HJ,TK,NL,S:,_\",:Z,QX,JC,KV,XB,BN,MM,W<,V>,Z?,[-,]=,{_,}+

" maybe: empty list for not.  singleton list for present.
" output is passed in singleton list; all output is maybe-optional.
:function! PartialArgs(out_partial, out_positional, out_positional_copy, out_positional_deepcopy, out_type, func, min_argc, maybe_range_argc, maybe_dict, argc, argv)
:	if len(a:maybe_range_argc) && a:maybe_range_argc[0] >= 0 && a:argc > a:min_argc + a:maybe_range_argc[0]
:		throw
\			printf
\				( '%s: Sorry, invalid number of arguments exceeds [%d, %d]: ''%d'' is too many.'
\				, string(func), a:min_argc, a:min_argc + a:maybe_range_argc[0], argc
\				)
:	elseif a:argc < min_argc
:		let l:func = a:func
:		let l:argv = a:argv
:
:		let l:anonymous = {}
:		if len(a:maybe_dict)
:			let l:dict = a:maybe_dict[0]
:
:			function l:anonymous.partial(...)
:				return call(l:func, l:argv, l:dict)
:			endfunction
:		else
:			function l:anonymous.partial(...)
:				return call(l:func, l:argv)
:			endfunction
:		endif
:
:		if len(a:out_partial)
:			let a:out_partial[0] = l:anonymous.partial
:		endif
:
:		return 1
:	else
:		if len(a:out_positional)
:			let a:out_positional[0] = a:argv
:		endif
:		if len(a:out_positional_copy)
:			let a:out_positional[0] = copy(a:argv)
:		endif
:		if len(a:out_positional_deepcopy)
:			let a:out_positional[0] = deepcopy(a:argv)
:		endif
:
:		if len(a:out_type)
:			let l:otype = a:out_type[0]
:
:			for i in a:argc
:				let l:type = type(a:argv[i])
:
:				if !has_key(l:otype l:type)
:					l:o_type[l:type]  = [i]
:				else
:					l:o_type[l:type] += [i]
:				endif
:			endfor
:		endif
:
:		return 0
:	endif
:endfunction

:function! Exec_PartialArgs(func, min_argc, ...)
:	return join(
\		[ ':if !exists(''l:partial'')'
\		, ':	let l:partial    = []'
\		, ':endif'
\		, ':if !exists(''l:positional'')'
\		, ':	let l:positional = []'
\		, ':endif'
\		, ':if !exists(''l:positional_copy'')'
\		, ':	let l:positional_copy= []'
\		, ':endif'
\		, ':if !exists(''l:positional_deepcopy'')'
\		, ':	let l:positional_deepcopy= []'
\		, ':endif'
\		, ':if !exists(''l:type'')'
\		, ':	let l:type       = []'
\		, ':endif'
\		, ':'
\		, ':if !exists(''l:p_partial'')'
\		, ':	let l:p_partial    = [l:partial]'
\		, ':endif'
\		, ':if !exists(''l:p_positional'')'
\		, ':	let l:p_positional = [l:positional]'
\		, ':endif'
\		, ':if !exists(''l:p_positional_copy'')'
\		, ':	let l:p_positional_copy= [l:positional_copy]'
\		, ':endif'
\		, ':if !exists(''l:p_positional_deepcopy'')'
\		, ':	let l:p_positional_deepcopy= [l:positional_deepcopy]'
\		, ':endif'
\		, ':if !exists(''l:p_type'')'
\		, ':	let l:p_type       = [l:type]'
\		, ':endif'
\		, ''
\		, ':if call'
\		, '\	( function(''PartialArgs'')'
\		, '\	, [ l:p_partial'
\		, '\	  , l:p_positional'
\		, '\	  , l:p_positional_copy'
\		, '\	  , l:p_positional_deepcopy'
\		, '\	  , l:p_type'
\		, '\	  , ' . a:func
\		, '\	  , ' . a:min_argc
\		, '\	  , ' . eval(Eval_MaybeExists(a:1))
\		, '\	  , eval(Eval_MaybeExists(''self''))
\		, '\	  , a:0'
\		, '\	  , a:000'
\		, '\	]' . (a:0 < 2 ? '' : ( ' + ' . string(a:000[1:]) ))
\		, '\	)'
\		, ':	return l:partial[0]'
\		, ':endif'
\		]
\	, "\n")
:endfunction

:function! Exp_MaybeExists(variable)
:	return printf('exists(%s) ? [%s] : []', string(variable), variable)
:endfunction

:function! RequireCallGlobals(...)
:	if !exists('g:call')
:		let g:call = {}
:	endif
:	if !exists('g:env')
:		let g:dict = {}
:	endif
:
:	return !a:0
:endfunction
:call RequireCallGlobals()

:function! Call(func, args, ...)
:	if RequireCallGlobals(a:000)
:		return
:	endif
:
:	let positional = []
:	let type       = {}
:	exec Exec_PartialArgs(function('Call'), 2, 1)
:
:	" String, function.
:	let ifunc  = get(type, type(function('Call')), [function(type[type('')][0])])
:	let iargs  =                                   type[type([])]
:	let midict =            eval(Eval_MaybeExists('type[type({})]'))
:
:	if len(midict)
:		return call(positional[ifunc[0]], positional[iargs[0]], positional[midict[0][0]])
:	else
:		return call(positional[ifunc[0]], positional[iargs[0]])
:	endif
:endfunction

" http://nvie.com/posts/how-i-boosted-my-vim/

set nocompatible

execute pathogen#infect()

" Use pathogen to easily modify the runtime path to include all
" plugins under the ~/.vim/bundle directory
execute pathogen#helptags()

autocmd!

set nobackup
set swapfile

set hidden
set nowrap
set backspace=indent,eol,start

set autoread

set autoindent
set copyindent

set showmatch
set incsearch
set hlsearch
set ignorecase
set smartcase

set ruler
set wildmenu

set laststatus=2

set spell

set virtualedit=onemore,block
"set virtualedit=onemore,block,insert,all

function! ResetTabSettings()
	" A tab is spaced as o:tabstop spaces.
	"
	" Set to the standard, not your preference, for which you should use
	" `softtabstop` and `shiftwidth` for insertion / editing and vim's
	" autoindentation and manual indentation procedures.
	"set tabstop=8
	"set tabstop=6
	set tabstop=4

	" Expand typed tabs and vim-alignment-inserted tabs into spaces.
	"set expandtab
	set noexpandtab
	" (c.f. tabstop)
	"set shiftwidth=2
	set shiftwidth=0
	"set softtabstop=2
	set softtabstop=0

	set noshiftround

	":execute IndentGuidesEnable()
	":execute indent_guides#enable()
endfunction
call ResetTabSettings()


"	\ .',tab:‣◦․'
"	\ .'=eol:😔﹩'
"	\ .',tab:‣◦'
"	\ .',trail:•'
"	\ .',tab:»·'
"execute 'set listchars'
"	\ .'=eol:⤙'
"	\ .',tab:‣·'
"	\ .',trail:◦'
"	\ .',extends:↣'
"	\ .',precedes:↢'
"	\ .',conceal:*'
"	\ .',nbsp: '
"	\ .'=tab:‣\ '
"execute 'set listchars'
"	\ .'=eol:▏'
"	\ .',tab:‣·'
"	\ .',trail:◦'
"	\ .',extends:↣'
"	\ .',precedes:↢'
"	\ .',conceal:*'
"	\ .',nbsp: '
execute 'set listchars'
	\ .'=tab:‣·'
	\ .',trail:◦'
	\ .',extends:↣'
	\ .',precedes:↢'
	\ .',conceal:*'
	\ .',nbsp: '

set list
"set list
"set listchars=eol:$
"set listchars=tab:>.,trail:.,extends:#,nbsp:.

filetype plugin on
filetype indent off













"set guifont=Consolas:h11
"set guifont=Monaco:h11

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
"set showmatch

" Set minimium window width to 0
set wmh=0

" enter spaces when tab is pressed:
"set expandtab
" do not break lines when line length increases
"set textwidth=0
" user 4 spaces to represent a tab
"set tabstop=4
"set tabstop=2
"set softtabstop=4
"set softtabstop=2
" number of space to use for auto indent
" you can use >> or << keys to indent current line or selection
" in normal mode.
"set shiftwidth=4
"set shiftwidth=2
" Copy indent from current line when starting a new line.
"set autoindent
" makes backspace key more powerful.
"set backspace=indent,eol,start
" shows the match while typing
"set incsearch
" case insensitive search
"set ignorecase
" show line and column number
"set ruler
" show some autocomplete options in status bar
"set wildmenu
" turns off the backup junk files that vim leaves. Files made whle Vim is open
" still stay
"set nobackup

"set spell

"" Colors and syntax
""colorscheme ir_black
"colorscheme wombat
"set background=dark
"syntax on

syntax enable
set background=dark
colorscheme solarized
" Set this if not using the solarized color palette, highly recommended.
"let g:solarized_termcolors=256  " Inferior alternative, according to documentation.

"set ignorecase
"set hidden
"set laststatus=2

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

" Insert mode.
inoremap <nowait> <Nul> <Esc>`]
" Command mode.
cnoremap <nowait> <Nul> <c-c>
" Normal mode.
nnoremap <nowait> <Nul> <Esc>
" Visual mode.
xnoremap <nowait> <Nul> <Esc>
" Select mode.
snoremap <nowait> <Nul> <Esc>
" Operator-pending mode.
onoremap <nowait> <Nul> <Esc>
" Insert, command-line, and lang-arg mode.
"lnoremap <nowait> <Nul> <Esc>
lnoremap <nowait> <Nul> <Esc>

"set pastetoggle=<Nul>
"nnoremap <f10> :set paste<cr>
"nnoremap <f10> :set pastetoggle=\<Nul><cr> :set paste<cr>  <-- then how to
"reset pastetoggle once done?
set pastetoggle=<f10>




"inoremap <nowait> <Nul> <Esc>l
""imap <Nul> <Esc><Esc>s
"nmap <Nul> <Esc><Esc>
"vmap <Nul> <Esc><Esc>
"xmap <Nul> <Esc><Esc>
"smap <Nul> <Esc><Esc>
"omap <Nul> <Esc><Esc>
"lmap <Nul> <Esc><Esc>
"cmap <Nul> <Esc><Esc>

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

" moved above
"" spell checking hilight
"set spell



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







"" map dvorak movement keys and ``domino'' keys
"noremap d h
"noremap D H
""noremap h j
""noremap t k
"" The capitol versions of these are already used for convenient purposes
"noremap h gj
"noremap t gk
"noremap n l
"noremap N L
"
"" delete -> junk
"noremap j d
"noremap J D
""" substitute -> junk and replace
""noremap j s
""noremap j S
"" join -> konnect
"noremap k j
"noremap K J
"" next -> last search
"noremap l n
"noremap L N
"
"" convenience stuff
"noremap H 8gj
"noremap T 8gk
"noremap <m-h> j
"noremap <m-t> k
""noremap - $
""noremap _ ^

" windows too
" Use ^U for moving through windows, and ^W for everything else
"map <C-u>d <C-w>h
"map <C-u>h <C-w>j
"map <C-u>t <C-w>k
"map <C-u>n <C-w>l
"map <C-i>d <C-w>h
"map <C-i>h <C-w>j
"map <C-i>t <C-w>k
"map <C-i>n <C-w>l
"map <C-u><C-d> <C-w><C-h>
"map <C-u><C-h> <C-w><C-j>
"map <C-u><C-t> <C-w><C-k>
"map <C-u><C-n> <C-w><C-l>

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
"nnoremap <c-k> :tabnew<Esc>:bn<Esc>:bd #<Esc>
nnoremap <c-k> :tabnew<cr>:bn<cr>:bd #<cr>
nnoremap <c-j> :tabclose<cr>
"nnoremap <c-m> :enew<Esc>

"nnoremap <c-d> :tabp<cr>
"nnoremap <c-n> :tabn<cr>
nnoremap <c-d> gT
"nnoremap <c-n> gt
nnoremap <c-n> :<c-u>execute 'tabn'.string(((tabpagenr() + v:count1)-1) % tabpagenr('$') + 1)<cr>
" Thanks <http://stackoverflow.com/a/2062858/2899502>!
"nnoremap <c-s-d> :<c-u>execute 'tabmove -'.v:count1<cr>
"nnoremap <c-s-n> :<c-u>execute 'tabmove +'.v:count1<cr>
"nnoremap <m-space><space>   <space>
"nnoremap <m-space><m-space> :call SpecialMode('m-space')












""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""
"""" LIST UTILITIES """
"""""""""""""""""""""""

:function! MaxLength(list)
:	let l:max = 0
:
:	for l:arg in a:list
:		let l:len = len(l:arg)
:
:		if l:len > l:max
:			let l:max = l:len
:		endif
:	endfor
:
:	return l:max
:endfunction

:function! MinLength(list)
:	let l:i_max = len(a:list) - 1
:
:	if l:i_max == -1
:		return 0
:	endif
:
:	let l:min = len(a:list[0])
:
:	let l:i = 1
:	let l:i-=1| while l:i < l:i_max |let l:i+=1
:		let l:len = len(a:list[l:i])
:
:		if l:len < l:min
:			let l:min = l:len
:		endif
:	endwhile
:
:	return l:min
:endfunction

" Result has same length as shortest.
"
" Example:
"
" ```
" :echo Zip([100, 200, 300], [10, 20, 30, 40, 50], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
" ```
"
" > ```
" > [[100, 10, 0], [200, 20, 1], [300, 30, 2]]
" > ```

" Prepopulating lists instead of repeated calls to `add` is a speculative
" optimization attempt.
:function! Zip(...)
:	if empty(a:000)
:		return []
:	endif
:
:
:	let l:i        = 0
:	let l:width    = len(a:000)
:	let l:windices = range(l:width)
:	" max length - 1
":	let l:max_i    = min(map(copy(a:000), 'len(v:val)')) - 1
:	let l:max_i    = MinLength(a:000) - 1
:	let l:zipped   = repeat([0x00], l:max_i + 1)
:
:	let l:i-=1 | while l:i < l:max_i | let l:i+=1
:		let l:pairing = repeat([0x00], l:width)
:		let l:zipped[i]=l:pairing
:
:		for l:wi in l:windices
:			let l:pairing[l:wi] = a:000[l:wi][l:i]
:		endfor
:	endwhile
:
:	return l:zipped
:endfunction

" Result has same length as longest.
"
" Example:
"
" ```
" :echo ZipCycling([100, 200, 300], [10, 20, 30, 40, 50], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
" ```
"
" > ```
" > [[100, 10, 0], [200, 20, 1], [300, 30, 2], [100, 40, 3], [200, 50, 4], [300, 10, 5], [100, 20, 6], [200, 30, 7], [300, 40, 8], [100, 50, 9]]
" > ```
:function! ZipCycling(...)
:	if empty(a:000)
:		return []
:	endif
:
:
:	let l:width    = len(a:000)
:	let l:windices = repeat([0x00], l:width)
:	" max length - 1
":	let l:max_i    = max(map(copy(a:000), 'len(v:val)')) - 1
":	let l:max_i    = MaxLength(a:000) - 1
:	let l:max_len  = 0
:
:	let l:i     = 0
:	let l:max_i = l:width - 1
:	let l:i-=1 | while l:i < l:max_i | let l:i+=1
:		let l:len = len(a:000[l:i])
:		let l:windices[l:i] = [l:i, l:len]
:
:		if l:len > l:max_len
:			let l:max_len = l:len
:		endif
:	endwhile
:
:	let l:zipped   = repeat([0x00], l:max_len)
:
:	let l:i        = 0
:	let l:max_i = l:max_len - 1
:	let l:i-=1 | while l:i < l:max_i | let l:i+=1
:		let l:pairing = repeat([0x00], l:width)
:		let l:zipped[i]=l:pairing
:
:		for [l:wi, l:len] in l:windices
:			let l:pairing[l:wi] = a:000[l:wi][l:i % l:len]
:		endfor
:	endwhile
:
:	return l:zipped
:endfunction










""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""
"""" VIM FUNCTION UTILITIES """
"""""""""""""""""""""""""""""""





" Set output to any non-dict value to discard.
" output (→: return value) (?: might be unset) (+: always set):
"   +isDefaultFunction: argument is one of the provided funcref itself, in which case isDefault is false
"   →isDefault: default specified.
"   ?key:   corresponding key in pair if default.
"   ?value: corresponding value in pair if default.
:function! IsDefault(argument, output) dict
:	if !exists('g:default_specifiers')
:		let g:default_specifiers = {IsDefault: function('IsDefault'), AddDefault: function('AddDefault'), Default: function('Default')}
:	endif
:
:	let a:output = type(a:output) == type({}) ? a:output : {}
:
:	if !has_key(self, string(a:argument)) || self[string(a:argument)] isnot a:argument
:		let a:output.isDefault   = 0
:	else
:		let a:output.isDefault   = 1
:		let a:output.key         = argument
:		let a:output.value       = self[argument]
:	endif
:
:	if a:argument == function('IsDefault') || a:argument == function('Default') || a:argument == function('AddDefault')
:		let a:output.isIsDefault = 1
:		let a:output.isDefault   = 0
:	else
:		let a:output.isIsDefault = 0
:	endif
:
:	return a:output.isDefault
:endfunction

:function! AddDefault(specifier) dict
:	if !exists('g:default_specifiers')
:		let g:default_specifiers = {IsDefault: function('IsDefault'), AddDefault: function('AddDefault'), Default: function('Default')}
:	endif
:
:	let self[string(a:specifier)] = specifier
:
:	return a:specifier
:endfunction

" Get a default specifier.
"
" Can also be used for initialization.
:function! Default() dict
:	if !exists('g:default_specifiers')
:		let g:default_specifiers = {IsDefault: function('IsDefault'), AddDefault: function('AddDefault'), Default: function('Default')}
:	endif
:
:	for value in values(self)
:		if value != function('IsDefault') && value != function('Default') && value != function('AddDefault')
:			return value
:		endif
:	endfor
:
:	return AddDefault([{'default': ['default']}])
:endfunction

" TODO: FIXME!
":if !exists('g:default_specifiers')
":	let g:default_specifiers = {IsDefault: function('IsDefault'), AddDefault: function('AddDefault'), Default: function('Default')}
":endif

:function! GDefault(...)
:	return call(function('Default'), a:000, g:default_specifiers)
:endfunction
":function! GAddDefault(...)
":	return call(function('AddDefault'), a:000, g:default_specifiers)
":endfunction
:function! GIsDefault(...)
:	return call(function('IsDefault'), a:000, g:default_specifiers)
:endfunction

" TODO: FIXME!
":call GDefault()







:let g:exec_lines = {}

:function! g:exec_lines.ExecLines(lines_key) dict
:	return ExecLines(self[a:lines_key])
:endfunction

:function! ExecLines(lines) dict
":	execute join(lines, "\n")
:	execute join(map(deepcopy(lines), 'call(function(''printf''), v:val)'), "\n")
:endfunction








" Extra arguments are added to printf's arguments.
:function! InvalidNumberOfArguments(name, valid_numbers, attempted_number, format_or_defaulting_zero, ...)
:	if format_or_defaulting_zero == 0
:		let a:format_or_defaulting_zero = '%s: Sorry, unsupported number of arguments ''%d''; supported: %s'
:	endif
:
:	if type(attempted_number) == type([])
:		let a:attempted_number = len(a:attempted_number)
:	endif
:
:	if type(valid_numbers) == type({})
:		let a:valid_numbers = keys(a:valid_numbers)
:	endif
:
:	throw (call(printf, [a:format_or_defaulting_zero, a:name, a:attempted_number, join(a:valid_numbers, ', ')] + a:000))
:endfunction

:let s:procedureVarArgsProcedures = {}
:function! ProcedureVarArgs(...)
:	let l:name       = 'ProcedureVarArgs'
:	let l:procedures = s:procedureVarArgsProcedures
:
:	let l:num = len(a:000)
:
:	if !has_key(l:procedures, 'arity' . string(l:num))
:		return InvalidNumberOfArguments(l:name, l:procedures, a:000, 0)
:	else
:		return call(l:procedures[], a:000, s:procedureVarArgsProcedures)
:	endif
:endfunction

:function! s:procedureVarArgsProcedures.arity2(...) dict
:	return call(self.arity3, [GDefault()] + a:000, self)
:endfunction

" Omit 'force' for default of false.
:function! s:procedureVarArgsProcedures.arity3(force_optional, name, procedures) dict
:	let l:force = (GIsDefault(a:force_optional) ? 0 : a:force_optional) ? '!' : ''
:	return ExecParts(
\		[ [':function%s %s(...)', l:force, a:name]
\		, [':	TODO']
\		, [':endfunction']
\		]
\	)
:endfunction




""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""
"""" DICT CACHES    """
"""""""""""""""""""""""


"TODO!!!!

":let s:dictCacheProcedures = {}
":function! s:dictCacheProcedures.DictCache(...) dict
":endfunction
":function! DictCache(...)
"	return ProcedureVarArgs('DictCache', s:dictCacheProcedures)
":endfunction

":function s:dictCacheProcedures.arity0
":endfunction




















":let s:special_modes_cache = [{}]
":let s:special_modes       = [[]]
":function! Internal_SpecialModes_RawGetCache()
":	return s:special_modes_cache
":endfunction
":function! Internal_SpecialModes_RawGetStorage()
":	return s:special_modes
":endfunction
":function! Internal_SpecialModes_RawSetCache(value)
":	let s:special_modes_cache = value
":	return value
":endfunction
":function! Internal_SpecialModes_RawSetStorage(value)
":	let s:special_modes = value
":	return value
":endfunction
"
":function! Internal_SpecialModes_GetCache()
":	return Internal_SpecialModes_RawGetCache()[0]
":endfunction
":function! Internal_SpecialModes_GetStorage()
":	return Internal_SpecialModes_RawGetStorage()[0]
":endfunction
":function! Internal_SpecialModes_SetCache(value)
":	let cache_pointer = Internal_SpecialModes_RawGetCache()
":	let cache_pointer[0] = value
":	return value
":endfunction
":function! Internal_SpecialModes_SetStorage(value)
":	let store_pointer = Internal_SpecialModes_RawGetStorage()
":	let store_pointer[0] = value
":	return value
":endfunction
"
":function! Internal_SpecialModes_ClearCache()
":	return Internal_SpecialModes_SetCache({})
":endfunction
":function! Internal_SpecialModes_CacheEntry(special_mode)
":	
":endfunction
":function! Internal_SpecialModes_RebuildCache()
":	let l:cache = Internal_SpecialModes_ClearCache()
":	let l:store = Internal_SpecialModes_GetStorage()
":
":	for l:special_mode in l:store
":		call Internal_SpecialModes_CacheEntry(l:special_mode)
":	endfor
":endfunction
"
":function! SpecialModes_Clear()
":	let l:store
":endfunction
"
":function! ModeModel
"\	( mode_name
"\	, start_key_name
"\	, start_key
"\	, start_getchar_code
"\	)
":	return
"\		{ 'primary_identifier':
"\			primary_identifier
"\		, 'mode_name':
"\			mode_name
"\		, 'start_key_name':
"\			start_key_name
"\		, 'start_key':
"\			start_key
"\		, 'start_getchar_code':
"\			start_getchar_code
"\		}
":endfunction
"
":function! ClearSpecialModes
"
":function! SpecialMode(mode)
":	while nr2char(getchar)
":	endwhile
":endfunction

set notimeout
set nottimeout

nnoremap <c-h> :bp<cr>
nnoremap <c-t> :bn<cr>

nnoremap  k.`[
nnoremap <c-a> j.`[
nnoremap <c-u> j.`[
nnoremap ' k.
nnoremap a j.
nnoremap u j.

"nnoremap  k.`[
"nnoremap <c-g> k.`[
"nnoremap <c-a> j.`[
"nnoremap <c-u> j.`[
"nnoremap ' k.
"nnoremap a j.
"nnoremap u j.

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

"""set cindent
""set nosmartindent
""set noautoindent
"set nocindent
"set autoindent
"set smartindent
"filetype indent on


map [11~ <F1>
map [12~ <F2>
map [13~ <F3>
map [14~ <F4>
map [15~ <F5>
map [17~ <F6>
map [18~ <F7>
map [19~ <F8>
map [20~ <F9>
map [21~ <F10>
map [23~ <F11>
map [24~ <F12>



" Read
"nnoremap <F3> :r ~/.vim/write_tmp<cr>
nnoremap <F3> :r ~/.vim/write_tmp<Esc>

" Write
"vmap <F4> :'<,'>w ~/.vim/tmp<Esc>  " '<,'> is inserted automatically
"vmap <F4> :w! ~/.vim/write_tmp<Esc>
vmap <F4> :w! ~/.vim/write_tmp<Esc>
"nmap <F4> :.w! ~/.vim/write_tmp<Esc>
"nmap <F4> :w! ~/.vim/write_tmp<Esc>
nmap <F4> :w! ~/.vim/write_tmp<Esc>

"nmap <F1>  iIt works!-f1<cr><Nul>
"nmap <F2>  iIt works!-f2<cr><Nul>
"nmap <F3>  iIt works!-f3<cr><Nul>
"nmap <F4>  iIt works!-f4<cr><Nul>
"nmap <F5>  iIt works!-f5<cr><Nul>
"nmap <F6>  iIt works!-f6<cr><Nul>
"nmap <F7>  iIt works!-f7<cr><Nul>
"nmap <F8>  iIt works!-f8<cr><Nul>
"nmap <F9>  iIt works!-f9<cr><Nul>
"nmap <F10> iIt works!-f10<cr><Nul>
"nmap <F11> iIt works!-f11<cr><Nul>
"nmap <F12> iIt works!-f12<cr><Nul>

set vb t_vb=

"set nocp
"filetype plugin on

"nm <c-p> :se invpaste paste?<CR>

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

"function! Asm()
"	set noexpandtab ts=16 sw=16 nocindent nosmartindent autoindent
"	map <Tab> <Nop>
"endfunction
"
"if !exists("autocommands_fileTypes_loaded")
"    let autocommands_fileTypes_loaded = 1
"    " for Haskell and python source files, set expand on
"    "au FileType python,haskell set expandtab ts=4 sw=4 nocindent nosmartindent autoindent
"    au FileType python,haskell set expandtab sts=2 ts=2 sw=2 nocindent nosmartindent autoindent
"    au FileType asm call Asm()
"endif

vmap <F2> :g/^/m 0<Esc>

"set tabpagemax=64


" Haskell

" use ghc functionality for haskell files
"au Bufenter *.hs compiler ghc

"" switch on syntax highlighting
"set syntax on
syntax on

"" enable filetype detection, plus loading of filetype plugins
"set filetype plugin on
"filetype plugin on

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


""colorscheme aqua
""colorscheme fruity
""colorscheme wood
"
""colorscheme dante
""colorscheme relaxedgreen
""colorscheme wombat
""colorscheme matrix
""colorscheme bairyn
"colorscheme wombat
"set background=dark

"highlight Pmenu ctermbg=238 gui=bold

" To disable a plugin, add it's bundle name to the following list
""let g:pathogen_disabled = []
"let g:pathogen_disabled = []

" for some reason the csscolor plugin is very slow when run on the terminal
" but not in GVim, so disable it if no GUI is running
"if !has('gui_running')
    "call add(g:pathogen_disabled, 'csscolor')
"endif

" Gundo requires at least vim 7.3
"if v:version < '703' || !has('python')
    "call add(g:pathogen_disabled, 'gundo')
"endif

"if v:version < '702'
    "call add(g:pathogen_disabled, 'autocomplpop')
    "call add(g:pathogen_disabled, 'fuzzyfinder')
    "call add(g:pathogen_disabled, 'l9')
"endif

""call pathogen#infect()
"call pathogen#infect()

"hdevtools Haskell vim
"au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
"au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>

au BufWritePost *.hs silent !fast-tags -o tags % &
"Problems with indentation / comments?  http://stackoverflow.com/questions/2360249/vim-automatically-removes-indentation-on-python-comments 
"set nosmartindent
"set cindent

function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc

nnoremap <C-_> :call NumberToggle()<cr>

"unmap <ENTER>

set tw=79

" # indentation
:inoremap # X<BS>#
"set cinkeys-=0#
"set indentkeys-=0#

nnoremap <ESC>h <C-w>j
nnoremap <ESC>t <C-w>k
nnoremap <ESC>d <C-w>h
nnoremap <ESC>n <C-w>l
nnoremap <ESC>s <C-w>s
nnoremap <ESC>v <C-w>v
nnoremap <ESC>q <C-w>q
nnoremap <ESC>+ <C-w>+
nnoremap <ESC>- <C-w>-
nnoremap <ESC>> <C-w>>
nnoremap <ESC>< <C-w><

" See line 74
"imap <Nul> <Esc><Esc>n

" http://vim.wikia.com/wiki/Restore_cursor_to_file_position_in_previous_editing_session
" Tell vim to remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  <100 :  new name of "100
"  :20  :  DON'T USE THIS - INSTEAD set history=100 - up to 20 lines of command-line history will be remembered
"  %    :  ?  saves and restores the buffer list? - You can use sessions, too!
"               (`mksession` to create a `source`able file.)
"  n... :  where to save the viminfo files
"  /    :  search patterns
"  f1   :  all file marks
"  h    :  ?  "Disable the effects of hlsearch upon loading viminfo"?
"set viminfo='10,\"100,:20,%,n~/.viminfo
""set viminfo='10,\"100,:20,%,nc:\\some\\place\\under\\Windows\\_viminfo
set viminfo=f11,!,'4096,<1024,/4096,%,n~/.vim/viminfo
" The maximum value is 10,000.
"set history=''65.536
execute 'set history='.10.000

function! ResCur()
  if line("'\"") <= line("$")
"    normal! g`"
"   FIXME: check if the mark is set!- TODO!
    silent! normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

"augroup! resCur




" vim -p max tabs displayed
exec 'set tabpagemax='.100.000


"""""""""""""""""""""""""""" Trailing whitespace.
" Thanks <http://stackoverflow.com/a/13795287/2899502>!

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
augroup whiteSpace
  autocmd!
  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
"  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
"  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
  autocmd BufWinLeave * call clearmatches()
augroup END

function! TrimWhiteSpace()
    "%s/\s\+$//e
    "%s/\v\s+$//e
    " Don't strip any line that could contain embedded markdown where trailing
    " whitespace is significant, e.g. a MD-formatted-comment.
    "
    " Ignores lines containing '*' and ending in two spaces.
    "%s/\v\s+$//e
    normal! m[
    %s/\v((\*(.*[^ ])?  $)|\s+$|)/\2/e
    normal! `[
endfunction
" TODO: Fixme: The cursor jumps upon saving!
"augroup whiteSpaceTrim
"  autocmd!
"  autocmd BufWritePre * :call TrimWhiteSpace()
"augroup END

"""""""""""""""""""""""""""" Misc utilities.
" Ah, strings can't contain NUL?
"let g:command_separator = "\<NUL>"
"let g:e__n_command_separator = 'let l:n =  '

:function! FuncWrap(assign_name, func)
":  :let l:n = g:command_separator
:  :eval g:e__n_command_separator
:  :execute ':'
     \ l:n . ':function! ' . escape(a:assign_name) . '(...)'
     \ l:n . '  :return call(' . escape(a:func) . ', a:000)'
     \ l:n . ':endfunction'
:endfunction

:function! s:SID()
:  :return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
:endfunction




" default on my system:
"clipboard=autoselect,exclude:cons\|linux

" Yanks, etc.
"unnamed: 
"set clipboard=unnamed,unnamedplus,autoselect,exclude:cons\|linux
set clipboard=unnamed,unnamedplus,exclude:cons\|linux


"""""""""""""""""""""""""""" Abbreviations.

" OLD
"        " Thanks `:h abbreviations`!
"        "func! Eatchar(pat)
"        "  let c = nr2char(getchar(0))
"        "  return (c =~ a:pat) ? '' : c
"        "endfunc
"        "
"        "function! Eatspace()
"        "  call Eatchar('\s')
"        "endfunc
"        "let g:eatspace="\<Left>\<C-R>=Eatspace()\<CR>"





" Thanks <http://stackoverflow.com/a/1680834/2899502>!
function! s:Expr(default, repl)
  if getline('.')[col('.')-2]=='\'
    return "\<bs>".a:repl
  else
    return a:default
  endif
endfunction

function! s:ExprFor(default, repl, prefix)
  if getline('.')[col('.')-1 - len(a:prefix):][:len(a:prefix)-1]==a:prefix
    return repeat("\<bs>", len(a:prefix)).a:repl
  else
    return a:default
  endif
endfunction

" Thanks <http://stackoverflow.com/a/1680834/2899502>!
:iab perspective <c-r>=<sid>Expr('perspective', '⌆')<cr>

"" Wow, thanks <http://stackoverflow.com/a/1680834/2899502>!
"function! s:DefIab(nore, ...)
"  let l:opt = ''
"  let l:i = 0
"  while l:i != len(a:000)
"    let l:arg = a:000[l:i]
"    if l:arg !~? '<buffer>\|<silent>'
"      break
"    endif
"    let l:opt .= ' '.l:arg
"    let l:i += 1
"  endwhile
"
"  if l:i+2 != len(a:000)
"    throw "Invalid number of arguments"
"  endif
"  let l:lhs = a:000[l:i]
"  let l:rhs = a:000[l:i+1]
"
"  exe 'i'.a:nore.'ab'.l:opt.' '.l:lhs.' <c-r>=<sid>Expr('.string(l:lhs).', '.string(l:rhs).')<cr>'
"endfunction


""   abbreviate_command (:inoreabbrev <silent>): TODO
":let s:Abbreviate = {}
":function! g:call.Abbreviate(...) dict
":	return call()
":endfunction!
":command! -nargs=+ Abbreviate call Abbreviate(<f-args>)
"
":function s:Abbreviate.function() dict
"
":function! Abbreviate(...)
":	if a:0 >= 1 && type(a:1) != type({})
":		return call Abbreviate({}, a:000)
":	endif
":
":	let opts = a:1
":	let cmd = get(opts, 'abbreviate_command',
"		'inoreabbrev <silent>')
":
":	let abbreviate_command = 
":
":	:if len == 0
":	:else
":	:endif
":
":	let opts = {}
":
":	if len == 0
":	endif
":	if len(a:000
":endfunction

" Wow, thanks <http://stackoverflow.com/a/1680834/2899502>!
"
" TODO: you can improve this!
"
" Just abbreviate the last one/two characters, which are always valid, 
"
"
" From the right of string, just determine which of the three types the
" abbreviation is, and then keep going backwards while the string is valid.
" Once it's invalid, split off prefix.
"
" In fact, you could just abbreviate the last two characters
"
" (end char: if keyword char (a letter or member of &iskeyword
" -> 
function! s:DefIab(nore, ...)
  let opt = ''
  let i = 0
  while i <= len(a:000)
    let arg = a:000[i]
    if arg !~? '<buffer>\|<silent>'
      break
    endif
    let opt .= ' '.arg
    let i += 1
  endwhile

  if i+2 != len(a:000) && i+3 != len(a:000)
    throw "Invalid number of arguments"
  endif
  let lhs = a:000[i]
  let rhs = a:000[i+1]

  if i+2 == len(a:000)
    exe 'i'.a:nore.'ab'.opt.' '.lhs.' <c-r>=<sid>Expr('.string(lhs).', '.string(rhs).')<cr>'
  elseif 1
    let prefix = a:000[i+2]
    exe 'i'.a:nore.'ab'.opt.' '.lhs.' <c-r>=<sid>ExprFor('.string(lhs).', '.string(rhs).', '.string(prefix).')<cr>'
  else
    let prefix_chars = ['\', '~']

    let prefix = ''

    while len(lhs) >= 1 && index(prefix_chars, lhs[0]) != -1
      let prefix .= remove(lhs, 0)
    endwhile

    if len(prefix) <= 0
      if len(opt) >= 0 || a:nore != 'nore'
        exe 'i'.a:nore.'ab'.opt.' <silent> '.lhs.' <c-r>=<sid>Expr('.string(lhs).', '.string(rhs).')<cr>'
      else
        exe 'inoreab <silent> '.lhs.' <c-r>=<sid>Expr('.string(lhs).', '.string(rhs).')<cr>'
      end
    else
      if len(opt) >= 0 || a:nore != 'nore'
        inoreab <silent> '.lhs.' <c-r>=<sid>ExprFor('.string(lhs).', '.string(rhs).', '.string(prefix).')<cr>'
      else
        exe 'inoreab <silent> '.lhs.' <c-r>=<sid>ExprFor('.string(lhs).', '.string(rhs).', '.string(prefix).')<cr>'
      end
    endif
  endif
endfunction

command! -nargs=+ InoreabBSlash call s:DefIab('nore', <f-args>)
command! -nargs=+ Shorthand call s:DefIab('nore', <f-args>)

" ⬅ ➡ ⬆ ⬇ ⬈ ⬉ ⬊ ⬋ ⬌ ⬍
:ab <3#  ❤
:ab ...# …
:ab ::#  ∷
:ab /#   ⁄
:ab /1#  ⁄
:ab /2#  ∕
:ab dagger# †
:ab ddagger# ‡
: ab doubledagger# ‡
: ab double-dagger# ‡

"http://emojipedia.org/new-emoji/
:ab peace# ☮
:ab poke# 🖙
:ab doveofpeace# 🕊

:ab x#  ×
:ab A#  ∀
:ab E#  ∃
:ab F#  Ⅎ
:ab ~E# ∄

" Guillemets (a.k.a angle quotes, Latin quotation marks, French quotation
" marks.)
:ab <<# «
:ab >># »

":Shorthand x         × \\
":Shorthand A         ∀ \\
":Shorthand forall    ∀ \\
":Shorthand Forall    ∀ \\
":Shorthand all       ∀ \\
":Shorthand All       ∀ \\
":Shorthand E         ∃ \\
":Shorthand exists    ∃ \\
":Shorthand Exists    ∃ \\
":Shorthand E         ∄ \\~
":Shorthand exists    ∄ \\~
":Shorthand Exists    ∄ \\~

Shorthand element   ∈ \\
"Shorthand ~element  ∉ \\~
Shorthand contains  ∋ \\
"Shorthand ~contains ∌ \\~

Shorthand s         σ \\
Shorthand sigma     σ \\
Shorthand S         Σ \\
Shorthand Sigma     Σ \\

Shorthand l         λ \\
Shorthand lambda    λ \\
Shorthand L         Λ \\
Shorthand Lambda    Λ \\

Shorthand p         π \\
Shorthand pi        π \\
Shorthand P         Π \\
Shorthand Pi        Π \\

Shorthand o         ω \\
Shorthand omega     ω \\
Shorthand O         Ω \\
Shorthand Omega     Ω \\

Shorthand a         α \\
Shorthand alpha     α \\
Shorthand A         Α \\
Shorthand Alpha     Α \\

Shorthand b         β \\
Shorthand beta      β \\
Shorthand B         Β \\
Shorthand Beta      Β \\

Shorthand g         γ \\
Shorthand gamma     γ \\
Shorthand G         Γ \\
Shorthand Gamma     Γ \\

Shorthand d         δ \\
Shorthand damma     δ \\
Shorthand D         Γ \\
Shorthand Damma     Γ \\

Shorthand e         ε \\
Shorthand epsilon   ε \\
Shorthand E         Ε \\
Shorthand Epsilon   Ε \\

Shorthand  e        ϶ \\<
Shorthand  epsilon  ϶ \\<

Shorthand k         κ \\
Shorthand kappa     κ \\
Shorthand K         Κ \\
Shorthand Kappa     Κ \\

Shorthand m         μ \\
Shorthand mu        μ \\
Shorthand M         Μ \\
Shorthand Mu        Μ \\

Shorthand t         θ \\
Shorthand theta     θ \\
Shorthand T         Θ \\
Shorthand Theta     Θ \\

" http://xahlee.info/comp/unicode_arrows.html

" First row: standard arrows.
" ← → ↑ ↓ ↔ ↕ ↖ ↗ ↘ ↙ ↚ ↛ ↮ ⟵ ⟶ ⟷
":iab <expr>   ->    '→'
:ab <expr>   ->#    '→'
:ab <expr>   \->    '→'
:ab <expr>   <-#    '←'
:ab <expr>   -^-#   '↑'
:ab <expr>   -v-#   '↓'
:ab <expr>   <->#   '↔'
:iab <expr>  ^-v#   '↕'
: iab <expr> v-^#   '↕'

" TODO: corner arrows!

:ab <expr>   -/>#   '↛'
:ab <expr>   </-#   '↚'
:ab <expr>   </->#  '↮'
: ab <expr>  <-/>#  '↮'
": :iab <expr> <-/-> '↮'
": :iab <expr> <-/-> '_***try </->!***'

:ab <expr>   -->#   '⟶'
:ab <expr>   <--#   '⟵'
:ab <expr>   <-->#  '⟷'

" Second row: double arrows.
" ⇐ ⇒ ⇑ ⇓ ⇔ ⇕ ⇖ ⇗ ⇘ ⇙ ⇍ ⇏ ⇎ ⟸ ⟹ ⟺
:iab <expr>   =>#    '⇒'
:iab <expr>   <=#    '⇐'
:iab <expr>   =^=#   '⇑'
:iab <expr>   =v=#   '⇓'
:iab <expr>   <=>#   '⇔'
:iab <expr>   ^=v#   '⇕'
: iab <expr>  v=^#   '⇕'

" TODO: corner arrows!

:iab <expr>  '=/>'   '⇏'
:iab <expr>  '</='   '⇍'
:iab <expr>   </=>   '⇎'
: :iab <expr> <=/>   '⇎'
": :iab <expr> <=/=>  '⇎'
: :iab <expr> <=/=> '_***try </->!***'

:iab <expr>   '==>'  '⟹'
:iab <expr>   '<=='  '⟸'
:iab <expr>   '<==>' '⟺'

" Third row: hollow arrows.
" ⇦ ⇨ ⇧ ⇩ ⬄ ⇳ ⬀ ⬁ ⬂ ⬃
:iab _>  ⇨
:iab <_  ⇦
:iab <_>  ⬄

" TODO!

" Fourth row: filled arrows.
" ⬅ ➡ ⬆ ⬇ ⬈ ⬉ ⬊ ⬋ ⬌ ⬍
"
" TODO!


""""""""""""""""""""""""""""""""""""""""""""""""""""" misc. niceness
" http://nvie.com/posts/how-i-boosted-my-vim/



"""""""""""""""""""""""""""""""""""""""""""""""""""""" tabs

""call pathogen#infect()
"execute pathogen#infect()
"execute pathogen#execute('IndentGuidesEnable')
""IndentGuidesEnable
""call indent_guides#enable()

augroup AfterPlugins
	autocmd!
	autocmd VimEnter * IndentGuidesEnable
augroup END

augroup PreserveTabSettings
	autocmd!
	""autocmd VimEnter * call ResetTabSettings()
	"autocmd FileType * call ResetTabSettings()
augroup END


finish































































:function s:GetTypes()
:  return
\    { 'Number':     0
\    , 'String':     1
\    , 'Funcref':    2
\    , 'List':       3
\    , 'Dictionary': 4
\    , 'Float':      5
\    }
:endfunction

:function s:TypesEqual(a, b)
:  return type(a) == type(b)
:endfunction

:function s:HasType(value, type)
:  :let l:err_msg_value_is_type = 0
:
:  :let l:types = s:GetTypes()
:
:  :for l:key in l:types
:  :  :if     tolower(a:type)  == tolower(l:key) || a:type  == l:types[l:key]
:  :  :  :return type(a:value) == l:types[l:key]
:  :  :elseif tolower(a:value) == tolower(l:key) || a:value == l:types[l:key]
:  :  :  :let l:err_msg_value_is_type = 1
:  :  :endif
:  :endfor
:
:  :" Type not found; throw an exception.
:  :"
:  :" If a:value is a type, let the user know that the arguments might be backwards.
:
:  :if !l:err_msg_value_is_type
:  :  :throw
\       ( "HasType(*value, type_name*): sorry, unrecognized type name: "
\       . a:type . "."
\       . "  A type_name of a magic value (c.f. ':h type()') is recognized,"
\       . " as are the following names, specified as strings: "
\       . keys(l:types)
\       )
:  :else
:  :  :throw
\       ( "HasType(*value, type_name*): arguments positioned correctly?"
\       . "  Sorry, unrecognized type name: " 
\       . a:type . "."
\       . "  (However, 'value' ('" . a:value . "') is a recognized type name.)"
\       . "  A type_name of a magic value (c.f. ':h type()') is recognized,"
\       . " as are the following names, specified as strings: "
\       . keys(l:types)
\       )
:  :endif
:endfunction


:" This is called at the end of this script.
:function! s:done() abort
:  :for l:hubbed in keys(s:switch)
:  :  :if has_key(l:hubbed, 'global_export')
:  :  :  :let l:exp = l:hubbed.global_export
:  :  :
:  :  :  :if s:HasType(exp, Number)
:  :  :  :execute ('function! ' . l:hubbed.global_export . '(...)')
:  :  :  :endfunction
:  :  :endif
:  :endfor
:endfunction

:function! s:SID()
:  :return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
:endfunction

:" Returns a List of hubbed function names inside 'switch' marked for global
:" export.  All arguments add to this List.
:if !exists('*s:global_export')
:  :function! s:global_export(...)
:  :  :function! s:global_export(...)
:  :  :let l:export_list = []
:  :
:  :  :for l:hubbed_function_names in a:000
:  :  :  :l:export_list = 
:  :  :endif
:  :endfunction
:endif

:let s:switch = {}

:function! s:Switch(func_name, ...) abort
:  :if !has_key(s:switch, a:func_name)
:  :  :throw
        \( 'Switch'
        \. '(): sorry, unrecognized function: '
        \. a:func_name
        \. '.'
        \)
:  :else
:  :  :call call(function('' . 'a:func_name'), a:000)
:  :endif
:endif

:function <SID>Switch(func_name, ...) abort
:  :if !has_key(s:switch, a:func_name)
:  :  :throw
        \( s:switch_fun_name
        \. '(): sorry, unrecognized function: '
        \. a:func_name
        \. '.'
        \)
:  :else
:  :  :call call(function('' . 'a:func_name'), a:000)
:  :endif
:endif
:let s:switch_func_ref = function('<SNR>' . s:SID() . '_Switch')

:function! NamespaceSwitch(func_name...) abort
:  :call call(s:switch_func_ref, a:000)
:endfunction

:

:function! Namespace(...)
:endif

:if !exists('s:namespaces')
:  :let s:namespaces = {}
:
:  :function! s:Namespace
:endif

:function 

:if !exists('*__NAMESPACING__')
:  let g:__NAMESPACING__ = {}
:endif

:if !exists('*Namespace')

:if !exists('g:_namespacing_')
:  :function! Namespace() abort
:  :  :let g:_namespacing_ = 1
:  :  :let l:namespaces = {}
:  :  :
:  :endfunction
:endif

:call Namespace()


:if !exists('g:profile')
:let g:namespaces
:augroup FileTypeAutocmdConfiguration
:  :autocmd!
:augroup END

:augroup FileTypeConfigurations()
:  :autocmd!
:  :autocmd FileType *        call FileTypeResetConfiguration()
:  :autocmd FileType *        call FileTypeSetConfiguration(&filetype)

:  :autocmd FileType txt      call FileTypeZshConfiguration()
:  :autocmd FileType zsh      call FileTypeZshConfiguration()
:  :autocmd FileType ruby     call FileTypeZshConfiguration()
:  :autocmd FileType haskell  call FileTypeHaskellConfiguration()
:  :autocmd FileType markdown call FileTypeZshConfiguration()
:augroup END

:function! FileTypeResetConfiguration() abort
:  :augroup FileTypeAutocmdConfiguration
:  :  autocmd!
:  :augroup END
:endfunction

:function! FileTypeSetConfiguration(...) abort
:  :function! _Args0()
:  :  :unfun
:  :  :call FileTypeDefaultConfiguration()
:  :endfunction
:
:  :function! _Args1()
:
:  :if a:0 == 0
:  :  :call FileTypeSetConfiguration0()
:  :elseif a:0 == 0
:  :  :call FileTypeSetConfiguration1(a:1)
:  :else
:  :  :echoerr(''
:  :    \. "FileTypeSetConfiguration: requires 0 or 1 arguments; " . a:0
:  :    \. " provided.")
:  :endif
:endfunction

:function! FileTypeZshConfiguration()
:  :augroup FileTypeAutocmdConfiguration
:  :  autocmd!
:  :  autocmd CursorMoved,CursorMovedI * TextWidth()
:  :augroup END
:endfunction

autocmd CursorMoved,CursorMovedI * call CheckTextWidth()
function CheckTextWidth()
  :if match(getline("."), 'foobar')
  :  echo hello
  :endif
endfunction
"      ^(    \s+     -    \s+     |    \s*     --    \s+     |    \s*     #    \s+     |    \s+     \*    \s+     |    \s*     //    \s+     )
"      ^( - comment               |-- comment                |# comment                | * comment                |// comment                )


:call s:done()




finish
