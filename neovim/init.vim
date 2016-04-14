set runtimepath^=/home/chaomai/Documents/Codes/Current/GitHub/dotfiles/neovim/dein.vim/repos/github.com/Shougo/dein.vim

call dein#begin(expand('/home/chaomai/Documents/Codes/Current/GitHub/dotfiles/neovim/dein.vim'))

call dein#add('Shougo/dein.vim')

" code completion
call dein#add('Valloric/YouCompleteMe', {'build': './install.py --clang-completer --gocode-completer --tern-completer --racer-completer'})

" language support
call dein#add('critiqjo/lldb.nvim')
call dein#add('derekwyatt/vim-scala')
call dein#add('elzr/vim-json')
call dein#add('fatih/vim-go')
call dein#add('hdima/python-syntax')
call dein#add('nsf/gocode', {'rtp': 'vim/'})

" git
call dein#add('Xuyuanp/nerdtree-git-plugin')
call dein#add('airblade/vim-gitgutter')
call dein#add('tpope/vim-fugitive')

" colorscheme
call dein#add('baskerville/bubblegum')
call dein#add('junegunn/seoul256.vim')
call dein#add('mhartington/oceanic-next')
call dein#add('morhetz/gruvbox')

" misc
call dein#add('Chiel92/vim-autoformat')
call dein#add('Shougo/unite.vim', {'on_cmd': 'Unite'})
call dein#add('Yggdroot/indentLine')
call dein#add('easymotion/vim-easymotion')
call dein#add('jeetsukumaran/vim-buffergator')
call dein#add('luochen1990/rainbow')
call dein#add('majutsushi/tagbar')
call dein#add('mhinz/vim-grepper')
call dein#add('scrooloose/nerdcommenter')
call dein#add('scrooloose/nerdtree', {'on_cmd': 'NERDTreeToggle'})
call dein#add('szw/vim-tags')
call dein#add('terryma/vim-expand-region')
call dein#add('tpope/vim-surround')
call dein#add('tsukkee/unite-tag')
call dein#add('vim-utils/vim-man')

call dein#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => general
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype plugin indent on

syntax enable

set autoread

set autowriteall

let mapleader=","

nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>

set encoding=utf8
set fileformats=unix,dos,mac

set clipboard=unnamed,unnamedplus

set expandtab

set smarttab

set shiftwidth=2
set tabstop=2

set nowrap
set nolinebreak
set textwidth=80

set autoindent
set smartindent

autocmd filetype markdown setlocal wrap linebreak
autocmd bufnewfile,bufread * setlocal formatoptions-=cro


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set scrolloff=7
set sidescrolloff=7
set sidescroll=1

set wildmenu

set wildignore=*.o,*.pyc

set ruler

set cmdheight=2

set hidden

set backspace=indent,eol,start
set whichwrap+=<,>,h,l

set mouse=a

set ignorecase

set smartcase

set hlsearch

set incsearch

set lazyredraw

set magic

set showmatch

set noerrorbells
set novisualbell
set tm=500

set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=1
set foldcolumn=1

set number

" set completeopt=menu,preview,longest
set completeopt-=preview

set cursorline

set colorcolumn=80

set linespace=3


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => colors, gui and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

"""""""""" bubblegum
" colorscheme bubblegum-256-dark


"""""""""" seoul256 dark
" Range:   233 (darkest) ~ 239 (lightest)
" Default: 237
" set background=dark
" let g:seoul256_background=235
" colorscheme seoul256


"""""""""" seoul256 light
" Range:   252 (darkest) ~ 256 (lightest)
" Default: 253
" set background=light
" let g:seoul256_background=252
" colorscheme seoul256-light


"""""""""" oceanic-next
" set background=dark
" colorscheme OceanicNext


"""""""""" gruvbox dark
set background=dark
let g:gruvbox_contrast_dark='soft'

"""""""""" gruvbox light
" set background=light
" let g:gruvbox_contrast_light='soft'

let g:gruvbox_bold=0
let g:gruvbox_italic=1
let g:gruvbox_invert_signs=0
let g:gruvbox_invert_selection=1
let g:gruvbox_improved_strings=0
let g:gruvbox_improved_warnings=1

colorscheme gruvbox


if has('gui_running')
  set guioptions-=l
  set guioptions-=L
  set guioptions-=r
  set guioptions-=R
  set guioptions-=m
  set guioptions-=T
  set guifont=Source\ Code\ Pro\ 12
  set guifontwide=Source\ Han\ Sans\ CN\ 12

  set lines=38
  set columns=189
endif

map<f11> :call system("wmctrl -ir " . v:windowid . " -b toggle,fullscreen")<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set noswapfile

set undolevels=5000
set undodir=/home/chaomai/Documents/Codes/Current/GitHub/dotfiles/neovim/vim_undo_files
set undofile

set updatecount=10


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <silent><bs> :nohlsearch<cr>

map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <leader>t<leader> :tabnext
map <leader>t? :map <leader>t <cr>

map <leader>1 :tabnext1<cr>
map <leader>2 :tabnext2<cr>
map <leader>3 :tabnext3<cr>
map <leader>4 :tabnext4<cr>
map <leader>5 :tabnext5<cr>
map <leader>6 :tabnext6<cr>
map <leader>7 :tabnext7<cr>
map <leader>8 :tabnext8<cr>
map <leader>9 :tabnext9<cr>

map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

set switchbuf=useopen,usetab,newtab

autocmd bufreadpost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif

set viminfo=h,'500,<10000,s1000,/1000,:1000


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => status line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" statusline setup
set statusline=%#identifier#
set statusline+=[%f]    " tail of the filename
set statusline+=%*

" display a warning if fileformat isnt unix
set statusline+=%#warningmsg#
set statusline+=%{&ff!='unix'?'['.&ff.']':''}
set statusline+=%*

set statusline+=%h      " help file flag
set statusline+=%y      " filetype

" read only flag
set statusline+=%#identifier#
set statusline+=%r
set statusline+=%*

" modified flag
set statusline+=%#warningmsg#
set statusline+=%m
set statusline+=%*

set statusline+=%{fugitive#statusline()}

" display a warning if &et is wrong, or we have mixed-indenting
set statusline+=%#error#
set statusline+=%{StatuslineTabWarning()}
set statusline+=%*

set statusline+=%{StatuslineTrailingSpaceWarning()}
" set statusline+=%{StatuslineLongLineWarning()}

" display a warning if &paste or &spell is set
set statusline+=%#error#
set statusline+=%{&paste?'[paste]':''}
set statusline+=%{&spell?'[spell]':''}
set statusline+=%*

set statusline+=%=      " left/right separator
" current highlight
" set statusline+=%{StatuslineCurrentHighlight()}
set statusline+=\ %c,   " cursor column
set statusline+=%l/%L   " cursor line/total lines
set statusline+=\ %P    " percent through file

" always show the status line
set laststatus=2

" recalculate the tab warning flag when idle and after writing
autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning

" return '[&et]' if &et is set wrong
" return '[mixed-indenting]' if spaces and tabs are used to indent
" return an empty string if everything is fine
func! StatuslineTabWarning()
  if !exists("b:statusline_tab_warning")
    let b:statusline_tab_warning=''

    if !&modifiable
      return b:statusline_tab_warning
    endif

    let tabs=search('^\t', 'nw') != 0

    "find spaces that arent used as alignment in the first indent column
    let spaces=search('^ \{' . &ts . ',}[^\t]', 'nw') != 0

    if tabs && spaces
      let b:statusline_tab_warning='[mixed-indenting]'
    elseif (spaces && !&et) || (tabs && &et)
      let b:statusline_tab_warning='[&et]'
    endif
  endif
  return b:statusline_tab_warning
endfunc

" return '[\s]' if trailing white space is detected
" return '' otherwise
func! StatuslineTrailingSpaceWarning()
  if !exists("b:statusline_trailing_space_warning")

    if !&modifiable
      let b:statusline_trailing_space_warning=''
      return b:statusline_trailing_space_warning
    endif

    if search('\s\+$', 'nw') != 0
      let b:statusline_trailing_space_warning='[\s]'
    else
      let b:statusline_trailing_space_warning=''
    endif
  endif
  return b:statusline_trailing_space_warning
endfunc

" return a warning for "long lines" where "long" is either &textwidth or 80
" (if no &textwidth is set)
"
" return '' if no long lines
"
" return '[#x,my,$z] if long lines are found,
" were x is the number of long lines,
" y is the median length of the long lines
" and z is the length of the longest line
function! StatuslineLongLineWarning()
  if !exists("b:statusline_long_line_warning")

    if !&modifiable
      let b:statusline_long_line_warning=''
      return b:statusline_long_line_warning
    endif

    let long_line_lens=s:LongLines()

    if len(long_line_lens) > 0
      let b:statusline_long_line_warning="[" .
            \ '#' . len(long_line_lens) . "," .
            \ 'm' . s:Median(long_line_lens) . "," .
            \ '$' . max(long_line_lens) . "]"
    else
      let b:statusline_long_line_warning=""
    endif
  endif
  return b:statusline_long_line_warning
endfunc

" return a list containing the lengths of the long lines in this buffer
function! s:LongLines()
  let threshold=(&tw ? &tw : 80)
  let spaces=repeat(" ", &ts)
  let line_lens=map(getline(1,'$'), 'len(substitute(v:val, "\\t", spaces, "g"))')
  return filter(line_lens, 'v:val > threshold')
endfunc

" find the median of the given array of numbers
function! s:Median(nums)
  let nums=sort(a:nums)
  let l=len(nums)

  if l % 2==1
    let i=(l-1) / 2
    return nums[i]
  else
    return (nums[l/2] + nums[(l/2)-1]) / 2
  endif
endfunc

" return the syntax highlight group under the cursor ''
function! StatuslineCurrentHighlight()
  let name=synIDattr(synID(line('.'),col('.'),1),'name')
  if name==''
    return ''
  else
    return '[' . name . ']'
  endif
endfunc


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>pp :setlocal paste!<cr>

func! TrimWhitespace()
  let l:winview=winsaveview()
  %s/\s\+$//e
  call winrestview(l:winview)
endfunc

autocmd bufwrite * :call TrimWhitespace()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <leader>ss :setlocal spell!<cr>

nmap <leader>sn :echo 'Next misspelled word' <bar> ]s<cr>
nmap <leader>sp [s
nmap <leader>sa zg
nmap <leader>sg z=
nmap <leader>s? :map <leader>s<cr>

autocmd filetype svn, *commit* setlocal spell


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""" YouCompleteMe
let g:ycm_filetype_blacklist = {
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'markdown' : 1,
      \ 'unite' : 1,
      \ 'text' : 1,
      \ 'vimwiki' : 1,
      \ 'pandoc' : 1,
      \ 'infolog' : 1,
      \ 'mail' : 1,
      \ 'help' : 1
      \}

let g:ycm_error_symbol='✖'
let g:ycm_warning_symbol='⚑'
let g:ycm_always_populate_location_list=1
let g:ycm_complete_in_comments=0
let g:ycm_complete_in_strings=0
let g:ycm_collect_identifiers_from_tags_files=1
let g:ycm_server_python_interpreter='/usr/bin/python'
let g:ycm_seed_identifiers_with_syntax=1
let g:ycm_autoclose_preview_window_after_completion=1
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_key_list_select_completion=['<c-j>', '<c-n>', '<down>']
let g:ycm_key_list_previous_completion=['<c-k>', '<c-p>', '<up>']
let g:ycm_global_ycm_extra_conf='/home/chaomai/Documents/Codes/Current/GitHub/dotfiles/neovim/ycm_extra_conf.py'
let g:ycm_extra_conf_globlist=['/home/chaomai/Documents/Codes/Current/GitHub/dotfiles/neovim/*']
let g:ycm_confirm_extra_conf=1
let g:ycm_cache_omnifunc=0
let g:ycm_use_ultisnips_completer=0
let g:ycm_goto_buffer_command='vertical-split'
let g:ycm_disable_for_files_larger_than_kb=1024

nnoremap <f5> :YcmForceCompileAndDiagnostics<cr>
nnoremap <leader>ji :YcmCompleter GoToInclude<cr>
nnoremap <leader>jd :YcmCompleter GoToDeclaration<cr>
nnoremap <leader>j :YcmCompleter GoTo<cr>
nnoremap <leader>gt :YcmCompleter GetType<cr>
nnoremap <leader>gp :YcmCompleter GetParent<cr>
nnoremap <leader>gd :YcmCompleter GetDoc<cr>

nnoremap <leader>g? :nnoremap <leader>g<cr>
nnoremap <leader>j? :nnoremap <leader>j<cr>


"""""""""" tagbar
let g:tagbar_compact=1
let g:tagbar_width=32

let g:tagbar_type_c={
      \ 'kinds': [
      \ 'd:macros:1:0',
      \ 'p:prototypes:1:0',
      \ 'g:enums',
      \ 'e:enumerators:0:0',
      \ 't:typedefs:0:0',
      \ 'n:namespaces',
      \ 'c:classes',
      \ 's:structs',
      \ 'u:unions',
      \ 'f:functions',
      \ 'm:members:0:0',
      \ 'v:variables:0:0',
      \ 'x:external:0:0',
      \ 'l:local:0:0'
      \ ]
      \ }

let g:tagbar_type_cpp={
      \ 'kinds': [
      \ 'd:macros:1:0',
      \ 'p:prototypes:1:0',
      \ 'g:enums',
      \ 'e:enumerators:0:0',
      \ 't:typedefs:0:0',
      \ 'n:namespaces',
      \ 'c:classes',
      \ 's:structs',
      \ 'u:unions',
      \ 'f:functions',
      \ 'm:members:0:0',
      \ 'v:variables:0:0',
      \ 'x:external:0:0',
      \ 'l:local:0:0'
      \ ]
      \ }

let g:tagbar_type_scala={
      \ 'ctagstype': 'scala',
      \ 'sro': '.',
      \ 'kinds': [
      \ 'p:packages',
      \ 'T:types:1',
      \ 't:traits',
      \ 'o:objects',
      \ 'O:case objects',
      \ 'c:classes',
      \ 'C:case classes',
      \ 'm:methods',
      \ 'V:values:1',
      \ 'v:variables:1'
      \ ]
      \ }

let g:tagbar_type_rust={
      \ 'ctagstype': 'rust',
      \ 'kinds': [
      \ 'T:types,type definitions',
      \ 'f:functions,function definitions',
      \ 'g:enum,enumeration names',
      \ 's:structure names',
      \ 'm:modules,module names',
      \ 'c:consts,static constants',
      \ 't:traits,traits',
      \ 'i:impls,trait implementations',
      \ ]
      \ }

let g:tagbar_type_go={
      \ 'ctagstype': 'go',
      \ 'kinds': [
      \ 'p:package',
      \ 'i:imports:1',
      \ 'c:constants',
      \ 'v:variables',
      \ 't:types',
      \ 'n:interfaces',
      \ 'w:fields',
      \ 'e:embedded',
      \ 'm:methods',
      \ 'r:constructor',
      \ 'f:functions'
      \ ],
      \ 'sro': '.',
      \ 'kind2scope': {
      \ 't': 'ctype',
      \ 'n': 'ntype'
      \ },
      \ 'scope2kind': {
      \ 'ctype': 't',
      \ 'ntype': 'n'
      \ },
      \ 'ctagsbin': 'gotags',
      \ 'ctagsargs': '-sort -silent'
      \ }

map <f3> :TagbarToggle<cr>

autocmd quitpre * TagbarClose
" autocmd vimenter * nested :call tagbar#autoopen(1)
" autocmd filetype * nested :call tagbar#autoopen(0)
" autocmd bufenter * nested :call tagbar#autoopen(0)


"""""""""" nerdcommenter
let g:NERDSpaceDelims=1

map <leader>c? :map <leader>c<cr>


"""""""""" nerdtree
let g:NERDTreeMinimalUI=1
let g:NERDTreeWinSize=32
let g:NERDTreeAutoDeleteBuffer=1
let NERDTreeShowHidden=1

map <f2> :NERDTreeToggle<cr>
" autocmd vimenter * NERDTreeToggle
autocmd quitpre * NERDTreeClose

" NERDTress File highlighting
func! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunc


"""""""""" python syntax
let python_highlight_all=1


"""""""""" vim-autoformat
func! Format()
  let l:winview=winsaveview()
  Autoformat
  call winrestview(l:winview)
endfunc

map <leader>af :call Format()<cr>

let g:formatdef_google_clang_format='"clang-format -style=Google"'
let g:formatters_cpp=['google_clang_format']
let g:formatters_c=['google_clang_format']
let g:formatters_h=['google_clang_format']

let g:formatdef_yapf_format='"yapf --style=google"'
let g:formatters_python=['yapf_format']


"""""""""" vim-easymotion
map <leader><leader>l <Plug>(easymotion-lineforward)
map <leader><leader>j <Plug>(easymotion-j)
map <leader><leader>k <Plug>(easymotion-k)
map <leader><leader>h <Plug>(easymotion-linebackward)
map <leader><leader>. <Plug>(easymotion-repeat)

" keep cursor column when JK motion
let g:EasyMotion_startofline=0

let g:EasyMotion_smartcase=1


"""""""""" buffergator
" Use the right side of the screen
let g:buffergator_viewport_split_policy='R'

" Disable default keys
let g:buffergator_suppress_keymaps=1

let g:buffergator_autoexpand_on_split=0

" Go to the previous buffer open
nmap <leader>jj :BuffergatorMruCyclePrev<cr>

" Go to the next buffer open
nmap <leader>kk :BuffergatorMruCycleNext<cr>

" Toggle the entire list of buffers open
nmap <f1> :BuffergatorToggle<cr>

nmap <leader>en :enew<cr>
nmap <leader>bq :bp <BAR> bd #<cr>


"""""""""" vim-tags
set tags=./tags
let g:vim_tags_ignore_files=['.gitignore', '.svnignore', '.cvsignore']
let g:vim_tags_directories=['CMakeLists.txt', 'Makefile']
let g:vim_tags_use_language_field=0
let g:vim_tags_project_tags_command='{CTAGS} -R --c-kinds=+p+l+x+c+d+e+f+g+m+n+s+t+u+v --c++-kinds=+p+l+x+c+d+e+f+g+m+n+s+t+u+v --fields=+iaSl --extra=+q {OPTIONS} {DIRECTORY} 2>/dev/null'
nmap <leader>tg :TagsGenerate!<cr>


"""""""""" vim-json
let g:vim_json_syntax_conceal=0


"""""""""" vim ack
nnoremap <leader>a :Ack


"""""""""" vim-gitgutter
let g:gitgutter_max_signs=5000
let g:gitgutter_enabled=0
let g:gitgutter_map_keys=0

map <leader>vg :GitGutterToggle<cr>


"""""""""" rainbow
" if has('gui_running')
" let g:rainbow_active=1
" let g:rainbow_conf = {
" \ 'guifgs': ['firebrick', 'brown1', 'gold3', 'purple4', 'blue', 'green'],
" \ }
" \ 'ctermfgs': ['red', 'bisque4']
" endif


"""""""""" indentLine
" ¦, ┆, or │
let g:indentLine_char='┊'
let g:indentLine_concealcursor=''
let g:indentLine_conceallevel=2
