"======================================================================
"
" init-plugins.vim -
"
" Created by skywind on 2018/05/31
" Last Modified: 2018/06/10 23:11
"
"======================================================================
" vim: set ts=4 sw=4 tw=78 noet :


"----------------------------------------------------------------------
" 默认情况下的分组，可以再前面覆盖之
"----------------------------------------------------------------------
if !exists("g:bundle_group")
    let g:bundle_group = ["basic", "enhanced", "filetypes", "textobj"]
    let g:bundle_group += ["lightline", "vimautoformat"]
    let g:bundle_group += ["fzf", "chadtree", "cocnvim"]
endif


"----------------------------------------------------------------------
" 计算当前 vim-init 的子路径
"----------------------------------------------------------------------
let s:home = fnamemodify(resolve(expand("<sfile>:p")), ":h:h")

function! s:path(path)
    let path = expand(s:home . "/" . a:path )
    return substitute(path, "\\", "/", "g")
endfunc


"----------------------------------------------------------------------
" 在 ~/.vim/bundles 下安装插件
"----------------------------------------------------------------------
if has("nvim")
    call plug#begin(stdpath("data") . "/plugged")
else
    call plug#begin(get(g:, "bundle_home", "~/.vim/bundles"))
endif


"----------------------------------------------------------------------
" 默认插件
"----------------------------------------------------------------------

" 全文快速移动，<leader><leader>f{char} 即可触发
Plug 'easymotion/vim-easymotion'

" 表格对齐，使用命令 Tabularize
" :<range>Tabularize \{characters}
Plug 'godlygeek/tabular', { 'on': 'Tabularize' }

" Diff 增强，支持 histogram / patience 等更科学的 diff 算法
Plug 'chrisbra/vim-diff-enhanced'


"----------------------------------------------------------------------
" 基础插件
"----------------------------------------------------------------------
if index(g:bundle_group, "basic") >= 0

    " 展示开始画面，显示最近编辑过的文件
    Plug 'mhinz/vim-startify'

    " 支持库，给其他插件用的函数库
    Plug 'xolox/vim-misc'

    " 用于在侧边符号栏显示 marks （ma-mz 记录的位置）
    Plug 'kshenoy/vim-signature'

    " 用于在侧边符号栏显示 git/svn 的 diff
    Plug 'mhinz/vim-signify'

    " 使用 ALT+e 会在不同窗口 / 标签上显示 A/B/C 等编号，然后字母直接跳转
    Plug 't9md/vim-choosewin'

    " 提供基于 TAGS 的定义预览，函数参数预览，quickfix 预览
    " Plug 'skywind3000/vim-preview'

    " Git 支持
    " Plug 'tpope/vim-fugitive'

    " indentLine
    Plug 'Yggdroot/indentLine'

    " vim with tmux
    Plug 'christoomey/vim-tmux-navigator'

    "==================================================================
    " vim-choosewin
    " 使用 ALT+E 来选择窗口
    nmap <m-e> <Plug>(choosewin)

    "==================================================================
    " startify
    let g:startify_session_dir = expand("~/.vim/session")
    let g:startify_change_to_dir = 0

    "==================================================================
    " signify 调优
    let g:signify_vcs_list               = ["git", "svn"]
    let g:signify_sign_add               = "+"
    let g:signify_sign_delete            = "_"
    let g:signify_sign_delete_first_line = "‾"
    let g:signify_sign_change            = "~"
    let g:signify_sign_changedelete      = g:signify_sign_change

    " git 仓库使用 histogram 算法进行 diff
    let g:signify_vcs_cmds = {
                \ "git": "git diff --no-color --diff-algorithm=histogram --no-ext-diff -U0 -- %f"
                \}

    "==================================================================
    " indentLine 设置
    let g:indentLine_char_list = ["¦", "┆", "┊", "│"]

    "==================================================================
    " vim-tmux-navigator 设置
    let g:tmux_navigator_no_mappings = 1
    " update (write the current buffer, but only if changed)
    let g:tmux_navigator_save_on_switch = 1

    noremap <silent><m-H> :TmuxNavigateLeft<cr>
    noremap <silent><m-J> :TmuxNavigateDown<cr>
    noremap <silent><m-K> :TmuxNavigateUp<cr>
    noremap <silent><m-L> :TmuxNavigateRight<cr>
    inoremap <silent><m-H> <esc> <bar> :TmuxNavigateLeft<cr>
    inoremap <silent><m-J> <esc> <bar> :TmuxNavigateDown<cr>
    inoremap <silent><m-K> <esc> <bar> :TmuxNavigateUp<cr>
    inoremap <silent><m-L> <esc> <bar> :TmuxNavigateRight<cr>
endif


"----------------------------------------------------------------------
" 增强插件
"----------------------------------------------------------------------
if index(g:bundle_group, "enhanced") >= 0

    " 用 v 选中一个区域后，ALT_+/- 按分隔符扩大 / 缩小选区
    Plug 'terryma/vim-expand-region'

    " 给不同语言提供字典补全，插入模式下 c-x c-k 触发
    Plug 'skywind3000/vim-dict'

    " 使用 :CtrlSF 命令进行模仿 sublime 的 grep
    Plug 'dyng/ctrlsf.vim'

    " 配对括号和引号自动补全
    Plug 'Raimondi/delimitMate'

    " nerd commenter
    Plug 'scrooloose/nerdcommenter'

    " snipMate & UltiSnip Snippets 供 coc-snippets 使用
    Plug 'honza/vim-snippets'

    " colorscheme
    Plug 'joshdick/onedark.vim'
    Plug 'liuchengxu/space-vim-theme'
    Plug 'kristijanhusak/vim-hybrid-material'
    Plug 'NLKNguyen/papercolor-theme'
    Plug 'rakr/vim-one'
    Plug 'ayu-theme/ayu-vim'
    Plug 'lifepillar/vim-solarized8'
    Plug 'arcticicestudio/nord-vim'
    Plug 'AlessandroYorba/Arcadia'
    Plug 'AlessandroYorba/Sierra'

    " file type icons
    Plug 'ryanoasis/vim-devicons'

    " quickui
    Plug 'skywind3000/vim-quickui'

    " vim-visual-multi
    Plug 'mg979/vim-visual-multi'

    " pangu
    Plug 'hotoo/pangu.vim'

    " floaterm
    Plug 'voldikss/vim-floaterm'

    "==================================================================
    " vim expand region 设置
    " ALT_+/- 用于按分隔符扩大缩小 v 选区
    map <m-=> <Plug>(expand_region_expand)
    map <m--> <Plug>(expand_region_shrink)

    " ==================================================================
    " nerd commenter 设置
    let g:NERDSpaceDelims = 1
    let g:NERDCreateDefaultMappings = 0
    map <space>cl <Plug>NERDCommenterInvert
    map <space>cs <Plug>NERDCommenterSexy
    map <space>ct <Plug>NERDCommenterToggle

    "==================================================================
    " quickui 设置
    " Default preview window size (more lines and width)
    let g:quickui_preview_w = 100
    let g:quickui_preview_h = 25
    let g:quickui_border_style = 2

    " color
    " let g:quickui_color_scheme = "system"
    highlight link QuickPreview Normal

    "==================================================================
    " vim-visual-multi 设置
    let g:VM_maps = {}
    let g:VM_maps['Find Under']         = '<space><c-d>'
    let g:VM_maps['Find Subword Under'] = '<space><c-d>'
    let g:VM_maps["Exit"]               = '<esc>'
endif


"----------------------------------------------------------------------
" 自动生成 ctags/gtags，并提供自动索引功能
" 不在 git/svn 内的项目，需要在项目根目录 touch 一个空的 .root 文件
" 详细用法见：https://zhuanlan.zhihu.com/p/36279445
"----------------------------------------------------------------------
if index(g:bundle_group, "tags") >= 0

    " 提供 ctags/gtags 后台数据库自动更新功能
    Plug 'ludovicchabant/vim-gutentags'

    " 提供 GscopeFind 命令并自动处理好 gtags 数据库切换
    " 支持光标移动到符号名上：<leader>cg 查看定义，<leader>cs 查看引用
    Plug 'skywind3000/gutentags_plus'

    " 设定项目目录标志：除了 .git/.svn 外，还有 .root 文件
    let g:gutentags_project_root = g:project_root
    let g:gutentags_ctags_tagfile = ".tags"

    " 默认生成的数据文件集中到 ~/.cache/tags 避免污染项目目录，好清理
    let g:gutentags_cache_dir = expand("~/.cache/tags")

    " 默认禁用自动生成
    let g:gutentags_modules = []

    " 如果有 ctags 可执行就允许动态生成 ctags 文件
    if executable("ctags")
        let g:gutentags_modules += ["ctags"]
    endif

    " 如果有 gtags 可执行就允许动态生成 gtags 数据库
    if executable("gtags") && executable("gtags-cscope")
        let g:gutentags_modules += ["gtags_cscope"]
    endif

    " 设置 ctags 的参数
    let g:gutentags_ctags_extra_args = []
    let g:gutentags_ctags_extra_args = ["--fields=+niazS", "--extra=+q"]
    let g:gutentags_ctags_extra_args += ["--c++-kinds=+px"]
    let g:gutentags_ctags_extra_args += ["--c-kinds=+px"]

    " 使用 universal-ctags 的话需要下面这行，请反注释
    " let g:gutentags_ctags_extra_args += ["--output-format=e-ctags"]

    " 禁止 gutentags 自动链接 gtags 数据库
    let g:gutentags_auto_add_gtags_cscope = 0
endif


"----------------------------------------------------------------------
" 文本对象：textobj 全家桶
"----------------------------------------------------------------------
if index(g:bundle_group, "textobj") >= 0

    " 基础插件：提供让用户方便的自定义文本对象的接口
    Plug 'kana/vim-textobj-user'

    " indent 文本对象：ii/ai 表示当前缩进，vii 选中当缩进，cii 改写缩进
    Plug 'kana/vim-textobj-indent'

    " 语法文本对象：iy/ay 基于语法的文本对象
    Plug 'kana/vim-textobj-syntax'

    " 函数文本对象：if/af 支持 c/c++/vim/java
    Plug 'kana/vim-textobj-function', { 'for':['c', 'cpp', 'vim', 'java'] }

    " 参数文本对象：i,/a, 包括参数或者列表元素
    Plug 'sgur/vim-textobj-parameter'

    " 提供 python 相关文本对象，if/af 表示函数，ic/ac 表示类
    Plug 'bps/vim-textobj-python',  {'for': 'python' }

    " 提供 uri/url 的文本对象，iu/au 表示
    Plug 'jceb/vim-textobj-uri'
endif


"----------------------------------------------------------------------
" 文件类型扩展
"----------------------------------------------------------------------
if index(g:bundle_group, "filetypes") >= 0

    " powershell 脚本文件的语法高亮
    " Plug 'pprovost/vim-ps1', { 'for': 'ps1' }

    " lua 语法高亮增强
    Plug 'tbastos/vim-lua', { 'for': 'lua' }

    " C++ 语法高亮增强，支持 11/14/17 标准
    " Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }

    " 额外语法文件
    " Plug 'justinmk/vim-syntax-extra', { 'for': ['c', 'bison', 'flex', 'cpp'] }

    " python 语法文件增强
    Plug 'vim-python/python-syntax', { 'for': ['python'] }

    " rust 语法增强
    Plug 'rust-lang/rust.vim', { 'for': 'rust' }

    " markdown
    Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }

    " org mode
    Plug 'jceb/vim-orgmode', { 'for': 'org' }

    "==================================================================
    " vim markdown 设置
    let g:vim_markdown_folding_disabled = 1
    let g:vim_markdown_conceal = 0
endif


"----------------------------------------------------------------------
" airline
"----------------------------------------------------------------------
if index(g:bundle_group, "airline") >= 0
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    let g:airline_left_sep = ""
    let g:airline_left_alt_sep = ""
    let g:airline_right_sep = ""
    let g:airline_right_alt_sep = ""
    let g:airline_powerline_fonts = 0
    let g:airline_exclude_preview = 1
    let g:airline_section_b = "%n"
    let g:airline_theme="hybrid"
    let g:airline#extensions#branch#enabled = 0
    let g:airline#extensions#csv#enabled = 0
    let g:airline#extensions#fugitiveline#enabled = 0
    let g:airline#extensions#syntastic#enabled = 0
    let g:airline#extensions#vimagit#enabled = 0
endif


"----------------------------------------------------------------------
" lightline
"----------------------------------------------------------------------
if index(g:bundle_group, "lightline") >= 0
    Plug 'itchyny/lightline.vim'
    set noshowmode

    autocmd User CocDiagnosticChange call lightline#update()

    function! CocCurrentFunction()
        return get(b:, "coc_current_function", "")
    endfunction

    function! LightlineFilename()
        let filename = expand("%:t") !=# "" ? expand("%:t") : "[No Name]"
        let modified = &modified ? " +" : ""
        return filename . modified
    endfunction

    function! s:is_show()
        return &filetype !~# "\v(help|defx)"
    endfunction

    function! s:light_line_left() abort
        let left = [
                    \ [ "mode", "paste", "gitbranch" ],
                    \ [ "filename", "currentfunction" ],
                    \ ]

        if <SID>is_show()
            return left
        else
            return []
        endif
    endfunction

    function! s:light_line_right() abort
        let right = [
                    \ [ "lineinfo" ],
                    \ [ "percent" ],
                    \ [ "readonly", "fileformat", "fileencoding", "filetype", "cocstatus" ],
                    \ ]

        if <SID>is_show()
            return right
        else
            return []
        endif
    endfunction

    let g:lightline = {
                \     "colorscheme": "PaperColor_dark",
                \     "active": {
                \       "left": <SID>light_line_left(),
                \       "right": <SID>light_line_right(),
                \     },
                \     "component_function": {
                \       "filename": "LightlineFilename",
                \       "gitbranch": "FugitiveHead",
                \       "cocstatus": "coc#status",
                \       "currentfunction": "CocCurrentFunction"
                \     },
                \     'separator': { 'left': '', 'right': '' },
                \     'subseparator': { 'left': '│', 'right': '│' }
                \ }
endif


"----------------------------------------------------------------------
" NERDTree
"----------------------------------------------------------------------
if index(g:bundle_group, "nerdtree") >= 0
    Plug 'scrooloose/nerdtree', { 'on': ['NERDTree', 'NERDTreeFocus', 'NERDTreeToggle', 'NERDTreeCWD', 'NERDTreeFind'] }
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

    let g:NERDTreeMinimalUI = 1
    let g:NERDTreeDirArrows = 1
    let g:NERDTreeHijackNetrw = 0

    noremap <space>nn :NERDTree<cr>
    noremap <space>no :NERDTreeFocus<cr>
    noremap <space>nm :NERDTreeMirror<cr>
    noremap <space>nt :NERDTreeToggle<cr>
endif


"----------------------------------------------------------------------
" LanguageTool 语法检查
"----------------------------------------------------------------------
if index(g:bundle_group, "grammer") >= 0
    Plug 'rhysd/vim-grammarous'

    noremap <space>rg :GrammarousCheck --lang=en-US --no-move-to-first-error --no-preview<cr>
    map <space>rr <Plug>(grammarous-open-info-window)
    map <space>rv <Plug>(grammarous-move-to-info-window)
    map <space>rs <Plug>(grammarous-reset)
    map <space>rx <Plug>(grammarous-close-info-window)
    map <space>rm <Plug>(grammarous-remove-error)
    map <space>rd <Plug>(grammarous-disable-rule)
    map <space>rn <Plug>(grammarous-move-to-next-error)
    map <space>rp <Plug>(grammarous-move-to-previous-error)
endif


"----------------------------------------------------------------------
" ale 动态语法检查
"----------------------------------------------------------------------
if index(g:bundle_group, "ale") >= 0
    Plug 'w0rp/ale'

    " 设定延迟和提示信息
    let g:ale_completion_delay = 500
    let g:ale_echo_delay = 20
    let g:ale_lint_delay = 100
    let g:ale_echo_msg_format = "[%linter%] %code: %%s"

    " 设定检测的时机：normal 模式文字改变，或者离开 insert 模式
    " 禁用默认 INSERT 模式下改变文字也触发的设置，太频繁外，还会让补全窗闪烁
    let g:ale_lint_on_text_changed = "normal"
    let g:ale_lint_on_insert_leave = 1

    " 在 linux/mac 下降低语法检查程序的进程优先级（不要卡到前台进程）
    if has("win32") == 0 && has("win64") == 0 && has("win32unix") == 0
        let g:ale_command_wrapper = "nice -n5"
    endif

    " 允许 airline 集成
    let g:airline#extensions#ale#enabled = 0

    " error 信息填充 quickfix
    let g:ale_set_quickfix = 1

    " 编辑不同文件类型需要的语法检查器
    let g:ale_linters =
                \ {
                \     "c": ["ccls"],
                \     "cpp": ["ccls"],
                \     "python": ["flake8", "pylint"],
                \     "lua": ["luac"],
                \     "go": ["gopls"],
                \     "java": ["javac"],
                \     "javascript": ["eslint"]
                \ }

    "==================================================================
    " cpp
    let g:ale_cpp_ccls_executable = expand("~/Documents/workspace/github/ccls/Release/ccls")

    let g:ale_cpp_ccls_init_options =
                \ {
                \     "capabilities": {
                \         "foldingRangeProvider": v:false
                \     },
                \     "cache": {
                \         "directory": ".ccls-cache"
                \     },
                \     "completion": {
                \         "caseSensitivity": 0
                \     },
                \     "compilationDatabaseDirectory": "cmake-build",
                \     "codeLens": {
                \         "localVariables": v:false
                \     },
                \     "client": {
                \         "snippetSupport": v:true
                \     },
                \     "diagnostics": {
                \         "onChange": 100,
                \         "onOpen": 100,
                \         "onSave": 100
                \     },
                \     "highlight": {
                \         "lsRanges" : v:true
                \     },
                \     "index": {
                \         "threads": 5
                \     }
                \ }

    "==================================================================
    " python
    " 获取 pylint, flake8 的配置文件，在 vim-init/tools/conf 下面
    function s:lintcfg(name)
        let conf = s:path("tools/conf/")
        let path1 = conf . a:name
        let path2 = expand("~/.vim/linter/". a:name)
        if filereadable(path2)
            return path2
        endif
        return shellescape(filereadable(path2)? path2 : path1)
    endfunc

    " 设置 flake8/pylint 的参数
    let g:ale_python_flake8_options = "--conf=".s:lintcfg("flake8.conf")
    let g:ale_python_pylint_options = "--rcfile=".s:lintcfg("pylint.conf")
    let g:ale_python_pylint_options .= " --disable=W"
    let g:ale_c_gcc_options = "-Wall -O2 -std=c99"
    let g:ale_cpp_gcc_options = "-Wall -O2 -std=c++17"
    let g:ale_c_cppcheck_options = ""
    let g:ale_cpp_cppcheck_options = ""

    let g:ale_linters.text = ["textlint", "write-good", "languagetool"]
endif


"----------------------------------------------------------------------
" FZF 文件模糊匹配
"----------------------------------------------------------------------
if index(g:bundle_group, "fzf") >= 0
    Plug 'junegunn/fzf', { 'do': './install --bin' }
    Plug 'junegunn/fzf.vim'

    let g:fzf_preview_window = "right:70%"
    let g:fzf_layout = { "window": { "width": 0.9, "height": 0.8 } }
    let g:fzf_command_prefix = "FZF"

    " ALT+p 打开文件模糊匹配
    nnoremap <m-p> :FZFFiles<cr>

    " ALT+p 打开 buffer 模糊匹配
    nnoremap <m-b> :FZFBuffers<cr>

    " ALT+n rg 搜索
    function! FZFRipgrep(query, fullscreen)
        let command_fmt = "rg --column --line-number --no-heading --color=always --smart-case %s || true"
        let initial_command = printf(command_fmt, shellescape(a:query))
        let reload_command = printf(command_fmt, "{q}")
        let spec = {"options": ["--phony", "--query", a:query, "--bind", "change:reload:".reload_command]}
        call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
    endfunction

    command! -nargs=* -bang FZFRG call FZFRipgrep(<q-args>, <bang>0)
    nnoremap <m-n> :FZFRG<cr>

    " ALT+w window 搜索
    nnoremap <m-w> :FZFWindows<cr>

    " ALT+m marks 搜索
    nnoremap <m-m> :FZFMarks<cr>

    " ALT+t task 搜索
    " nnoremap <m-t> :FZFAsyncTask<cr>
    
    " fzf preview highlight
    augroup update_bat_theme
        autocmd!
        autocmd colorscheme * call ToggleBatEnvVar()
    augroup end

    function ToggleBatEnvVar()
        if (&background == "light")
            let $BAT_THEME='OneHalfLight'
        else
            let $BAT_THEME='OneHalfDark'
        endif
    endfunction
endif


"----------------------------------------------------------------------
" LeaderF：CtrlP / FZF 的超级代替者，文件模糊匹配，tags/ 函数名 选择
"----------------------------------------------------------------------
if index(g:bundle_group, "leaderf") >= 0
    " 如果 vim 支持 python 则启用  Leaderf
    if has("python") || has("python3")
        Plug 'Yggdroot/LeaderF', { 'do': './install.sh' }

        " CTRL+p 打开文件模糊匹配
        let g:Lf_ShortcutF = "<c-p>"

        " ALT+n 打开 buffer 模糊匹配
        let g:Lf_ShortcutB = "<m-n>"

        " CTRL+n 打开最近使用的文件 MRU，进行模糊匹配
        noremap <c-n> :LeaderfMru<cr>

        " ALT+p 打开函数列表，按 i 进入模糊匹配，ESC 退出
        " noremap <m-p> :LeaderfFunction!<cr>

        " ALT+SHIFT+p 打开 tag 列表，i 进入模糊匹配，ESC 退出
        " noremap <m-P> :LeaderfBufTag!<cr>

        " ALT+n 打开 buffer 列表进行模糊匹配
        noremap <m-n> :LeaderfBuffer<cr>

        " ALT+m 全局 tags 模糊匹配
        " noremap <m-m> :LeaderfTag<cr>

        " 最大历史文件保存 2048 个
        let g:Lf_MruMaxFiles = 2048

        " ui 定制
        let g:Lf_StlSeparator = { "left": "|", "right": "|", "font": "" }

        " 如何识别项目目录，从当前文件目录向父目录递归知道碰到下面的文件 / 目录
        let g:Lf_RootMarkers = g:project_root
        let g:Lf_WorkingDirectoryMode = "Ac"
        let g:Lf_WindowHeight = 0.30
        let g:Lf_CacheDirectory = expand("~/.vim/cache")

        " 显示绝对路径
        let g:Lf_ShowRelativePath = 0

        " 隐藏帮助
        let g:Lf_HideHelp = 1

        " 模糊匹配忽略扩展名
        let g:Lf_WildIgnore =
                    \ {
                    \     "dir": [".svn",".git",".hg"],
                    \     "file": ["*.sw?","~$*","*.bak","*.exe","*.o","*.so","*.py[co]"]
                    \ }

        " MRU 文件忽略扩展名
        let g:Lf_MruFileExclude = ["*.so", "*.exe", "*.py[co]", "*.sw?", "~$*", "*.bak", "*.tmp", "*.dll"]
        let g:Lf_StlColorscheme = "powerline"

        " popup mode
        let g:Lf_WindowPosition = "popup"
        let g:Lf_PreviewInPopup = 1

        " 预览功能，可以手动用 p 预览
        let g:Lf_PreviewResult =
                    \ {
                    \     "File": 0,
                    \     "Buffer": 0,
                    \     "Mru": 0,
                    \     "Tag": 0,
                    \     "BufTag": 1,
                    \     "Function": 1,
                    \     "Line": 0,
                    \     "Colorscheme": 0,
                    \     "Rg": 1,
                    \     "Gtags": 0
                    \}

        " 使用 ESC 键可以直接退出 leaderf 的 normal 模式
        let g:Lf_NormalMap =
                    \ {
                    \     "File":   [["<ESC>", ":exec g:Lf_py "fileExplManager.quit()"<CR>"]],
                    \     "Buffer": [["<ESC>", ":exec g:Lf_py "bufExplManager.quit()"<CR>"]],
                    \     "Mru": [["<ESC>", ":exec g:Lf_py "mruExplManager.quit()"<CR>"]],
                    \     "Tag": [["<ESC>", ":exec g:Lf_py "tagExplManager.quit()"<CR>"]],
                    \     "BufTag": [["<ESC>", ":exec g:Lf_py "bufTagExplManager.quit()"<CR>"]],
                    \     "Function": [["<ESC>", ":exec g:Lf_py "functionExplManager.quit()"<CR>"]]
                    \ }

        " let g:Lf_CommandMap = {"<CR>": ["<C-O>"] ,"<C-T>": ["<CR>"]}

        autocmd filetype leaderf set nonumber

    else
        " 不支持 python ，使用 CtrlP 代替
        Plug 'ctrlpvim/ctrlp.vim'

        " 显示函数列表的扩展插件
        Plug 'tacahiroy/ctrlp-funky'

        " 忽略默认键位
        let g:ctrlp_map = ""

        " 模糊匹配忽略
        let g:ctrlp_custom_ignore =
                    \ {
                    \     "dir":  "\v[\/]\.(git|hg|svn)$",
                    \     "file": "\v\.(exe|so|dll|mp3|wav|sdf|suo|mht)$",
                    \     "link": "some_bad_symbolic_links"
                    \ }

        " 项目标志
        let g:ctrlp_root_markers = g:project_root
        let g:ctrlp_working_path = 0

        " CTRL+p 打开文件模糊匹配
        noremap <c-p> :CtrlP<cr>

        " CTRL+n 打开最近访问过的文件的匹配
        noremap <c-n> :CtrlPMRUFiles<cr>

        " ALT+p 显示当前文件的函数列表
        noremap <m-p> :CtrlPFunky<cr>

        " ALT+n 匹配 buffer
        noremap <m-n> :CtrlPBuffer<cr>
    endif
endif


"----------------------------------------------------------------------
" YouCompleteMe 默认设置：YCM 需要你另外手动编译安装
"----------------------------------------------------------------------
if index(g:bundle_group, "ycm") >= 0
    Plug 'ycm-core/YouCompleteMe'

    " 禁用预览功能：扰乱视听
    let g:ycm_add_preview_to_completeopt = 0

    " 禁用诊断功能：我们用前面更好用的 ALE 代替
    let g:ycm_show_diagnostics_ui = 0
    let g:ycm_server_log_level = "info"
    let g:ycm_min_num_identifier_candidate_chars = 2
    let g:ycm_collect_identifiers_from_comments_and_strings = 1
    let g:ycm_complete_in_strings=1
    let g:ycm_key_invoke_completion = "<c-z>"

    " noremap <c-z> <NOP>

    " 两个字符自动触发语义补全
    let g:ycm_semantic_triggers =
                \ {
                \     "c,cpp,python,java,go,erlang,perl": ["re!\w{2}"],
                \     "cs,lua,javascript": ["re!\w{2}"]
                \ }

    " Ycm 白名单（非名单内文件不启用 YCM），避免打开个 1MB 的 txt 分析半天
    let g:ycm_filetype_whitelist =
                \ {
                \     "c":1,
                \     "cpp":1,
                \     "objc":1,
                \     "objcpp":1,
                \     "python":1,
                \     "java":1,
                \     "javascript":1,
                \     "coffee":1,
                \     "vim":1,
                \     "go":1,
                \     "cs":1,
                \     "lua":1,
                \     "perl":1,
                \     "perl6":1,
                \     "php":1,
                \     "ruby":1,
                \     "rust":1,
                \     "erlang":1,
                \     "asm":1,
                \     "nasm":1,
                \     "masm":1,
                \     "tasm":1,
                \     "asm68k":1,
                \     "asmh8300":1,
                \     "asciidoc":1,
                \     "basic":1,
                \     "vb":1,
                \     "make":1,
                \     "cmake":1,
                \     "html":1,
                \     "css":1,
                \     "less":1,
                \     "json":1,
                \     "cson":1,
                \     "typedscript":1,
                \     "haskell":1,
                \     "lhaskell":1,
                \     "lisp":1,
                \     "scheme":1,
                \     "sdl":1,
                \     "sh":1,
                \     "zsh":1,
                \     "bash":1,
                \     "man":1,
                \     "markdown":1,
                \     "matlab":1,
                \     "maxima":1,
                \     "dosini":1,
                \     "conf":1,
                \     "config":1,
                \     "zimbu":1,
                \     "ps1":1
                \ }
endif


"----------------------------------------------------------------------
" vim-autoformat
"----------------------------------------------------------------------
if index(g:bundle_group, "vimautoformat") >= 0
    Plug 'Chiel92/vim-autoformat'

    nnoremap <space>af :Autoformat<cr>

    "==================================================================
    " cpp
    let s:configfile_def = "'clang-format -lines='.a:firstline.':'.a:lastline.' --assume-filename=\"'.expand('%:p').'\" -style=file'"

    if has("macunix")
        let g:formatdef_clang_format = "'clang-format --lines='.a:firstline.':'.a:lastline.' --assume-filename=\"'.expand('%:p').'\" --style=\"{BasedOnStyle: Google, IndentWidth: 4, AlignTrailingComments: true, SortIncludes: false, '.(&textwidth ? 'ColumnLimit: '.&textwidth.', ' : '').(&expandtab ? 'UseTab: Never, IndentWidth: '.shiftwidth() : 'UseTab: Always').'}\"'"
    else
        let g:formatdef_clang_format = "'clang-format-10 --lines='.a:firstline.':'.a:lastline.' --assume-filename=\"'.expand('%:p').'\" --style=\"{BasedOnStyle: Google, IndentWidth: 4, AlignTrailingComments: true, SortIncludes: false, '.(&textwidth ? 'ColumnLimit: '.&textwidth.', ' : '').(&expandtab ? 'UseTab: Never, IndentWidth: '.shiftwidth() : 'UseTab: Always').'}\"'"
    endif

    let g:formatters_objc = ["clang_format"]
    let g:formatters_cpp = ["clang_format"]
    let g:formatters_c = ["clang_format"]
    let g:formatters_h = ["clang_format"]

    "==================================================================
    " python
    " use default
    let g:formatters_python = ["black"]

    "==================================================================
    " haskell
    " use default
    let g:formatters_haskell = ["stylish_haskell"]
    autocmd FileType haskell let b:autoformat_autoindent=0
endif


"----------------------------------------------------------------------
" asynctask
"----------------------------------------------------------------------
if index(g:bundle_group, "asynctask") >= 0
    Plug 'skywind3000/asynctasks.vim'
    Plug 'skywind3000/asyncrun.vim'

    let g:asyncrun_rootmarks = g:project_root
    let g:asyncrun_open = 6

    let g:asynctasks_extra_config = [
                \     expand("~/Documents/workspace/dotfiles/vim/vim-init/tools/conf/asynctasks.ini")
                \ ]

    " query task in fzf
    function! s:fzf_sink(what)
        let p1 = stridx(a:what, "<")

        if p1 >= 0
            let name = strpart(a:what, 0, p1)
            let name = substitute(name, '^\s*\(.\{-}\)\s*$', '\1', '')
            if name != ""
                exec "AsyncTask ". fnameescape(name)
            endif
        endif
    endfunction

    function! s:fzf_task()
        let rows = asynctasks#source(&columns * 0.9)
        let source = []

        for row in rows
            let name = row[0]
            let source += [name . "  " . row[1] . "  : " . row[2]]
        endfor

        let opts =
                    \ {
                    \     "source": source,
                    \     "sink": function("s:fzf_sink"),
                    \     "options": "+m --nth 1 --inline-info --tac"
                    \ }

        if exists("g:fzf_layout")
            for key in keys(g:fzf_layout)
                let opts[key] = deepcopy(g:fzf_layout[key])
            endfor
        endif

        call fzf#run(opts)
    endfunction

    command! -nargs=0 FZFAsyncTask call s:fzf_task()
endif


"----------------------------------------------------------------------
" cocnvim
"----------------------------------------------------------------------
if index(g:bundle_group, "cocnvim") >= 0
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    " Plug 'jackguo380/vim-lsp-cxx-highlight'

    " Use tab for trigger completion with characters ahead and navigate.
    inoremap <silent><expr> <c-n>
                \ pumvisible() ? coc#_select_confirm() :
                \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
                \ <SID>check_back_space() ? "\<TAB>" :
                \ coc#refresh()

    function! s:check_back_space() abort
        let col = col(".") - 1
        return !col || getline(".")[col - 1]  =~# "\s"
    endfunction

    " coc-snippet
    " Use <c-j> for jump to next placeholder, it's default of coc.nvim
    let g:coc_snippet_next = '<c-n>'

    " Use <c-k> for jump to previous placeholder, it's default of coc.nvim
    let g:coc_snippet_prev = '<c-p>'

    " Use <cr> to confirm completion, `<c-g>u` means break undo chain at current
    " position. Coc only does snippet and additional edit on confirm.
    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<c-y>" : "\<c-g>u\<cr>"

    nmap <silent>ep <Plug>(coc-diagnostic-prev)
    nmap <silent>en <Plug>(coc-diagnostic-next)

    " GoTo code navigation
    nmap <silent>gd <Plug>(coc-definition)
    nmap <silent><space>gd :call CocAction("jumpDefinition", "vsplit")<cr>

    nmap <silent>gl <Plug>(coc-declaration)
    nmap <silent><space>gl :call CocAction("jumpDeclaration", "vsplit")<cr>
    " nmap <silent>gi <Plug>(coc-implementation)
    " nmap <silent>gt <Plug>(coc-type-definition)

    nmap <silent>gr <Plug>(coc-references)
    nmap <silent><space>gr :call CocAction("jumpReferences", "vsplit")<cr>

    " Use K to show documentation in preview window.
    nnoremap <silent>K :call <SID>show_documentation()<cr>
    function! s:show_documentation()
        if (index(["vim", "help"], &filetype) >= 0)
            execute "h ".expand("<cword>")
        else
            call CocAction("doHover")
        endif
    endfunction

    " preview definition
    nmap <silent>gp :<c-u>PreviewDefinition<cr>
    " https://github.com/wookayin/dotfiles/commit/c64b4f34c1f8051d4e0feb15eb3d4e57d7eab475
    " Experimental feature (preview definition): gp, `<leader>K`, or <Shift-F12>:
    " Peek into the definition in a floating window.
    " TODO: If there are 2+ definitions, it does not work with floating windows (coc.nvim problem)
    command! -nargs=0 PreviewDefinition :call CocActionAsync("jumpDefinition", ":OpenAsPreview")
    command! -nargs=* OpenAsPreview :call s:open_as_preview("<args>")
    function! s:open_as_preview(callstr)
        " e.g. the string should look like: +call cursor(<line>,<col>) <filename>
        let m = matchlist(a:callstr, '^+call cursor(\(\d\+\),\s*\(\d\+\))\s\+\(.*\)')
        if len(m) < 4   " TODO: more robust error handling
            echohl WarningMsg | echom "ERROR: Invalid callstr format" | echohl None
            return
        endif
        let linenr = m[1]
        let filename = expand(m[3])
        call quickui#preview#open(filename, {
                    \ 'cursor': linenr,
                    \ 'number' : 1,
                    \ 'persist': 0,
                    \ })
    endfunction

    " https://github.com/MaskRay/ccls/wiki/coc.nvim
    " caller and callee
    " nnoremap <silent>gc :call CocLocations("ccls","$ccls/call")<cr>
    " nnoremap <silent>gC :call CocLocations("ccls","$ccls/call",{"callee":v:true})<cr>

    " nnoremap <silent>gv :call CocLocations("ccls","$ccls/vars")<cr>
    " nnoremap <silent>gV :call CocLocations("ccls","$ccls/vars",{"kind":1})<cr>

    " Highlight the symbol and its references when holding the cursor.
    autocmd CursorHold * silent call CocActionAsync("highlight")
    autocmd CursorHoldI * silent call CocActionAsync("showSignatureHelp")

    " Symbol renaming.
    " nmap <leader>cr <Plug>(coc-rename)

    " Mappings using CoCList:
    " Show all diagnostics.
    " nnoremap <silent><space>a :<c-u>CocList diagnostics<cr>
    " " Manage extensions.
    " nnoremap <silent><space>e :<c-u>CocList extensions<cr>
    " " Show commands.
    " nnoremap <silent><space>c :<c-u>CocList commands<cr>
    " " Find symbol of current document.
    " nnoremap <silent><space>o :<c-u>CocList outline<cr>
    " " Search workspace symbols.
    " nnoremap <silent><space>s :<c-u>CocList -I symbols<cr>
    " " Do default action for next item.
    " nnoremap <silent><space>j :<c-u>CocNext<CR>
    " " Do default action for previous item.
    " nnoremap <silent><space>k :<c-u>CocPrev<CR>
    " " Resume latest coc list.
    " nnoremap <silent><space>p :<c-u>CocListResume<CR>
endif


"----------------------------------------------------------------------
" defx
"----------------------------------------------------------------------
if index(g:bundle_group, "defx") >= 0
    if has("nvim")
        Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
    else
        Plug 'Shougo/defx.nvim'
        Plug 'roxma/nvim-yarp'
        Plug 'roxma/vim-hug-neovim-rpc'
    endif

    Plug 'kristijanhusak/defx-git'
    " need nerd font
    " Plug 'kristijanhusak/defx-icons'

    nnoremap <space>- :Defx<cr>
    nnoremap <space>ft :Defx -search=`expand('%:p')` `getcwd()`<cr>

    function! s:defx_open_node() abort
        return defx#is_directory() ?
                    \ defx#do_action("open_or_close_tree") :
                    \ defx#do_action("drop")
    endfunction

    function! s:defx_my_settings() abort
        " Define mappings
        nnoremap <silent><buffer><expr> <CR> <SID>defx_open_node()
        nnoremap <silent><buffer><expr> <TAB> <SID>defx_open_node()

        nnoremap <silent><buffer><expr> <2-LeftMouse> <SID>defx_open_node()

        nnoremap <silent><buffer><expr> o <SID>defx_open_node()
        nnoremap <silent><buffer><expr> s defx#do_action("drop", "split")
        nnoremap <silent><buffer><expr> v defx#do_action("drop", "vsplit")

        nnoremap <silent><buffer><expr> <c-r> defx#do_action("redraw")
        nnoremap <silent><buffer><expr> . defx#do_action("toggle_ignored_files")
        nnoremap <silent><buffer><expr> .. defx#do_action("cd", [".."])
        nnoremap <silent><buffer><expr> D defx#do_action("remove_trash")
        nnoremap <silent><buffer><expr> Y defx#do_action("copy")
        nnoremap <silent><buffer><expr> M defx#do_action("move")
        nnoremap <silent><buffer><expr> P defx#do_action("paste")
        nnoremap <silent><buffer><expr> R defx#do_action("rename")
        nnoremap <silent><buffer><expr> N defx#do_action("new_file")
        nnoremap <silent><buffer><expr> E defx#do_action("yank_path")
    endfunction

    " operations in defx buffer
    autocmd FileType defx call s:defx_my_settings()

    " redraw when write to buffer
    autocmd BufWritePost * call defx#redraw()

    " close when defx is last buffer
    autocmd BufEnter * if (winnr("$") == 1 && exists("b:defx")) | q | endif
endif


"----------------------------------------------------------------------
" chadtree
"----------------------------------------------------------------------
if index(g:bundle_group, "chadtree") >= 0
    Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': ':UpdateRemotePlugins'}

    nnoremap <space>df :CHADopen<cr>
endif


"----------------------------------------------------------------------
" vimspector
"----------------------------------------------------------------------
if index(g:bundle_group, "vimspector") >= 0
    Plug 'puremourning/vimspector'

    " F3    Stop debugging.                                             vim spector#Stop()
    " F4    Restart debugging with the same configuration.              vim spector#Restart()
    " F5    When debugging, continue. Otherwise start debugging.        vim spector#Continue()
    " F6    Pause debugee.                                              vim spector#Pause()
    " F9    Toggle line breakpoint on the current line.                 vim spector#ToggleBreakpoint()
    " F8    Add a function breakpoint for the expression under cursor   vim spector#AddFunctionBreakpoint( "<cexpr>" )
    " F10   Step Over                                                   vim spector#StepOver()
    " F11   Step Into                                                   vim spector#StepInto()
    " F12   Step out of current function scope                          vim spector#StepOut()
    let g:vimspector_enable_mappings = "HUMAN"
endif


"----------------------------------------------------------------------
" lcnvim
"----------------------------------------------------------------------
if index(g:bundle_group, "lcnvim") >= 0
    Plug 'autozimu/LanguageClient-neovim', {
                \ 'branch': 'next',
                \ 'do': 'bash install.sh'
                \ }

    let g:LanguageClient_loggingFile = g:project_root
    let g:LanguageClient_diagnosticsList = "Location"
    let ccls_path = expand("~/Documents/workspace/github/ccls/Release/ccls")
    let g:LanguageClient_serverCommands =
                \ {
                \     "c": [ccls_path, "--log-file=/tmp/ccls_lcnvim.log"],
                \     "cpp": [ccls_path, "--log-file=/tmp/ccls_lcnvim.log"],
                \     "cuda": [ccls_path, "--log-file=/tmp/ccls_lcnvim.log"],
                \     "objc": [ccls_path, "--log-file=/tmp/ccls_lcnvim.log"]
                \ }
    let g:LanguageClient_loggingFile = "/tmp/languageserver-neovim.log"
    let g:LanguageClient_settingsPath = expand("~/Documents/workspace/dotfiles/vim/vim-init/tools/conf/languageclient-neovim.json")

    nnoremap <silent>K :call LanguageClient#textDocument_hover()<cr>
    nnoremap <silent>gd :call LanguageClient#textDocument_definition()<cr>
    nnoremap <silent>gr :call LanguageClient#textDocument_references({"includeDeclaration": v:false})<cr>
endif


"----------------------------------------------------------------------
" dadbod
"----------------------------------------------------------------------
if index(g:bundle_group, "dadbod") >= 0
    Plug 'tpope/vim-dadbod'
    Plug 'kristijanhusak/vim-dadbod-ui'
endif

"----------------------------------------------------------------------
" 结束插件安装
"----------------------------------------------------------------------
call plug#end()


"----------------------------------------------------------------------
" defx 其他设置
"----------------------------------------------------------------------
if index(g:bundle_group, "defx") >= 0
    call defx#custom#column(
                \ "icon", {
                \     "directory_icon": "▸",
                \     "opened_icon": "▾",
                \     "root_icon": '│'
                \ })

    call defx#custom#column(
                \ "filename", {
                \     "min_width": 40,
                \     "max_width": 40
                \ })

    call defx#custom#column(
                \ "mark", {
                \     "readonly_icon": "✗",
                \     "selected_icon": "✓"
                \ })

    " use this with Plug 'kristijanhusak/defx-icons'
    " \     "columns": "indent:mark:git:icons:filename",

    call defx#custom#option(
                \ "_", {
                \     "columns": "indent:mark:git:icon:filename",
                \     "split": "vertical",
                \     "direction": "topleft",
                \     "show_ignored_files": 0,
                \     "resume": 1,
                \     "toggle": 1,
                \     "winwidth": 40,
                \     "ignored_files":
                \       "*.d,*.slo,*.lo,*.o,*.obj,*.gch,*.pch,"
                \     . "*.so,*.dylib,*.dll,*.mod,*.smod,*.lai,"
                \     . "*.la,*.a,*.lib,*.exe,*.out,*.app,*.exe~,"
                \     . "*.test,*.class,__pycache__,*.pyc,*.pyo,"
                \     . "*.pyd,*py.class,*.swp,.DS_Store,.mypy_cache,"
                \     . ".pytest_cache,.git"
                \ })

    "==================================================================
    " defx-git
    call defx#custom#column(
                \ "git", "indicators", {
                \     "Modified"  : "✹",
                \     "Staged"    : "✚",
                \     "Untracked" : "✭",
                \     "Renamed"   : "➜",
                \     "Unmerged"  : "═",
                \     "Ignored"   : "☒",
                \     "Deleted"   : "✖",
                \     "Unknown"   : "?"
                \ })
endif


"----------------------------------------------------------------------
" cocnvim 其他设置
"----------------------------------------------------------------------
" language server 设置
" ccls 使用~表示 home ，兼容 macos 和 linux
if index(g:bundle_group, "cocnvim") >= 0
    "==================================================================
    " color
    highlight link CocFloating Visual

    " highlight LspCxxHlSymStructFieldNone ctermfg=Magenta guifg=Magenta
    " highlight LspCxxHlSymClassFieldNone ctermfg=Magenta guifg=Magenta
    " highlight LspCxxHlSymClassFieldStatic ctermfg=Magenta guifg=Magenta
    " highlight LspCxxHlSymUnknownFieldStatic ctermfg=Magenta guifg=Magenta
    " highlight LspCxxHlSymUnknownFieldNone ctermfg=Magenta guifg=Magenta

    " config
    " call coc#config("diagnostic.displayByAle", v:true)
    call coc#config("diagnostic.virtualText", v:true)
    call coc#config("suggest.enablePreview", v:true)
    call coc#config(
                \ "coc.preferences", {
                \     "currentFunctionSymbolAutoUpdate": v:true,
                \     "rootPatterns": g:project_root,
                \ })

    if has("macunix")
        let g:extra_args =
                    \ [
                    \     "-isystem",
                    \     "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
                    \ ]
    else
        let g:extra_args = []
    endif

    call coc#config(
                \ "languageserver", {
                \     "ccls": {
                \         "command": "~/Documents/workspace/github/ccls/Release/ccls",
                \         "filetypes": ["c", "cpp", "cuda", "objc", "objcpp"],
                \         "rootPatterns": g:project_root,
                \         "initializationOptions": {
                \             "clang": {
                \                 "extraArgs": g:extra_args
                \             },
                \             "capabilities": {
                \                 "foldingRangeProvider": v:false
                \             },
                \             "cache": {
                \                 "directory": ".ccls-cache"
                \             },
                \             "completion": {
                \                 "caseSensitivity": 0
                \             },
                \             "compilationDatabaseDirectory": "cmake-build",
                \             "codeLens": {
                \                 "localVariables": v:false
                \             },
                \             "client": {
                \                 "snippetSupport": v:true
                \             },
                \             "diagnostics": {
                \                 "onChange": 100,
                \                 "onOpen": 100,
                \                 "onSave": 100
                \             },
                \             "highlight": {
                \                 "lsRanges" : v:true
                \             },
                \             "index": {
                \                 "threads": 5
                \             }
                \         }
                \     },
                \     "haskell": {
                \         "command": "haskell-language-server-wrapper",
                \         "filetypes": ["haskell", "lhaskell"],
                \         "rootPatterns": g:project_root,
                \         "args": ["--lsp"]
                \     }
                \ })

    " call coc#config(
    " \ "languageserver", {
    " \     "clangd": {
    " \         "command": "/usr/local/opt/llvm/bin/clangd",
    " \         "args": ["--clang-tidy", "--completion-style=detailed", "-j=5", "--pch-storage=memory", "--suggest-missing-includes"],
    " \         "filetypes": ["c", "cpp", "cuda", "objc", "objcpp"],
    " \         "rootPatterns": g:project_root,
    " \         "semanticHighlighting": v:true
    " \     }
    " \ })

    "==================================================================
    " extensions
    let s:coc_extensions = [
                \     "coc-json",
                \     "coc-snippets",
                \     "coc-pyright",
                \     "coc-floaterm"
                \ ]

    for extension in s:coc_extensions
        call coc#add_extension(extension)
    endfor
endif


"----------------------------------------------------------------------
" sytle related
"----------------------------------------------------------------------
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
    if (has("vim"))
        let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
        let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    endif

    set termguicolors
endif

hi Normal guibg=NONE ctermbg=NONE   

"==================================================================
" colorscheme onedark
" let g:onedark_terminal_italics = 1

"==================================================================
" set background=dark
" colorscheme space_vim_theme

"==================================================================
set background=dark
let g:enable_bold_font = 1
let g:enable_italic_font = 1
let g:hybrid_transparent_background = 0
colorscheme hybrid_material

"==================================================================
" set background=dark
" colorscheme PaperColor

"==================================================================
" let g:one_allow_italics = 1
" colorscheme one
" set background=dark

"==================================================================
" let ayucolor="light"
" colorscheme ayu

"==================================================================
" set background=light
" colorscheme solarized8

"==================================================================
" colorscheme nord

"==================================================================
" let g:sierra_Sunset = 1
" let g:sierra_Twilight = 1
" let g:sierra_Midnight = 1
" let g:sierra_Pitch = 1
" colorscheme sierra 

" let g:arcadia_Sunset = 1
" let g:arcadia_Twilight = 1
" let g:arcadia_Midnight = 1
" let g:arcadia_Pitch = 1
" colorscheme arcadia

"==================================================================
" vim-devicons

if has("macunix")
    let g:WebDevIconsOS = "Darwin"
endif

" adding to vim-airline's tabline
let g:webdevicons_enable_airline_tabline = 0
" adding to vim-airline's statusline
let g:webdevicons_enable_airline_statusline = 0

" adding to vim-startify screen
let g:webdevicons_enable_startify = 0

"==================================================================
" startify
" let g:startify_custom_header = startify#pad(startify#fortune#quote())

let g:ascii = [
            \ '=================     ===============     ===============   ========  ======== ',
            \ '\\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . // ',
            \ '||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .|| ',
            \ '|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . || ',
            \ '||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .|| ',
            \ '|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . || ',
            \ '||. . ||   ||-.  || ||  `-||   || . .|| ||. . ||   ||-.  || ||  `|\_ . .|. .|| ',
            \ '|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . || ',
            \ '||_-. ||  .|/    || ||    \|.  || `-_|| ||_-. ||  .|/    || ||   | \  / |-_.|| ',
            \ '||    ||_-.      || ||      `-_||    || ||    ||_-.      || ||   | \  / |  `|| ',
            \ '||    `.         || ||         `.    || ||    `.         || ||   | \  / |   || ',
            \ '||            .===. `===.         .===..`===.         .===. /==. |  \/  |   || ',
            \ '||         .==.   \_|-_ `===. .===.   _|_   `===. .===. _-|/   `==  \/  |   || ',
            \ '||      .==.    _-.    `-_  `=.    _-.   `-_    `=.  _-.   `-_  /|  \/  |   || ',
            \ '||   .==.    _-.          `-__\._-.         `-_./__-.         `. |. /|  |   || ',
            \ '||.==.    _-.                                                     `. |  /==.|| ',
            \ '==.    _-.                                                            \/   `== ',
            \ '\   _-.                                                                `-_   / ',
            \ ' `..                                                                      ``.  ',
            \ ]

let g:startify_custom_header = startify#center(g:ascii + startify#fortune#boxed())
