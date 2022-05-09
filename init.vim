" ----------------------------------------------------------------------------
" |
" File name     :  ~/.config/nvim/init.vim
" Copyright     :  nis
" Modified      :  2021/06/27 20:33
" ----------------------------------------------------------------------------



" ----------------------------------------------------------------------------
" PLUGIN SETTINGS
" ----------------------------------------------------------------------------

" >>> dein.vim settings >>>
" Reference:
" https://woodyzootopia.github.io/2018/12/自分のVimのプラグイン環境設定-Dein/Denite/Deoplete を動かすまで

if &compatible
  set nocompatible " Be iMproved
endif

" Use dein for plugin management. See update.sh in this directory.
let s:cache_home = expand('~/.config/nvim')
let s:dein_dir = s:cache_home . '/dein'
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
if !isdirectory(s:dein_repo_dir)
  call system('git clone https://github.com/Shougo/dein.vim ' . shellescape(s:dein_repo_dir))
endif
let &runtimepath = s:dein_repo_dir .",". &runtimepath
let g:python3_host_prog = substitute(system("which python3"), '\n', '', 'g')

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)
  " locate toml directory beforehand
  let g:rc_dir    = s:cache_home . '/toml'
  let s:toml      = g:rc_dir . '/dein.toml'
  let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

  " read toml file and cache them
  call dein#load_toml(s:toml,      {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})

  call dein#end()
  call dein#save_state()
endif

if has('vim_starting') && dein#check_install()
  call dein#install()
endif

" plugins delete
if len(dein#check_clean()) != 0
  call map(dein#check_clean(), "delete(v:val, 'rf')")
  call dein#recache_runtimepath()
endif
" <<< dein.vim settings <<<

" ----------------------------------------------------------------------------
" PATH SETTINGS
" ----------------------------------------------------------------------------

" Python path setting
let g:python_host_prog = $HOME . '/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = $HOME . '/.pyenv/versions/neovim3/bin/python'

" Use fzf
set rtp+=/usr/local/opt/fzf

" Use vim-polyglot
set runtimepath+=~/src/vim-polyglot

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------

" Essential for syntax
syntax enable
" Essential for filetype plugins.
filetype plugin indent on

" Carry over indenting from previous line
set autoindent
" IMPORTANT! Vimの無名レジスタとOSのクリップボードを連携させる
set clipboard+=unnamed
" UTF-8 by default
set encoding=utf-8
scriptencoding utf-8
" Change Tab'code to Space'code
set expandtab
set fileencodings=ucs-boms,utf-8,euc-jp,cp932
" Prefer Unix
set fileformats=unix,dos,mac
" help language
set helplang=ja,en
" How many lines of history to save
set history=5000
" Hiligth searching
set hlsearch
" Case insensitive
set ignorecase
" Search as you type
set incsearch
" ignorecase optionの副作用を解除する(cf.Practical Vim, Drew Neil p.354)
set infercase
" modifiable
" set modifiable
" Move cursor by mouse
set mouse=a
set nocp incsearch
" number + relative number
set number relativenumber
" 大文字/小文字の区別を予測してくれる
set smartcase
set smartindent
" Number of spaces to shift for autoindent or >,<
set shiftwidth=8
" Hilight matching braces/parens/etc.
set showmatch
" Spaces 'feel' like tabs
set softtabstop=8
" The One True Tab
set tabstop=8
" Show possible completions on command line
set wildmenu
" List all options and complete
set wildmode=full
" No backup file
set nobackup

" ----------------------------------------------------------------------------
" CUSTOM COMMANDS AND FUNCTIONS
" ----------------------------------------------------------------------------

" >>> インサートモードから出ずにVimを使いこなす >>>
" Reference:
" https://woodyzootopia.github.io/2019/11/インサートモードから出ずにVimを使いこなす
" cnoremap mode: command line
" inoremap mode: insert

" 左へ移動
cnoremap <C-b> <Left>
inoremap <C-b> <Left>
" 右へ移動
cnoremap <C-f> <Right>
inoremap <C-f> <Right>
" 上へ移動
cnoremap <C-p> <Up>
inoremap <C-p> <Up>
" 下へ移動
cnoremap <C-n> <Down>
inoremap <C-n> <Down>
" 行頭へ移動
cnoremap <C-a> <Home>
inoremap <C-a> <Home>
" 行末へ移動
cnoremap <C-e> <End>
inoremap <C-e> <End>
" 一文字削除
cnoremap <C-d> <Del>
inoremap <C-d> <Del>
" <<< インサートモードから出ずにVimを使いこなす <<<

" >>> Practical Vim, Drew Neil >>>
" アクティブなファイルが含まれているディレクトリを手早く展開する
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Vimに同梱されているmatchitプラグインを有効化する (cf. p.176)
filetype plugin on
runtime macros/matchit.vim

" 「&」コマンドの修正 (cf. p.293)
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" substitution/qargs.vim
" vimgrepが返す各マッチはquickfixリストに記録される。
" このqargs.vimを使って、:Qargsとするとquickfixリストに含まれる
" 個々のファイル名を引数リストに設定してくれる。(cf. p.302)
command! -nargs=0 -bar Qargs execute 'args' QuickfixFilenames()
function! QuickfixFilenames()
          let buffer_numbers = {}
          for quickfix_item in getqflist()
                    let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
          endfor
          return join(map(values(buffer_numbers), 'fnameescape(v:val)'))
endfunction

" Vimのリストをサクサク移動するためのキーマッピング p.116
" nnoremap <silent> [b :bprevious<CR>
" nnoremap <silent> ]b :bnext<CR>
" nnoremap <silent> [B :bfirst<CR>
" nnoremap <silent> ]B :blast<CR>

" <<< Practical Vim, Drew Neil <<<

" >>> 俺的にはずせない[Vim]こだわりmap(説明付き) >>>
" Reference:
" https://qiita.com/itmammoth/items/312246b4b7688875d023
" カーソル下の単語をハイライトする
nnoremap <silent> <Space><Space> "zyiw:let @/ = '\<' . @z . '\>'<CR>:set hlsearch<CR>

" カーソル下の単語をハイライトしてから置換する
" nmapについてですが、こいつはnnoremapと違い、右辺の再マップを行います。
" つまり右辺最初の<Space><Space>によって上のハイライトmapを発動させるということです。
" 通常mapはnoreを付けて再マップ無しでmapすることが一般的ですが、
" きちんと理解した上で再マップを利用するのはアリです。
nnoremap ` <Nop>
nmap ` <Space><Space>:%s/<C-r>///gc<Left><Left><Left>

" ハイライトを消去する
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" 挿入モードでのDelete, Backspace
inoremap <C-d> <Del>
imap <C-h> <BS>

" CTRL + ] で右にエスケープする
inoremap <C-]> <Esc><Right>
" <<< 俺的にはずせない[Vim]こだわりmap(説明付き) <<<

" >>> Vimの生産性を高める12の方法 >>>
" Reference:
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" https://postd.cc/how-to-boost-your-vim-productivity/

let mapleader = "\<Space>"

" スペースキーを prefix にする例
" スペースキー単体では何も起きないようにする
" これをしておかないと、うっかり <Space> + 割り当ててないキーを
" 押すと <Space> の元の機能が発動する
" Reference: https://thinca.hatenablog.com/entry/q-as-prefix-key-in-vim
nnoremap <Leader> <Nop>

" <Space>e を押してカーソルの後ろに，新しいファイルを開く
nnoremap <Leader>e :edit<Space>
" <Space>w を押してファイルを保存する
nnoremap <silent><Leader>w :<C-u>write<CR>
" <Space>r を押してカーソルの後ろに，ファイルを挿入する
nnoremap <Leader>r :read<Space>


" <<< Vimの生産性を高める12の方法 <<<

" >>> Leader key setting >>>

" 現在のウィンドウを水平に分割する
nnoremap <Leader>sp :split<CR>
" 現在のウィンドウを垂直に分割する
nnoremap <Leader>vs :vsplit<CR>
" 新しいタブページを開く
nnoremap <Leader>te :tabedit
" ウィンドウを閉じる
nnoremap <silent> <Leader>q :<C-u>quit<CR>
" ウィンドウの高さをできるだけ高くする。To Window Hight size
nnoremap <Leader>wr :resize<CR>
" ウィンドウの高さを10行分低くする。To Window Low size
nnoremap <Leader>wv :resize -10<CR>

" Windows間の移動
" 上のWindowへ移動する
nnoremap <Leader>jk <C-w>k
" 下のWindowへ移動する
nnoremap <Leader>jj <C-w>j
" 左のWindowへ移動する
nnoremap <Leader>jh <C-w>h
" 右のWindowへ移動する
nnoremap <Leader>jl <C-w>l

" Hot key for open init.vim file
nnoremap <Leader>. :<C-u>edit $MYVIMRC<CR>
" Check for marks
nnoremap <Leader>mm :<C-u>marks<CR>
" Check for registers
" nnoremap <Leader>r :<C-u>registers<CR>
" sweep_trail.vim
nnoremap <Leader>sw :<C-u>SweepTrail<CR>
" undotree
" The undo history visualizer for VIM
nnoremap <Leader>ut :<C-u>UndotreeToggle<CR>

" >>> vim-fugitive >>>
" Reference: https://code-log.hatenablog.com/entry/2018/12/08/101732
nnoremap <Leader>ga :Git add %:p<CR><CR>
nnoremap <Leader>gc :Gcommit<CR><CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gp :Gpush<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gb :Gblame<CR>
" <<< vim-fugitive <<<

" <Space>h を押して行頭へカーソルを移動させる
nnoremap <Leader>h <Home>
" <Space>l を押して行末へカーソルを移動させる
nnoremap <Leader>l <End>

" >>> fzf.vim >>>
" https://wonderwall.hatenablog.com/entry/2017/10/07/220000
" Mapping selecting mappings
" :Maps
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
" Insert mode completion
" INSERT modeでファイル名や行を補完する
" <C-x><C-l>
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)
" <<< fzf.vim <<<

" <<< Leader key setting <<<

" >>> HOT KEYS >>>
" :helpを3倍の速度で引く
nnoremap <C-h> :<C-u>help<Space>
" <<< HOT KEYS <<<

" 検索後にジャンプした際に検索単語を画面中央に持ってくる
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" 'verymagic'
nnoremap / /\v

" eskk
" imap jk <Plug>(eskk:toggle)
" cmap jk <Plug>(eskk:toggle)

" >>> INSERT MODE KEYMAPS >>>
" Change INSERT mode to NORMAL mode
inoremap <silent> jj <Esc>
" File Save
inoremap <silent> js <C-o>:write<CR>

" Scroll to center line
inoremap <silent> zz <C-o>zz
" Scroll to top line
inoremap <silent> zk <C-o>z<CR>
" Scroll to bottom line
inoremap <silent> zj <C-o>z-

" Bracket Completion
" inoremap ( ()<Left>
" inoremap { {}<Left>
" inoremap [ []<Left>
" inoremap (( ()
" inoremap {{ {}
" inoremap [[ []
" " Quotation Completion
" inoremap ' ''<Left>
" inoremap " ""<Left>

" <<< INSERT MODE KEYMAPS <<<

" >>> ファイルを編集したあと、sudoでそのファイルを保存する
" Reference: https://laboradian.com/save-buffer-using-sudo-in-vim/
cmap w!! w !sudo tee > /dev/null %

" >>> Python codes execute on Terminal >>>
autocmd Filetype python nnoremap <buffer> <F5> :w<CR>:terminal python3 "%"<CR>
" Python codes execute on Terminal (Vertical windows)
" Please the new window close, then to codes window.
"autocmd Filetype python nnoremap <buffer> <F6> :w<CR>:vert terminal python3 "%"<CR>
" <<< Python codes execute on Terminal >>>

" >>> Restore last position of the cursor>>>
if has("autocmd")
        autocmd BufReadPost *
        \ if line("'\"") > 0 && line ("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif
endif
" <<< Restore last position of the cursor <<<

" >>> Undoの永続化 >>>
" Reference:
" Vimをカスタマイズしよう〜Vimはいいぞ！ゴリラと学ぶVim講座(6)
" 2019.12.03
" https://knowledge.sakura.ad.jp/23121/
" ディレクトリは事前に作成しておく必要があります。
if has('persistent_undo')
        let undo_path = expand('~/.vim/undo')
        exe 'set undodir=' .. undo_path
        set undofile
endif
" <<< Undoの永続化 <<<

" >>> Persistent undo >>>
" Reference:
" vim-users.jp
" Hack #162: Vimを終了してもundo履歴を復元する
" Posted at 2010/07/19
" thinca
"if has('persistent_undo')
"        set undodir=./.vimundo,~/.vimundo
"        augroup vimrc-undofile
"                autocmd!
"                autocmd BufReadPre ~/* setlocal undofile
"        augroup END
"endif
" <<< Persistent undo <<<

" >>> Search by Google on the cursor word >>>
" Reference: https://www.rasukarusan.com/entry/2019/03/09/011630
function! s:search_by_google()
    let line = line(".")
    let col  = col(".")
    let searchWord = expand("<cword>")
    if searchWord  != ''
        execute 'read !open https://www.google.co.jp/search\?q\=' . searchWord
        execute 'call cursor(' . line . ',' . col . ')'
    endif
endfunction
command! SearchByGoogle call s:search_by_google()
nnoremap <silent> <Space>gg :SearchByGoogle<CR>
" <<< Search by Google on the cursor word <<<

" ----------------------------------------------------------------------------
" COLORS
" ----------------------------------------------------------------------------

" lightline setting
if !has('gui_running')
  set t_Co=256
endif

" >>> hybrid setting >>>
" Reference: https://qastack.jp/vi/3576/trouble-using-color-scheme-in-neovim
" let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" set termguicolors
" Reference: https://github.com/w0ng/vim-hybrid
" Set color for iTerm2
" let g:hybrid_custom_term_colors = 1
" set background=dark
" colorscheme hybrid
" <<< hybrid setting <<<

" >>> iceberg colorscheme setting >>>
colorscheme iceberg
" <<< iceberg colorscheme setting <<<

" ----------------------------------------------------------------------------
" OTHER
" ----------------------------------------------------------------------------

" >>> Vimの生産性を高める12の方法 >>>
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
" 貼り付け時にペーストバッファが上書きされないようにする
" vp doesn't replace paste buffer

function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction
function! s:Repl()
  let s:restore_reg = @"
  return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()
" <<< Vimの生産性を高める12の方法 <<<

" " >>> limelight.vim Options >>>
" " Color name (:help cterm-colors) or ANSI code
" let g:limelight_conceal_ctermfg = 'gray'
" let g:limelight_conceal_ctermfg = 240

" " Color name (:help gui-colors) or RGB color
" let g:limelight_conceal_guifg = 'DarkGray'
" let g:limelight_conceal_guifg = '#777777'

" " Default: 0.5
" let g:limelight_default_coefficient = 0.7

" " Number of preceding/following paragraphs to include (default: 0)
" let g:limelight_paragraph_span = 1

" " Beginning/end of paragraph
" "   When there's no empty line between the paragraphs
" "   and each paragraph starts with indentation
" let g:limelight_bop = '^\s'
" let g:limelight_eop = '\ze\n^\s'

" " Highlighting priority (default: 10)
" "   Set it to -1 not to overrule hlsearch
" let g:limelight_priority = -1
" " <<< limelight.vim Options <<<

" >>> eskk setting >>>
" eskk dictionary autoload
if !filereadable(expand('~/.config/eskk/SKK-JISYO.L'))
  call mkdir('~/.config/eskk', 'p')
  call system('cd ~/.config/eskk/ && wget http://openlab.jp/skk/dic/SKK-JISYO.L.gz && gzip -d SKK-JISYO.L.gz')
endif

" eskk read dictionary
let g:eskk#directory = "~/.config/eskk"
let g:eskk#dictionary = { 'path': "~/.config/eskk/my_jisyo", 'sorted': 1, 'encoding': 'utf-8',}
let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}

" StatusLine dispy change mode
function L_eskk_get_mode()
    if (mode() == 'i') && eskk#is_enabled()
        return g:eskk#statusline_mode_strings[eskk#get_mode()]
    else
        return ''
    endif
endfunction

let g:lightline = {
\   'active': {
\     'left': [ ['mode', 'paste'], ['readonly', 'filename', 'eskk', 'modified'] ]
\   },
\   'component_function': {
\     'eskk': 'L_eskk_get_mode'
\   },
\ }

" https://zenn.dev/kouta/articles/87947515bff4da
" Basic setting
let g:eskk#kakutei_when_unique_candidate = 1 "漢字変換した時に候補が1つの場合、自動的に確定する
let g:eskk#enable_completion = 0             "neocompleteを入れないと、1にすると動作しなくなるため0推奨
let g:eskk#keep_state = 0                    "ノーマルモードに戻るとeskkモードを初期値にする
let g:eskk#egg_like_newline = 1              "漢字変換を確定しても改行しない。

"表示文字を変更(オレ サンカクデ ハンダン デキナイ)
" let g:eskk#marker_henkan = "`c`"
" let g:eskk#marker_henkan_select = "`o`"
" let g:eskk#marker_okuri = "`s`"
let g:eskk#marker_jisyo_touroku = "`d`"

" Sticky Shift
autocmd User eskk-initialize-post call s:eskk_initial_pre()
function! s:eskk_initial_pre() abort
  EskkUnmap -type=sticky Q
  EskkMap -type=sticky ;
endfunction

" 'l' inputed -> eskk mode break through
augroup vimrc_eskk
  autocmd!
  autocmd User eskk-enable-post lmap <buffer> l <Plug>(eskk:disable)
augroup END

imap jk <Plug>(eskk:toggle)
cmap jk <Plug>(eskk:toggle)

" <<< eskk setting <<<

" ----------------------------------------------------------------------------
" END OF FILE: init.vim
" ----------------------------------------------------------------------------

