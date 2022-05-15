"-----------------------------------------------------------------------------
" |
" File name     :  .vimrc
" Copyright     :  nis
" Modified      :  2022/05/15 16:19
"-----------------------------------------------------------------------------

set nocompatible

" >>> dein settings >>>
" Reference:
" woodyzootopia.github.io/2018/12/自分のVimのプラグイン環境設定-Dein/Denite/Deoplete を動かすまで

" Use dein for plugin management. See update.sh in this directory.
" Use ~/.config/nvim/toml/dein.toml
" Use ~/.config/nvim/toml/dein_lazy.toml

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
call map(dein#check_clean(), "delete(v:val, 'rf')")
" <<< dein settings <<<

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------

" Essential for syntax
syntax enable
" Essential for filetype plugins.
filetype plugin indent on

" Carry over indenting from previous line
set autoindent
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
" Move cursor by mouse
set mouse=a
set nocp incsearch
" relative number
set relativenumber
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

" ----------------------------------------------------------------------------
" PLUGIN SETTINGS
" ----------------------------------------------------------------------------

" NeoBundle(Vimプラグインの管理) -----------------------------
if has('vim_starting')
" 初回起動時のみruntimepathにNeoBundleのパスを指定する
        set runtimepath+=~/.vim/bundle/neobundle.vim/

" NeoBundleが未インストールであればgit cloneする・・・・①
        if !isdirectory(expand("~/.vim/bundle/neobundle.vim/"))
                echo "install NeoBundle..."
                :call system("git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim")
        endif
endif

call neobundle#begin(expand('~/.vim/bundle/'))

" インストールするVimプラグインを以下に記述

" NeoBundle自身を管理
NeoBundleFetch 'Shougo/neobundle.vim'

"-------------------------------------------------------------
" ここに追加したいVimプラグインを記述する・・・・・・・②

" カラースキームmolokai
NeoBundle 'tomasr/molokai'

" Solarized install
NeoBundle 'altercation/vim-colors-solarized'

" iceberg.vim
NeoBundle 'cocopon/iceberg.vim'

" ステータスラインの表示内容強化
NeoBundle 'itchyny/lightline.vim'

" 日本語ヘルプをインストールする
NeoBundle 'vim-jp/vimdoc-ja'

" goyo.vim setting
NeoBundle 'junegunn/goyo.vim'

" vim-sweep_trail
NeoBundle 'vim-jp/vim-sweep_trail'

" fzf.vim
NeoBundle 'junegunn/fzf.vim'

"-------------------------------------------------------------
call neobundle#end()

" 未インストールのVimプラグインがある場合、
" インストールするかどうかを尋ねてくれるようにする設定・・・③
NeoBundleCheck

" End of NeoBundle(Vimプラグインの管理) ----------------------

" molokaiの設定 ----------------------------------------------
"if neobundle#is_installed('molokai')
"        colorscheme molokai
"endif
"
"set t_Co=256
"syntax enable

" Solarized settings -----------------------------------------
"syntax enable
"set background=dark
"colorscheme solarized
"let g:solarized_termcolors=256

" lightline setting
"let g:lightline = {
"        \ 'colorscheme': 'wombat'
"        \ }

" iceberg.vim settings -----------------------------------------
syntax enable
colorscheme iceberg

" ステータスラインの設定 -------------------------------------
set laststatus=2
set noshowmode

" ----------------------------------------------------------------------------
" KEYMAPS AND CUSTOM COMMANDS, FUNCTIONS
" ----------------------------------------------------------------------------

" >>> インサートモードから出ずにVimを使いこなす >>>
" Reference:
" https://woodyzootopia.github.io/2019/11/インサートモードから出ずにVimを使いこなす

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
" 保存
inoremap <C-l> <Esc>:update<CR>a
" <<< インサートモードから出ずにVimを使いこなす <<<

" Practical Vim, Drew Neil -----------------------------------
" <C-p>, <C-n>でも、コマンド履歴のフィルタリングまで行う
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" アクティブなファイルが含まれているディレクトリを手早く展開する
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Vimに同梱されているmatchitプラグインを有効化する (cf. p.176)
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

" End of Practical Vim, Drew Neil ----------------------------

" カーソル下の単語をハイライトする
nnoremap <silent> <Space><Space> "zyiw:let @/ = '\<' . @z . '\>'<CR>:set hlsearch<CR>

" カーソル下の単語をハイライトしてから置換する
nnoremap # <Space><Space>:%s/<C-r>///gc<Left><Left><Left>

" ハイライトを消去する
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" Delete, Backspace
inoremap <C-d> <Del>
imap <C-h> <BS>

" fzf(command-line fuzzy finder) mapping ---------------------
nnoremap s :Buffers<CR>
nnoremap t :Files<CR>

" Leader key setting -----------------------------------------
let mapleader = "\<Space>"

" スペースキーを prefix にする例
" スペースキー単体では何も起きないようにする
" これをしておかないと、うっかり <Space> + 割り当ててないキーを
" 押すと <Space> の元の機能が発動する
" thinca (id:thinca)
nnoremap <Leader> <Nop>

nnoremap <Leader>e :edit
nnoremap <Leader>w :<C-u>write<CR>
nnoremap <Leader>sp :split<CR>
nnoremap <Leader>vs :vsplit<CR>
" 新しいタブページを開く
nnoremap <Leader>te :tabedit
nnoremap <Leader>x :xit<CR>
" ウィンドウを閉じる
nnoremap <silent> <Leader>q :<C-u>quit<CR>

" ウィンドウの高さをできるだけ高くする。To Window Hight size
nnoremap <Leader>wr :resize<CR>
" ウィンドウの高さを10行分低くする。To Window Low size
nnoremap <Leader>wv :resize -10<CR>

" Windows間の移動 --------------------------------------------
" 上のWindowへ移動する
nnoremap <Leader>jk <C-w>k
" 下のWindowへ移動する
nnoremap <Leader>jj <C-w>j
" 左のWindowへ移動する
nnoremap <Leader>jh <C-w>h
" 右のWindowへ移動する
nnoremap <Leader>jl <C-w>l

" HOT KEYS ---------------------------------------------------
" Hot key for open init.vim file
" nnoremap <Leader>. :<C-u>edit $MYVIMRC<CR>
" Check for marks
nnoremap <Leader>m :<C-u>marks
" Check for registers
nnoremap <Leader>re :<C-u>registers
" :helpを3倍の速度で引く
nnoremap <C-h>  :<C-u>help<Space>

" 検索後にジャンプした際に検索単語を画面中央に持ってくる
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

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
" <<< INSERT MODE KEYMAPS <<<

" ペースト設定 クリップボードからペーストする時だけ、インデントしない
if &term =~ "xterm"
        let &t_SI        .= "\e[?2004h"
        let &t_EI        .= "\e[?2004l"
        let &pastetoggle =  "\e[201~"

        function XTermPasteBegin(ret)
                set paste
                return a:ret
        endfunction

        inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif

" 最後のカーソル位置を復元する--------------------------------
if has("autocmd")
        autocmd BufReadPost *
        \ if line("'\"") > 0 && line ("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif
endif

" Undoの永続化-------------------------------------------------
" Comment out for Error MacVim.app
" % mvim
" E15 無効な式です: . undo_path
"if has('persistent_undo')
"        let undo_path = expand('~/.vim/undo')
"        exe 'set undodir=' .. undo_path
"        set undofile
"endif

" カーソル下の単語をGoogleで検索する -------------------------
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
nnoremap <silent> <Space>g :SearchByGoogle<CR>

" goyo.vim の散文モード --------------------------------------
" Reference: https://postd.cc/vim3/
" VIM AFTER 15 YEARS (2017-10-17) by Ian Langworth

function! ProseMode()
        call goyo#execute(0, [])
        set nocopyindent nosmartindent noautoindent nolist noshowmode noshowcmd
        set complete+=s
        set background=light
        if !has('gui_running')
                let g:solarized_termcolors=256
        endif
        colors solarized
endfunction
command! ProseMode call ProseMode()
nnoremap \p :ProseMode<CR>
" End of goyo.vim の散文モード -------------------------------

" fzf.vim setting
set rtp+=/usr/local/opt/fzf

" vim-polyglot runtimepath
set runtimepath+=~/src/vim-polyglot

" ~/.exrc ファイルを読み込む
" source ~/.exrc

" set vim-commentary commentstring
 autocmd FileType python setlocal commentstring=#\ %s
 autocmd FileType haskell setlocal commentstring=--\ %s

" ----------------------------------------------------------------------------
" END OF FILE: .vimrc
" ----------------------------------------------------------------------------

