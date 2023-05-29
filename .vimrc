"-----------------------------------------------------------------------------
" File name     :  .vimrc
" Copyright     :  nis
"-----------------------------------------------------------------------------

set nocompatible

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------

syntax enable                                   " Essential for syntax

filetype plugin indent on                       " Essential for filetype plugins.

set autoindent                                  " Carry over indenting from previous line

set clipboard+=unnamed

set encoding=utf-8                              " UTF-8 by default

scriptencoding utf-8

set expandtab                                   " Change Tab'code to Space'code

set fileencodings=ucs-boms,utf-8,euc-jp,cp932

set fileformats=unix,dos,mac                    " Prefer Unix

set helplang=ja,en                              " help language

set history=5000                                " How many lines of history to save

set hlsearch                                    " Hiligth searching

set ignorecase                                  " Case insensitive

set incsearch                                   " Search as you type

set infercase                                   " ignorecase optionの副作用を解除する(cf.Practical Vim, Drew Neil p.354)

set mouse=a                                     " Move cursor by mouse

set nocp incsearch

set relativenumber                              " relative number

set smartcase                                   " 大文字/小文字の区別を予測してくれる

set smartindent

set shiftwidth=4                                " Number of spaces to shift for autoindent or >,<

set showmatch                                   " Hilight matching braces/parens/etc.

set softtabstop=4                               " Spaces 'feel' like tabs

set tabstop=4                                   " The One True Tab

set wildmenu                                    " Show possible completions on command line

set wildmode=full                               " List all options and complete

set nobackup                                    " Does not make backup file

set backspace=indent,eol,start                  " lexima option

set signcolumn=yes                              " Show gitgutter column always

" ----------------------------------------------------------------------------
" PLUGIN SETTINGS
" ----------------------------------------------------------------------------

"NeoBundle(Vimプラグインの管理) ----------------------
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

" 日本語ヘルプをインストールする
NeoBundle 'vim-jp/vimdoc-ja'

" goyo.vim Setting
NeoBundle 'junegunn/goyo.vim'

" limelight.vim
NeoBundle 'junegunn/limelight.vim'

" vim-sweep_trail
NeoBundle 'vim-jp/vim-sweep_trail'

" fzf.vim
NeoBundle 'junegunn/fzf.vim'

" eskk.vim
NeoBundle 'vim-skk/eskk.vim'

" neoterm
NeoBundle 'kassio/neoterm'

" Auto close parentheses and repeat by dot dot dot...
NeoBundle 'cohama/lexima.vim'

" vim-expand-region
NeoBundle 'terryma/vim-expand-region'

" tpope/vim-commentary
NeoBundle 'tpope/vim-commentary'

"-------------------------------------------------------------
call neobundle#end()

" 未インストールのVimプラグインがある場合、
" インストールするかどうかを尋ねてくれるようにする設定・・・③
NeoBundleCheck

"End of NeoBundle(Vimプラグインの管理) -----------------------

" vim-expand-region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

"goyo + limelight (focus mode)------------
nnoremap <silent> <space>gy :Goyo<CR>
let g:goyo_width = 120
nnoremap <silent> <space>ll :Limelight!!<CR>
let g:limelight_default_coefficient = 0.7
" let g:limelight_paragraph_span = 1
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

"End goyo + limelight (focus mode)--------

" ----------------------------------------------------------------------------
" vim-plug
" ----------------------------------------------------------------------------

call plug#begin('~/.vim/plugged')

"ddc.vim ---------------------------------
Plug 'Shougo/ddc.vim'
Plug 'vim-denops/denops.vim'
Plug 'Shougo/pum.vim'

" Install your sources
Plug 'Shougo/ddc-around'
Plug 'Shougo/ddc-nextword'

" Install your filters
Plug 'Shougo/ddc-matcher_head'
Plug 'Shougo/ddc-sorter_rank'

"END OF ddc.vim --------------------------

" Mark change on vim
Plug 'airblade/vim-gitgutter'

" Git on vim
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

" Disply indent line
Plug 'Yggdroot/indentLine'

" translate.vim
Plug 'skanehira/translate.vim'

" deepl.vim
Plug 'ryicoh/deepl.vim'

"-----------------------------------------
call plug#end()
"-----------------------------------------

" ----------------------------------------------------------------------------
" END OF vim-plug
" ----------------------------------------------------------------------------

"deepl.vim--------------------------------
" フリー版のエンドポイントを指定
let g:deepl#endpoint = "https://api-free.deepl.com/v2/translate"

" AuthKey
let g:deepl#auth_key = "027ec6a8-c7a9-627c-40d6-2a6b3824b210:fx"

" replace a visual selection
vmap ,e <Cmd>call deepl#v("EN")<CR>
vmap ,j <Cmd>call deepl#v("JA")<CR>

" translate a current line and display on a new line
nmap ,e yypV<Cmd>call deepl#v("EN")<CR>
nmap ,j yypV<Cmd>call deepl#v("JA")<CR>

"End deepl.vim----------------------------

"ddc.vim----------------------------------
" Reference: https://github.com/Shougo/ddc.vim
" Customize global Settings
" Use around source.
" Reference: https://github.com/Shougo/ddc-around
call ddc#custom#patch_global('sources', ['around'])

" Use matcher_head and sorter_rank.
" Reference: https://github.com/Shougo/ddc-matcher_head
" Reference: https://github.com/Shougo/ddc-sorter_rank
call ddc#custom#patch_global('sourceOptions', {
      \ '_': {
      \   'matchers': ['matcher_head'],
      \   'sorters': ['sorter_rank']},
      \ })

" Change source options
call ddc#custom#patch_global('sourceOptions', {
      \ 'around': {'mark': 'A'},
      \ })
call ddc#custom#patch_global('sourceParams', {
      \ 'around': {'maxSize': 500},
      \ })

" Mappings

" <TAB>: completion.
inoremap <silent><expr> <TAB>
\ ddc#map#pum_visible() ? '<C-n>' :
\ (col('.') <= 1 <Bar><Bar> getline('.')[col('.') - 2] =~# '\s') ?
\ '<TAB>' : ddc#map#manual_complete()

" <S-TAB>: completion back.
inoremap <expr><S-TAB>  ddc#map#pum_visible() ? '<C-p>' : '<C-h>'

" Use ddc.
call ddc#enable()

"End ddc.vim------------------------------

" indentLine
let g:indentLine_color_term = 239
let g:indentLine_char =  '┊'

"translate.vim----------------------------
" skanehira/translate
let g:translate_source = "en"
let g:translate_target = "ja"
let g:translate_popup_window = 0 " if you want use popup window, set value 1
" let g:translate_winsize = 10 " set buffer window height size if you doesn't use popup window

" Key Mappings
" Gorilla Translate
nmap gt <Plug>(Translate)
vmap t <Plug>(VTranslate)

"End translate.vim------------------------

" ----------------------------------------------------------------------------
"  PATH
" ----------------------------------------------------------------------------

" Use vim-polyglot
set runtimepath+=~/src/vim-polyglot

" fzf.vim Setting
set rtp+=/usr/local/opt/fzf

" vim-polyglot runtimepath
set runtimepath+=~/src/vim-polyglot

" ----------------------------------------------------------------------------
" CUSTOM COMMANDS AND FUNCTIONS
" ----------------------------------------------------------------------------

"インサートモードから出ずにVimを使いこなす--------------------
" Reference: https://woodyzootopia.github.io/2019/11/インサートモードから出ずにVimを使いこなす
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

"End インサートモードから出ずにVimを使いこなす---------------

"Practical Vim, Drew Neil-----------------
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

"End Practical Vim, Drew Neil-------------

"俺的にはずせない[Vim]こだわりmap(説明付き)--------------
" Reference: https://qiita.com/itmammoth/items/312246b4b7688875d023

" カーソル下の単語をハイライトする
" レジスタを使用しない簡易版
nnoremap <silent> <Space><Space> :let @/ = '\<' . expand('<cword>') . '\>'<CR>:set hlsearch<CR>

" カーソル下の単語をハイライトしてから置換する
" nmapについてですが、こいつはnnoremapと違い、右辺の再マップを行います。
" つまり右辺最初の<Space><Space>によって上のハイライトmapを発動させるということです。
" 通常mapはnoreを付けて再マップ無しでmapすることが一般的ですが、
" きちんと理解した上で再マップを利用するのはアリです。
" nnoremap ` <Nop>
" nmap ` <Space><Space>:%s/<C-r>///gc<Left><Left><Left>

" ハイライトを消去する
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" 挿入モードでのDelete, Backspace
inoremap <C-d> <Del>
imap <C-h> <BS>

" CTRL + ] で右にエスケープする
inoremap <C-]> <Esc><Right>

"End 俺的にはずせない[Vim]こだわりmap(説明付き)----------

"-----------------------------------------------------------------------
"LEADER KEY MAPPINGS
"-----------------------------------------------------------------------

" Vimの生産性を高める12の方法
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
let mapleader = "\<Space>"

" スペースキーを prefix にする例
" スペースキー単体では何も起きないようにする
" これをしておかないと、うっかり <Space> + 割り当ててないキーを
" 押すと <Space> の元の機能が発動する
" Reference: https://thinca.hatenablog.com/entry/q-as-prefix-key-in-vim
nnoremap <Leader> <Nop>

" 新しいファイルを開く
nnoremap <Leader>e :edit<Space>

" 新しいファイルを開く
nnoremap <Leader>o :edit<Space>

" ファイルを保存する
nnoremap <silent><Leader>w :<C-u>write<CR>

" ファイルを挿入する
nnoremap <Leader>r :read<Space>

" カレントウィンドウを閉じる
nnoremap <Leader>q :quit

" 現在のバッファに変更点があっても、書き込まずにVimを終了する
nnoremap <Leader>q! :quit!<CR>

" <Space>p と <Space>y でシステムのクリップボードにコピー＆ペーストする
nmap <Leader><Leader> V

" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

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


" move to current Window
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
nnoremap <Leader>re :<C-u>registers<CR>

" sweep_trail.vim
nnoremap <Leader>sw :<C-u>SweepTrail<CR>

" undotree
" The undo history visualizer for VIM
nnoremap <Leader>ut :<C-u>UndotreeToggle<CR>

"vim-fugitive----------------------------
" Reference: https://code-log.hatenablog.com/entry/2018/12/08/101732
nnoremap <Leader>ga :Git add %:p<CR><CR>
nnoremap <Leader>gc :Gcommit<CR><CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gp :Gpush<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gb :Gblame<CR>

"End vim-fugitive-------------------------

" <Space>h を押して行頭へカーソルを移動させる
nnoremap <Leader>h <Home>

" <Space>l を押して行末へカーソルを移動させる
nnoremap <Leader>l <End>

"-----------------------------------------------------------------------
"End LEADER kEY MAPPINGS
"-----------------------------------------------------------------------

"fzf.vim----------------------------------
" Reference: https://wonderwall.hatenablog.com/entry/2017/10/07/220000
" dispay new window
let g:fzf_layout = { 'window': 'enew' }

" Mapping selecting mappings
" :Maps
nnoremap s <Nop>
nnoremap s :Buffers<CR>
nnoremap t <Nop>
nnoremap t :Files<CR>

" Reference: https://github.com/junegunn/fzf.vim
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
" Reference: https://github.com/junegunn/fzf.vim
" INSERT modeでファイル名や行を補完する
" <C-x><C-l>
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

"End fzf.vim------------------------------

" :helpを3倍の速度で引く
nnoremap <C-h> :<C-u>help<Space>

" 検索後にジャンプした際に検索単語を画面中央に持ってくる
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" 'verymagic'
nnoremap / /\v

"Insert Mode Keymaps----------------------
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

"End Insert Mode Keymaps------------------

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

"カーソル下の単語をGoogleで検索する -------------------------
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

"goyo.vim の散文モード ---------------------------------------
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


" set vim-commentary commentstring
autocmd FileType python setlocal commentstring=#\ %s
autocmd FileType haskell setlocal commentstring=--\ %s

"eskk Settings----------------------------
" Reference: https://zenn.dev/kato_k/articles/753b36262b3213
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

" Basic Setting
" Reference: https://zenn.dev/kouta/articles/87947515bff4da
let g:eskk#kakutei_when_unique_candidate = 1 " 漢字変換した時に候補が1つの場合、自動的に確定する
let g:eskk#enable_completion = 0             " neocompleteを入れないと、1にすると動作しなくなるため0推奨
let g:eskk#keep_state = 0                    " ノーマルモードに戻るとeskkモードを初期値にする
let g:eskk#egg_like_newline = 1              " 漢字変換を確定しても改行しない。

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

" eskk mode on keymapping
imap jk <Plug>(eskk:toggle)
cmap jk <Plug>(eskk:toggle)

"End eskk Settings------------------------

"cursor Change----------------------------
" Reference: https://qiita.com/Linda_pp/items/9e0c94eb82b18071db34
if has('vim_starting')
    " 挿入モード時に非点滅の縦棒タイプのカーソル
    let &t_SI .= "\e[6 q"

    " ノーマルモード時に点滅のブロックタイプのカーソル
    let &t_EI .= "\e[1 q"

    " 置換モード時に非点滅の下線タイプのカーソル
    let &t_SR .= "\e[4 q"
    endif

" NOMAL modeのカーソルを非点滅させる
" Reference: https://chanko.hatenadiary.jp/entry/2016/10/28/162648
" let &t_EI .= "\e[2 q"

"End cursor Change------------------------

"neoterm Setting--------------------------
" Wrapper of some vim/neovim's :terminal functions.
let g:neoterm_default_mod='belowright'
let g:neoterm_size=10
" Command done -> dispay result
let g:neoterm_autoscroll=1
" TERMINAL(insert-mode) -> vim(NORMAL)
tnoremap <silent> <C-w> <C-\><C-n><C-w>
" 3<leader>tl will clear neoterm-3.
nnoremap <leader>tl :<c-u>exec v:count.'Tclear'<cr>
" RUN REPL
"       use: C-n
nnoremap <silent> <C-n> :TREPLSendLine<CR>j0
vnoremap <silent> <C-n> V:TREPLSendSelection<CR>'>j0

"End neoterm Setting----------------------

"Vimの生産性を高める12の方法--------------
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
" 12<Enter> を押して、12行目に移動する（ 12G だと手首が曲がってしまう）
" Enterを押して、ファイルの末尾に移動する
nnoremap <CR> <Nop>
nnoremap <CR> G

"End Vimの生産性を高める12の方法----------

"cpと打つと ペーストモードになる
" Reference: https://kekaku.addisteria.com/wp/20170621231629
nnoremap cp :set paste<CR>

"挿入モードを抜けるとき、set nopaste を実行する。
autocmd InsertLeave * set nopaste


" ----------------------------------------------------------------------------
" ABBREVIATION
" ----------------------------------------------------------------------------

iabbrev .b #!/bin/bash
iabbrev .r #!/usr/bin/ruby
iabbrev EC # -*- coding: utf-8 -*-
iabbrev .e niijimatakashi993@icloud.com
iabbrev .g niijimatakashi993@gmail.com
iabbrev .t takashiniijima213@gmail.com
iabbrev .y takashiniijima213@yahoo.co.jp

" ----------------------------------------------------------------------------
" End ABBREVIATION
" ----------------------------------------------------------------------------

" ----------------------------------------------------------------------------
"  COLORS
" ----------------------------------------------------------------------------

"iceberg.vim
colorscheme iceberg
" True Color
set termguicolors
set cursorline                          " cursorline on

" 貼り付け時にペーストバッファが上書きされないようにする
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
" コードを ~/.vimrc の末尾付近に置きます
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

" ----------------------------------------------------------------------------
" END OF FILE: .vimrc
" ----------------------------------------------------------------------------

