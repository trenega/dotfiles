" ----------------------------------------------------------------------------
" |
" File name     :  ~/.config/nvim/init.vim
" Copyright     :  nis
" ----------------------------------------------------------------------------



" ----------------------------------------------------------------------------
" PLUGIN SETTINGS
" ----------------------------------------------------------------------------

"dein Scripts-----------------------------
" Reference:
" https://woodyzootopia.github.io/2018/12/自分のVimのプラグイン環境設定-Dein/Denite/Deoplete を動かすまで

if &compatible
  set nocompatible " Be iMproved
endif

" Use dein for plugin management. See update.sh in this directory.
let s:cache_home = expand('~/.config/nvim')

" dein.vimインストール時に指定したディレクトリをセット
let s:dein_dir = s:cache_home . '/dein'

" dein.vimの実体があるディレクトリをセット
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" dein.vimが存在していない場合はgithubからclone
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

" If you want to install not installed plugins on startup.
if has('vim_starting') && dein#check_install()
  call dein#install()
endif

" plugins delete
if len(dein#check_clean()) != 0
  call map(dein#check_clean(), "delete(v:val, 'rf')")
  call dein#recache_runtimepath()
endif

"End dein Scripts-------------------------


" ----------------------------------------------------------------------------
" PATH SETTINGS
" ----------------------------------------------------------------------------

" Python path setting
let g:python_host_prog = $HOME . '/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = $HOME . '/.pyenv/versions/neovim3/bin/python'

" Use j0fzf
set rtp+=/usr/local/opt/fzf

" Use vim-polyglot
" refer: https://eh-career.com/engineerhub/entry/2019/01/28/103000
set runtimepath+=~/src/vim-polyglot

" dein update Setting
" https://thinca.hatenablog.com/entry/dein-vim-with-graphql-api
let g:dein#install_github_api_token = '$TOKEN'
call dein#check_update(v:true)

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------

filetype plugin indent on               " Essential for filetype plugins.

set autoindent                          " Carry over indenting from previous line

set clipboard+=unnamed                  " IMPORTANT! Vimの無名レジスタとOSのクリップボードを連携させる

set encoding=utf-8                      " UTF-8 by default

scriptencoding utf-8

set expandtab                           " Change Tab'code to Space'code

set fileencodings=ucs-boms,utf-8,euc-jp,cp932

set fileformats=unix,dos,mac            " Prefer Unix

set helplang=ja,en                      " help language

set history=5000                        " How many lines of history to save

set hlsearch                            " Hiligth searching

set ignorecase                          " Case insensitive

set incsearch                           " Search as you type

set infercase                           " ignorecase optionの副作用を解除する(cf.Practical Vim, Drew Neil p.354)

 set mouse=a                            " modifiable
                                        " set modifiable
                                        " Move cursor by mouse
set nocp incsearch

set number relativenumber               " number + relative number

set smartcase                           " 大文字/小文字の区別を予測してくれる
set smartindent

set shiftwidth=4                        " Number of spaces to shift for autoindent or >,<

set showmatch                           " Hilight matching braces/parens/etc.

set softtabstop=4                       " Spaces 'feel' like tabs

set tabstop=4                           " The One True Tab

set wildmenu                            " Show possible completions on command line

set wildmode=full                       " List all options and complete

set nobackup                            " No backup file

set signcolumn=yes                      " Show gitgutter column always

set background=dark                     " ノーマルモードで入力したコマンドが右下に表示される

set cursorline                          " cursorline on

set termguicolors                       " Ture Color

set statusline=%<%f\ %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%)\ %P
                                        " Disply statusline [Git(master)]

" ----------------------------------------------------------------------------
" CUSTOM COMMANDS AND FUNCTIONS
" ----------------------------------------------------------------------------

" Vimのページスクロールの挙動をちょっと改善するマッピング
"「<C-f>/<C-b>を入力すると<C-d>/<C-u>を2回実行する」マッピングを作りました。
" refer: https://zenn.dev/kawarimidoll/articles/17dad86545cbb4
nnoremap <C-f> <Cmd>set scroll=0<CR><Cmd>execute 'normal!' repeat("\<C-d>", v:count1 * 2)<CR>
nnoremap <C-b> <Cmd>set scroll=0<CR><Cmd>execute 'normal!' repeat("\<C-u>", v:count1 * 2)<CR>

" 論理行移動と表示行移動を入れ替える
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" <C-c>を完全な<ESC>に
inoremap <C-c> <ESC>

" カーソル位置から末尾までをヤンクする
nnoremap Y y$

" プロンプトで%%を入力すると現在編集中のバッファパスを展開する
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" 現在編集しているファイルを他のエディタで開く
map <f7> :!open -a /Applications/Sublime\ Text.app %<CR>

"インサートモードから出ずにVimを使いこなす----------
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


"End インサートモードから出ずにVimを使いこなす-------

"Practical Vim, Drew Neil----------------
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

"俺的にはずせない[Vim]こだわりmap(説明付き)------------
" Reference: https://qiita.com/itmammoth/items/312246b4b7688875d023

" カーソル下の単語をハイライトする
" レジスタを使用しない簡易版
" nnoremap <silent> <Space><Space> :let @/ = '\<' . expand('<cword>') . '\>'<CR>:set hlsearch<CR>
" カーソル下の単語をハイライトしてから置換する
" nmapについてですが、こいつはnnoremapと違い、右辺の再マップを行います。
" つまり右辺最初の<Space><Space>によって上のハイライトmapを発動させるということです。
" 通常mapはnoreを付けて再マップ無しでmapすることが一般的ですが、
" きちんと理解した上で再マップを利用するのはアリです。
" nnoremap # <Nop>
" nmap # <Space><Space>:%s/<C-r>///gc<Left><Left><Left>

" ハイライトを消去する
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" 挿入モードでのDelete, Backspace
inoremap <C-d> <Del>
imap <C-h> <BS>

" CTRL + ] で右にエスケープする
inoremap <C-]> <Esc><Right>

"End 俺的にはずせない[Vim]こだわりmap(説明付き)---------

"Leader Key-------------------------------
"Vimの生産性を高める12の方法
" Reference:
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" https://postd.cc/how-to-boost-your-vim-productivity/
" let mapleader = "\<Space>"
let mapleader = ","

" Make a simple "search" text object.
" refer:https://vim.fandom.com/wiki/Copy_or_change_search_hit
" 検索を実行するには通常と同様に /something を使う
" cgn を押し、最初の一致項目を置換して、 <Esc> を押す
" n.n.n.n.n. と押して、全ての一致項目を確認しながら置換する
vnoremap <silent> s //e<C-r>=&selection=='exclusive'?'+1':''<CR><CR>
    \:<C-u>call histdel('search',-1)<Bar>let @/=histget('search',-1)<CR>gv
omap s :normal vs<CR>

" スペースキーを prefix にする例
" スペースキー単体では何も起きないようにする
" これをしておかないと、うっかり <Space> + 割り当ててないキーを
" 押すと <Space> の元の機能が発動する
" Reference: https://thinca.hatenablog.com/entry/q-as-prefix-key-in-vim
nnoremap <Leader> <Nop>

" space 4で行末へ移動（shiftを押しながらの$は、手が遠いので）
" refer: https://original-game.com/mini_howto/how-to-use-the-leader-key-in-vim/
nnoremap <Leader>4 $

" space 6で行頭へ移動（英字キーボードの場合、shiftを押しながらの^は、手が遠いので）
nnoremap <Leader>6 ^

" {motion} のテキストを大文字にする。
" refer: ~/.config/nvim/dein/.cache/init.vim/.dein/doc/change.jax
map! <Leader>U <Esc>gUiw`]a

" {motion} のテキストを小文字にする。
map! <Leader>u <Esc>guiw`]a

" カレントファイルを開く。
" これは Vim 外部でカレントファイルに変更が加えられたとき、
" 開き直すのに便利である。
nnoremap <Leader>e :e<Space>

" 無名のバッファの編集を新規に開始する
nnoremap <Leader>n :enew<CR>

" ファイルを挿入する
nnoremap <Leader>r :r<Space>

" ファイルを保存する
" refer: thinca vimrc
nnoremap <silent> <Leader>w :<C-u>update<CR>
nnoremap <silent> <Leader>W :<C-u>update!<CR>
" nvimを終了する
nnoremap <silent> <Leader>q :<C-u>quit<CR>
nnoremap <silent> <Leader>Q :<C-u>quit!<CR>

" {count} なしの場合、カレントウィンドウを閉じる。
" もし {count} が与えられた場合、{count} ウィンドウを閉じる。
nnoremap <Leader>cl :close<CR>

" カレントウィンドウをスクリーン上にある唯一のウィンドウにする
nnoremap <Leader>on :only<CR>

" 現在のバッファに変更点があっても、書き込まずにVimを終了する
nnoremap <Leader>q! :quit!<CR>

" <Space><Space> でビジュアルラインモードに切り替える
nmap <Leader><Leader> V

" <Space>p と <Space>y でシステムのクリップボードにコピー＆ペーストする
" https://postd.cc/how-to-boost-your-vim-productivity/
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

" 新しいタブページを開く
nnoremap <Leader>te :tabedit

" 現在のウィンドウを水平に分割する
nnoremap <Leader>sp :split<CR>

" 現在のウィンドウを垂直に分割する
nnoremap <Leader>vs :vsplit<CR>

noremap <Leader>wv :resize -5<CR>

"move to current Window-------------------
" 上のWindowへ移動する
nnoremap <Leader>k <C-w>k

" 下のWindowへ移動する
nnoremap <Leader>j <C-w>j

" 左のWindowへ移動する
nnoremap <Leader>h <C-w>h

" 右のWindowへ移動する
nnoremap <Leader>l <C-w>l

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

"vim-fugitive Mapping---------------------
" refer: https://code-log.hatenablog.com/entry/2018/12/08/101732
nnoremap <Leader>ga :Git add %:p<CR><CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gl :GlLog<CR>
nnoremap <Leader>gb :Gblame<CR>

" Reload init.vim without restarting neovim
" refer: https://dev-to.translate.goog/reobin/reload-init-vim-without-restarting-neovim-1h82?_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc
nnoremap <silent> <Leader>rl :source $MYVIMRC<CR>

"End Leader Key Setting-------------------

"fzf.vim----------------------------------
" https://wonderwall.hatenablog.com/entry/2017/10/07/220000
" dispay new window
let g:fzf_layout = { 'window': 'enew' }

" Mapping selecting mappings
" :Maps
" https://github.com/junegunn/fzf.vim
nmap <Leader><tab> <plug>(fzf-maps-n)
xmap <Leader><tab> <plug>(fzf-maps-x)
omap <Leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
" https://github.com/junegunn/fzf.vim
" INSERT modeでファイル名や行を補完する
" <C-x><C-l>
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" refer: https://qiita.com/yuucu/items/03baae12d40f9699ec59
nnoremap <silent> <Leader>f :Files<CR>
nnoremap <silent> <Leader>g :GFiles<CR>
nnoremap <silent> <Leader>G :GFiles?<CR>
nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <Leader>H :History<CR>
nnoremap <silent> <Leader>R :Rg<CR>

"End fzf.vim------------------------------

" :helpを3倍の速度で引く
nnoremap ,h :<C-u>help<Space>

" 検索後にジャンプした際に検索単語を画面中央に持ってくる
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" 'verymagic'
nnoremap / /\v
nnoremap ? ?\v

"INSERT MODE KEYMAPS----------------------
" Change INSERT mode to NORMAL mode
inoremap <silent> fd <Esc>
" File Save
inoremap <silent> js <C-o>:write<CR>
" Scroll to center line
inoremap <silent> zz <C-o>zz
" Scroll to top line
inoremap <silent> zk <C-o>z<CR>
" Scroll to bottom line
inoremap <silent> zj <C-o>z-

"End INSERT MODE KEYMAPS------------------


inoremap <silent> a9 (

" ファイルを編集したあと、sudoでそのファイルを保存する
" Reference: https://laboradian.com/save-buffer-using-sudo-in-vim/
cmap w!! w !sudo tee > /dev/null %

"Python codes execute on Terminal---------
autocmd Filetype python nnoremap <buffer> <F5> :w<CR>:terminal python3 "%"<CR>
" Python codes execute on Terminal (Vertical windows)
" Please the new window close, then to codes window.
"autocmd Filetype python nnoremap <buffer> <F6> :w<CR>:vert terminal python3 "%"<CR>

"Restore last position of the cursor Script---------
if has("autocmd")
        autocmd BufReadPost *
        \ if line("'\"") > 0 && line ("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif
endif

"Undoの永続化 Script----------------------
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

"Persistent undo Script-------------------
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

"Search by Google on the cursor word------
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

"eskk Setting-----------------------------
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

" Basic setting
" https://zenn.dev/kouta/articles/87947515bff4da
let g:eskk#kakutei_when_unique_candidate = 1 " 漢字変換した時に候補が1つの場合、自動的に確定する
let g:eskk#enable_completion = 0             " neocompleteを入れないと、1にすると動作しなくなるため0推奨
let g:eskk#keep_state = 0                    " ノーマルモードに戻るとeskkモードを初期値にする
let g:eskk#egg_like_newline = 1              " 漢字変換を確定しても改行しない。

" 表示文字を変更(オレ サンカクデ ハンダン デキナイ)
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

"End eskk Setting-------------------------

"neoterm Setting--------------------------
" Wrapper of some vim/neovim's :terminal functions.
let g:neoterm_default_mod='belowright'
let g:neoterm_size=10
" Command done -> dispay result
let g:neoterm_autoscroll=1
" TERMINAL(insert-mode) -> vim(NORMAL)
tnoremap <silent> <C-w> <C-\><C-n><C-w>
" 3<leader>tl will clear neoterm-3.
nnoremap <Leader>tl :<c-u>exec v:count.'Tclear'<cr>
" RUN REPL
"       use: C-n
nnoremap <silent> <C-n> :TREPLSendLine<CR>j0
vnoremap <silent> <C-n> V:TREPLSendSelection<CR>'>j0

"End neoterm Setting----------------------

"vim-easymotion---------------------------
" Default bindings <Leader>s
" map <Leader> <Plug>(easymotion-prefix)
" <Leader>f{char} to move to {char}
" map  <Leader>f <Plug>(easymotion-bd-f)
" nmap <Leader>f <Plug>(easymotion-overwin-f)

" <Leader>S{char}{char} to move to {char}{char}
" nmap <Leader>S <Plug>(easymotion-overwin-f2)

" Move to line
" map <Leader>L <Plug>(easymotion-bd-jk)
" nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
" map  <Leader>wd <Plug>(easymotion-bd-w)
" nmap <Leader>wd <Plug>(easymotion-overwin-w)
"End vim-easymotion-----------------------

" Save in VISUAL mode -> smooth save
" https://zenn.dev/monaqa/articles/2020-12-22-vim-abbrev
cabbrev <expr> w (getcmdtype() ==# ":" && getcmdline() ==# "'<,'>w") ? "\<C-u>w" : "w"


"goyo + limelight (focus mode)------------
nnoremap <silent> <space>gy :Goyo<CR>
let g:goyo_width = 120
nnoremap <silent> <space>ll :Limelight!!<CR>
let g:limelight_default_coefficient = 0.7
" let g:limelight_paragraph_span = 1
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" https://zenn.dev/eqs/scraps/694f16529cdd70
lang en_US.UTF-8

"Terminal---------------------------------
" https://qiita.com/Lennon_x00x_/items/e8fa47d27aaab9635161
" 新規タブでターミナルモードを起動
" nnoremap <silent> tt <cmd>terminal<CR>

" 下分割でターミナルモードを起動
nnoremap <silent> tx <cmd>belowright new<CR><cmd>terminal<CR>

" ターミナルを開いたらに常にinsertモードに入る
autocmd TermOpen * :startinsert

" ターミナルモードで行番号を非表示
autocmd TermOpen * setlocal norelativenumber
autocmd TermOpen * setlocal nonumber

" Terminal-Job モードのマッピング（つまりシェルの入力中のキー操作）で <Esc> を <C-\><C-n> に割り当てる
" https://qiita.com/delphinus/items/aea16e82de2145d2a6b7
tnoremap <Esc> <C-\><C-n>

"End Terminal-----------------------------

"TerminalからDefx.nvimを起動する----------
" https://qiita.com/penTech/items/54156f52b4ed6ce83630
" use
" > nvim 'directory'
" or
" > nvim .

function! s:defx_my_settings() abort
  " ... configure defx as per :help defx-examples ...
endfunction

function! s:open_defx_if_directory()
  " This throws an error if the buffer name contains unusual characters like
  " [[buffergator]]. Desired behavior in those scenarios is to consider the
  " buffer not to be a directory.
  try
    let l:full_path = expand(expand('%:p'))
  catch
    return
  endtry

  " If the path is a directory, delete the (useless) buffer and open defx for
  " that directory instead.
  if isdirectory(l:full_path)
    Defx `expand('%:p')`
  endif
endfunction

augroup defx_config
  autocmd!
  autocmd FileType defx call s:defx_my_settings()

  " It seems like BufReadPost should work for this, but for some reason, I can't
  " get it to fire. BufEnter seems to be more reliable.
  autocmd BufEnter * call s:open_defx_if_directory()
augroup END

"End TerminalからDefx.nvimを起動する------

" Defx.nvim 自動更新
" https://qiita.com/arks22/items/9688ec7f4cb43444e9d9
autocmd BufWritePost * call defx#redraw()
autocmd BufEnter * call defx#redraw()

"Vimの生産性を高める12の方法-------------
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/

" 12<Enter> を押して、12行目に移動する（ 12G だと手首が曲がってしまう）
" Enterを押して、ファイルの末尾に移動する
" '^J'では不可
nnoremap <CR> <Nop>
nnoremap <CR> G

"End Vimの生産性を高める12の方法-------------
"deepl.vim--------------------------------
" Reference: https://github.com/ryicoh/deepl.vim
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

"ddu-ui-filer Scripts---------------------
call ddu#custom#patch_local('filer', {
\   'ui': 'filer',
\   'sources': [
\     {
\       'name': 'file',
\       'params': {},
\     },
\   ],
\   'sourceOptions': {
\     '_': {
\       'columns': ['filename'],
\     },
\   },
\   'kindOptions': {
\     'file': {
\       'defaultAction': 'open',
\     },
\   },
\   'uiParams': {
\     'filer': {
\       'winWidth': 40,
\       'split': 'vertical',
\       'splitDirection': 'topleft',
\     }
\   },
\ })

autocmd TabEnter,CursorHold,FocusGained <buffer>
	\ call ddu#ui#filer#do_action('checkItems')

autocmd FileType ddu-filer call s:ddu_filer_my_settings()
function! s:ddu_filer_my_settings() abort
  nnoremap <buffer><silent><expr> <CR>
    \ ddu#ui#get_item()->get('isTree', v:false) ?
    \ "<Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'narrow'})<CR>" :
    \ "<Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'open', 'params': {'command': 'vsplit'}})<CR>"

  nnoremap <buffer><silent><expr> <Space>
    \ ddu#ui#get_item()->get('isTree', v:false) ?
    \ "<Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'narrow'})<CR>" :
    \ "<Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'open', 'params': {'command': 'split'}})<CR>"

  nnoremap <buffer><silent> <Esc>
    \ <Cmd>call ddu#ui#filer#do_action('quit')<CR>

  nnoremap <buffer><silent> q
    \ <Cmd>call ddu#ui#filer#do_action('quit')<CR>

  nnoremap <buffer><silent> ..
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'narrow', 'params': {'path': '..'}})<CR>

  nnoremap <buffer><silent> c
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'copy'})<CR>

  nnoremap <buffer><silent> p
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'paste'})<CR>

  nnoremap <buffer><silent> d
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'delete'})<CR>

  nnoremap <buffer><silent> r
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'rename'})<CR>

  nnoremap <buffer><silent> mv
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'move'})<CR>

  nnoremap <buffer><silent> t
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'newFile'})<CR>

  nnoremap <buffer><silent> mk
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'newDirectory'})<CR>

  nnoremap <buffer><silent> yy
    \ <Cmd>call ddu#ui#filer#do_action('itemAction', {'name': 'yank'})<CR>
endfunction

nmap <silent> ;d <Cmd>call ddu#start({
\   'name': 'filer',
\   'searchPath': expand('%:p'),
\ })<CR>

"End ddu-ui-filer Scripts-----------------

"ddu.vim Scripts--------------------------
call ddu#custom#patch_global({
\   'ui': 'ff',
\   'sources': [
\     {
\       'name': 'file_rec',
\       'params': {
\         'ignoredDirectories': ['.git', 'node_modules', 'vendor', '.next']
\       }
\     }
\   ],
\   'sourceOptions': {
\     '_': {
\       'matchers': ['matcher_substring'],
\     },
\   },
\   'filterParams': {
\     'matcher_substring': {
\       'highlightMatched': 'Title',
\     },
\   },
\   'kindOptions': {
\     'file': {
\       'defaultAction': 'open',
\     },
\   },
\   'uiParams': {
\     'ff': {
\       'startFilter': v:true,
\       'prompt': '> ',
\       'split': 'floating',
\     }
\   },
\ })

call ddu#custom#patch_local('grep', {
\   'sourceParams' : {
\     'rg' : {
\       'args': ['--column', '--no-heading', '--color', 'never'],
\     },
\   },
\   'uiParams': {
\     'ff': {
\       'startFilter': v:false,
\     }
\   },
\ })


autocmd FileType ddu-ff call s:ddu_my_settings()
function! s:ddu_my_settings() abort
  nnoremap <buffer><silent> <CR>
    \ <Cmd>call ddu#ui#ff#do_action('itemAction', {'name': 'open', 'params': {'command': 'vsplit'}})<CR>

  nnoremap <buffer><silent> <Space>
    \ <Cmd>call ddu#ui#ff#do_action('itemAction', {'name': 'open', 'params': {'command': 'split'}})<CR>

  nnoremap <buffer><silent> a
    \ <Cmd>call ddu#ui#ff#do_action('openFilterWindow')<CR>

  nnoremap <buffer><silent> p
    \ <Cmd>call ddu#ui#ff#do_action('preview')<CR>

  nnoremap <buffer><silent> <Esc>
    \ <Cmd>call ddu#ui#ff#do_action('quit')<CR>
endfunction

autocmd FileType ddu-ff-filter call s:ddu_filter_my_settings()
function! s:ddu_filter_my_settings() abort
  inoremap <buffer><silent> <CR>
    \ <Esc><Cmd>close<CR>

  inoremap <buffer><silent> <Esc>
    \ <Esc><Cmd>close<CR>

  nnoremap <buffer><silent> <CR>
    \ <Cmd>close<CR>

  nnoremap <buffer><silent> <Esc>
    \ <Cmd>close<CR>
endfunction

nmap <silent> ;f <Cmd>call ddu#start({})<CR>
nmap <silent> ;g <Cmd>call ddu#start({
\   'name': 'grep',
\   'sources':[
\     {'name': 'rg', 'params': {'input': expand('<cword>')}}
\   ],
\ })<CR>

"ddu.vim Scripts--------------------------

"貼り付け時にペーストバッファが上書きされないようにする-------
" コードを ~/.vimrc の末尾付近に置きます
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
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

"End 貼り付け時にペーストバッファが上書きされないようにする---

" Perl provider (optional)
let g:loaded_perl_provider = 0


" ----------------------------------------------------------------------------
" COLORS
" ----------------------------------------------------------------------------

"lightline Script-------------------------
if !has('gui_running')
  set t_Co=256
endif

"hybrid Setting---------------------------
" Reference: https://qastack.jp/vi/3576/trouble-using-color-scheme-in-neovim
" let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" set termguicolors
" Reference: https://github.com/w0ng/vim-hybrid
" Set color for iTerm2
" let g:hybrid_custom_term_colors = 1
" set background=dark
" colorscheme hybrid

"End hybrid Setting-----------------------

"iceberg colorscheme Setting--------------
colorscheme iceberg
" set termguicolors

" ----------------------------------------------------------------------------
" abbreviation
" ----------------------------------------------------------------------------

iabbrev .b #!/bin/bash
iabbrev .z #!/bin/zsh
iabbrev .r #!/usr/bin/ruby
iabbrev .p #!/usr/bin/perl
iabbrev .a #!/usr/bin/awk -f
" iabbrev .l #!/usr/local/bin/lua
iabbrev .l #!/usr/bin/env lua
iabbrev EC # -*- coding: utf-8 -*-

" iabbrev .e niijimatakashi993@icloud.com
" iabbrev .g niijimatakashi993@gmail.com
" iabbrev .t takashiniijima213@gmail.com
" iabbrev .y takashiniijima213@yahoo.co.jp

" ----------------------------------------------------------------------------
" End abbreviation
" ----------------------------------------------------------------------------

" ----------------------------------------------------------------------------
" 日付挿入
" ----------------------------------------------------------------------------
" inoremap <Leader>date <C-R>=strftime('%Y/%m/%d (%a)')<CR>
inoremap <Leader>date <C-R>=strftime('%Y%m%d')<CR>
inoremap <Leader>time <C-R>=strftime('%H:%M')<CR>
inoremap <Leader>w3cd <C-R>=strftime('%Y-%m-%dT%H:%M:%S+09:00')<CR>

" Essential for syntax
syntax enable

" ----------------------------------------------------------------------------
" END OF FILE: init.vim
" ----------------------------------------------------------------------------
