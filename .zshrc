# -------------------------------------------------------------------
#
#  File name     :  ~/.zshrc
#  Copyright     :  nis
#
#  .zshrc file
#  initial setup file for only interactive zsh
#  This file is read after .zshenv file is read.
#  Reference: http://www.gentei.org/~yuuji/support/zsh/files/zshrc
#
# -------------------------------------------------------------------

#テキスト処理のための標準的なコマンド群のmacOSへの導入手順---
# Reference:
# @eumesy 2019/04/01
# GNU/Linux Commands
# https://qiita.com/eumesy/items/3bb39fc783c8d4863c5f
# in ~/.zshenv, executed `unsetopt GLOBAL_RCS` and ignored /etc/zshrc
[ -r /etc/zshrc ] && . /etc/zshrc
#End テキスト処理のための標準的なコマンド群のmacOSへの導入手順---

#SET SHELL VARIABLE-----------------------
# Sample .zshrc file
# Reference: http://www.gentei.org/~yuuji/support/zsh/files/zshrc
# WORDCHARS=$WORDCHARS:s,/,,
HISTSIZE=200 HISTFILE=~/.zhistory SAVEHIST=180

#End SET SHELL VARIABLE-----------------------

#SET SHELL OPTIONS------------------------
# 有効にしてあるのは副作用の少ないもの
setopt auto_cd auto_pushd auto_remove_slash auto_name_dirs
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys pushd_ignore_dups
setopt correct
setopt PUSHD_IGNORE_DUPS
# 便利だが副作用の強いものはコメントアウト
#setopt auto_menu  correct rm_star_silent sun_keyboard_hack inc_append_history
setopt share_history hist_reduce_blanks hist_ignore_all_dups

#END SET SHELL OPTIONS--------------------

#colordiff
# refs: https://qiita.com/catatsuy/items/8bafef2a60762a1c9f0f
if [[ -x `which colordiff` ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi

export LESS='-R'

#End colordiff

#ALIAS AND FUNCTIONS----------------------
alias copy='cp -ip' move='mv -i'
alias fullreset='echo "\ec\ec"'
h () {history $* | less}
alias ja='LANG=ja_JP.eucJP XMODIFIERS=@im=kinput2'
alias fl='fc -l'
alias la='ls -A --color=auto' ll='ls -alF --color=auto' l='ls -CF' lr='ls -lR --color=auto' l1='ls -1a --color=auto'
alias d='cd ~/dotfiles'
alias tes='cd ~/pl/test'
alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
mdcd ()	{mkdir -p "$@" && cd "$*[-1]"}
mdpu ()	{mkdir -p "$@" && pushd "$*[-1]"}
alias psd=pushd ppd=popd ds='dirs -v'
alias cle='clear'
# alias diff='diff -y --suppress-common-lines --color=auto'
alias v="vim"
alias n="nvim"
alias vv='vim -R'                               # set view mode
alias nv='nvim -R'                              # set view mode
alias vi='vim -u NONE -N'                       # 設定ファイル や環境変数による初期化、
                                                # .gvimrcによるGUIの初期化も含め、
                                                # 全て省略される。プラグインも読み込まれない。
                                                # 'nocompatible' (Vimの拡張を有効) にすることができる。
alias em='emacs'                                # emacs
alias emd='emacs --debug-init'                  # emacs debug

# alias E='emacsclient -nc -a ""'                 # Emacs Client グラフィカル
# alias Ec='emacsclient -t -a ""'                 # Emacs Client コンソールクライアント
# alias kill-emacs="emacsclient -e '(kill-emacs)'" # Emacs daemonの終了

alias note=/Users/nis/script/note.py            # alias of script/note.py
alias pylint='pylint --max-line-length=79'      # pylint
alias grep='ggrep -E --color=auto'              # ggrep, color options
alias fgrep='fgrep --color=auto'                # Reference: https://kaworu.jpn.org/linux/grepのGREP_OPTIONSは廃止されました
alias egrep='egrep --color=auto'
alias mcb=mcb.pyw                               # mcb.pyw - Multi Clip Board.pyw alias
                                                # Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.207
alias pw=pw.py                                  # pw.py - Passwords locker
alias pwu=pwu.py                                # Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.144
alias mapIt=mapIt.py                            # mapIt.py - open map
                                                # Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.260-263
alias ggl=ggl.py                                # ggl.py - Google search
                                                # Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.278
alias note=note.py                              # note.pyの辞書をクリップボードにコピーする
alias zak=zak.sh                                # 雑記帳をVimで開く
alias st='open -a Sourcetree'                   # Activate Sourcetree

# ghc
# alias ghci='stack ghci'
# alias ghc='stack ghc --'
# alias runghc='stack runghc --'

# Git
alias g='git'
alias gA='git add --all :/'
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias gl='git log --oneline --all'
alias glo='git lol'
alias gls='git log --show-signature'
alias gm='gitmoji -c'
alias gmt='git mergetool --tool=nvimdiff2'
alias gp='git push'
alias gpo='git push origin'
alias gs='git status'
alias gsw='git switch'

alias gal='alias | grep git'
# tmux
alias t='tmux'                                  # tmux
alias tl='tmux ls'                              # tmux ls
alias ts='tmux choose-tree -s'                  # tmux sesssion window

# Dave Taylor Shell Scripts
# alias rm='~/script/newrm'                     # newrm (by Dave Taylor "Wicked Cool Shell Scripts" 2004)
alias fd='~/script/formatdir'                   # formatdir (by Dave Taylor "Wicked Cool Shell Scripts" 2004)

# trash
alias rm=trash                                  # Move files and folders to the trash

# irb simple prompt
alias irbs='irb --simple-prompt'
alias ruby='ruby -w'
alias ru='ruby -w'
alias gcc='gcc -Wall -fno-pic -fomit-frame-pointer'

alias vimf='vim -o `fzf`'                       # vim current file complement
alias nvimf='nvim -o `fzf`'                     # vim current file complement

# Rust cargo commands
# alias cb='cargo build'
# alias cr='cargo run'
# alias cc='cargo check'
# alias c='cargo'

# script 'environment'
alias envi='environment'

# install-data
alias install-ls='nvim ~/data/install/install-data.list'

# edit memo
alias memo='nvim ~/data/memo/memo'

# Common Lisp: PEPL
alias lispi='rlwrap ros run'

# Clojure repl
alias clj='lein repl'

#End ALIAS AND FUNCTIONS------------------

#BINDING KEYS-----------------------------
# viins keymap
bindkey -v

# emacs keymap
# bindkey -e
# bindkey '^p'	history-beginning-search-backward
# bindkey '^n'	history-beginning-search-forward

# Use <C-q> push-line (zsh emacs keymap)
stty start undef

#補完システムを利用:----------------------
# 補完の挙動が分かりやすくなる2つの設定のみを記述
zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''
autoload -U compinit && compinit

#End 補完システムを利用:------------------

#End Sample .zshrc file-------------------

# viins (like emacs-mode)
# refs: https://qiita.com/b4b4r07/items/8db0257d2e6f6b19ecb9
bindkey -M viins '\er' history-incremental-pattern-search-forward
bindkey -M viins '^?'  backward-delete-char
bindkey -M viins '^A'  beginning-of-line
bindkey -M viins '^B'  backward-char
bindkey -M viins '^D'  delete-char-or-list
bindkey -M viins '^E'  end-of-line
bindkey -M viins '^F'  forward-char
bindkey -M viins '^G'  send-break
bindkey -M viins '^H'  backward-delete-char
bindkey -M viins '^K'  kill-line
bindkey -M viins '^N'  down-line-or-history
bindkey -M viins '^P'  up-line-or-history
bindkey -M viins '^R'  history-incremental-pattern-search-backward
bindkey -M viins '^U'  backward-kill-line
# bindkey -M viins '^W'  backward-kill-word
bindkey -M viins '^Y'  yank

#End BINDING KEYS--------------------------

# zplug
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

# zsh-vimode-visual
zplug "b4b4r07/zsh-vimode-visual"

#Complement for git commands--------------
# Git git-completionでコマンドやブランチ名を補完する方法(zsh)
# Refs: https://alpacat.com/blog/git-completion-zsh/
# git-completion
fpath=(~/.zsh/completion $fpath)
autoload -U compinit
compinit -u

#End Complement for git commands----------

# -------------------------------------------------------------------
# PATH MODIFICATIONS
# -------------------------------------------------------------------

# Functions which modify the path given a directory, but only if the directory
# exists and is not already in the path. (Super useful in ~/.zshlocal)
_append_to_path() {
  if [ -d $1 -a -z ${path[(r)$1]} ]; then
    path=($1 $path);
  fi
}

# Homebrew PATH
export PATH=$PATH:/usr/local/sbin
export PATH="/usr/local/opt/gettext/bin:$PATH"

# wget PATH
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

# pip PATH
export PATH=$PATH:/Users/nis/Library/Python/2.7/bin

# tmux PATH
export PATH=$PATH:/usr/local/Cellar/tmux/3.1b/bin/tmux

# MacVim PATH
export PATH=$PATH:/Applications/MacVim.app/Contents/bin

# install vim from brew
export PATH="/usr/local/opt/ruby/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/ruby/lib"
export CPPFLAGS="-I/usr/local/opt/ruby/include"

#pyenv environment variable---------------
# export PYENV_ROOT="$HOME/.pyenv"
# idle3 change path
# Reference:https://teratail.com/questions/211641
# export PYENV_ROOT=/usr/local/var/pyenv
# export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
fi

#End of pyenv environment variable--------

# python3.8/site-packages PATH
# export PYTHONPATH="$PYENV_ROOT/versions/3.8.5/lib/python3.8/site-packages"
export PYTHONPATH="/usr/local/var/pyenv/versions/3.8.5/lib/python3.8/site-packages"

# certifi PATH
# export SSL_CERT_FILE="$PYENV_ROOT/versions/3.8.5/lib/python3.8/site-packages/certifi/cacert.pem"
export SSL_CERT_FILE="/usr/local/var/pyenv/versions/3.8.5/lib/python3.8/site-packages/certifi/cacert.pem"

#python@3.9 PATH--------------------------
# If you need to have python@3.9 first in your PATH run:
export PATH="/usr/local/opt/python@3.9/bin:$PATH"

# For compilers to find python@3.9 you may need to set:
export LDFLAGS="-L/usr/local/opt/python@3.9/lib"

# For pkg-config to find python@3.9 you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/python@3.9/lib/pkgconfig"
#End python@3.9 path----------------------

#pyenv pyenv-virtualenv settings----------
# HomebrewでmacOSにNeovimをインストールして、使えるように設定する方法
# https://yu8mada.com/2018/08/03/how-to-install-neovim-on-macos-using-homebrew-and-set-it-up-to-make-it-able-to-be-used/
eval "$(pyenv init -)"
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# rbenv
eval "$(rbenv init -)"

#brew nano initialize---------------------
export PATH="/usr/local/opt/ncurses/bin:$PATH"
#For compilers to find ncurses you may need to set:
#export LDFLAGS="-L/usr/local/opt/ncurses/lib"
#export CPPFLAGS="-I/usr/local/opt/ncurses/include"

#For pkg-config to find ncurses you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/ncurses/lib/pkgconfig"

#End brew nano initialize-----------------

#guile path-------------------------------
export GUILE_LOAD_PATH="/usr/local/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/usr/local/lib/guile/3.0/extensions"

#End of guile path------------------------

#ruby PATH--------------------------------
# If you need to have ruby first in your PATH run:
export PATH="/usr/local/opt/ruby/bin:$PATH"

# For compilers to find ruby you may need to set:
export LDFLAGS="-L/usr/local/opt/ruby/lib"
export CPPFLAGS="-I/usr/local/opt/ruby/include"

# For pkg-config to find ruby you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"

#End ruby path----------------------------

#ruby-build-------------------------------
# ruby-build installs a non-Homebrew OpenSSL for each Ruby version installed
# and these are never upgraded.

# To link Rubies to Homebrew's OpenSSL 1.1 (which is upgraded) add the following
# to your ~/.zshrc:
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"

# Note: this may interfere with building old versions of Ruby (e.g <2.4) that use
# OpenSSL <1.1.

# End ruby-build

#openssl@1.1 path-------------------------
# If you need to have openssl@1.1 first in your PATH run:
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

# For compilers to find openssl@1.1 you may need to set:
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"

# For pkg-config to find openssl@1.1 you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"

#End openssl@1.1 path---------------------

#Upgrading grep 3.4 -> 3.5----------------
# All commands have been installed with the prefix "g".
# If you need to use these commands with their normal names, you
# can add a "gnubin" directory to your PATH from your bashrc like:
export PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"

#End Upgrading grep 3.4 -> 3.5------------

# shims
# Reference: https://yoshikiito.net/blog/archives/2048/
export PATH="$HOME/.pyenv/shims:$PATH"

# pynvim
export PATH="$HOME/.pyenv/versions/3.8.5/lib/python3.8/site-packages:$PATH"

# stack
export PATH="~/.local/bin:$PATH"
# stack Use bash completion
# autoload -U +X compinit && compinit
# autoload -U +X bashcompinit && bashcompinit
# eval "$(stack ---bash-completion-script stack)"

# Cabal PATH
# export PATH="~/.cabal/bin:$PATH"
# ghcup
# export PATH="~/.ghcup/bin:$PATH"

# gnu-tar PATH
export PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"

# Deno
export DENO_INSTALL="/Users/nis/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# GEM PATHS
export PATH="/usr/local/lib/ruby/gems/3.1.0:$PATH"
export PATH="/Users/nis/.gem/ruby/3.1.0:$PATH"
export PATH="/usr/local/Cellar/ruby/3.1.2/lib/ruby/gems/3.1.0:$PATH"

# ddc-nextword
export NEXTWORD_DATA_PATH="~/.vim/plugged/ddc-nextword"

# Go PATH
export GOPATH=$(go env GOPATH)
export PATH=$PATH:$(go env GOPATH)/bin

# cpanm PATH
export PATH="/Users/nis/bin:$PATH"

# local::lib PATH
eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)

# git PATH
export PATH="/usr/local/opt/git/bin:$PATH"

# nodebrew PATH
export PATH=$HOME/.nodebrew/current/bin:$PATH
export NODEBREW_ROOT=/usr/local/var/nodebrew

# stack PATH
export PATH=$PATH:/Users/nis/.local/bin

# -------------------------------------------------------------------
#End PATH MODIFICATIONS
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# APPLICATION CUSTOMIZATIONS
# -------------------------------------------------------------------

# lightline color initialize
# export TERM=xterm-256color

#INTERNAL UTILITY FUNCTIONS---------------
# Returns whether the given command is executable or aliased.
# Reference: VIM AFTER 15 YEARS （2017-10-17） by Ian Langworth
# .zshrc by Ian Langworth
# https://postd.cc/vim3/
_has() {
  return $( whence $1 >/dev/null )
}

# Returns whether out terminal supports color.
_color() {
  return $( [ -z "$INSIDE_EMACS" -a -z "$VIMRUNTIME" ] )
}

#End INTERNAL UTILITY FUNCTIONS---------------

# GNU grep
if _color; then
  export GREP_COLORS='mt=1;32'
fi

# Ack is better than grep
if ! _color; then
  alias ack='ack --nocolor'
fi

# GNU and BSD ls colorization.
if _color; then
  export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=33:so=01;35:bd=33;01:cd=33;01:or=01;05;37;41:mi=01;37;41:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'
  export LSCOLORS='ExGxFxdxCxDxDxcxcxxCxc'
  export CLICOLOR=1
fi

#tcl-tk initialize------------------------
# Reference:
# https://qiita.com/skyloken/items/a5f839eba1bd79cd5ef9
# @skyloken 2019/11/02
# pyenvのpythonでtkinterを使用する方法
# qiita.com
# pyenv install doesn't work with homebrew installed tcl-tk · Issue #1375 · pyenv/pyenv · GitHub
export PATH="/usr/local/opt/tcl-tk/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/tcl-tk/lib"
export CPPFLAGS="-I/usr/local/opt/tcl-tk/include"
export PKG_CONFIG_PATH="/usr/local/opt/tcl-tk/lib/pkgconfig"
export PYTHON_CONFIGURE_OPTS="--with-tcltk-includes='-I/usr/local/opt/tcl-tk/include' --with-tcltk-libs='-L/usr/local/opt/tcl-tk/lib -ltcl8.6 -ltk8.6'"

#End tcl-tk initialize

# GNU "tar" has been installed as "gtar".
# If you need to use it as "tar", you can add a "gnubin" directory
# to your PATH from your bashrc like:
PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"

# 簡易エディタ zed 利用のための準備
autoload zed

# mathfuncモジュールをロードする
zmodload zsh/mathfunc

#Ctrl-Zを使ってVimにスイッチバックする----
# Reference: Vimの生産性を高める12の方法
# How to boost your Vim productivity
# (2014-03-21)
# by Adam Stankiewicz
# https://postd.cc/how-to-boost-your-vim-productivity/
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

#End Ctrl-Zを使ってVimにスイッチバックする---

#pure simple prompt-----------------------
# sindresorhus/pure
# Pretty, minimal and fast ZSH prompt

autoload -U promptinit; promptinit

# turn on git stash status
zstyle :prompt:pure:git:stash show yes

# change the color for both `prompt:success` and `prompt:error`
zstyle ':prompt:pure:prompt:*' color cyan

prompt pure

#End pure simple prompt-------------------

#zsh-users/zsh-syntax-highlightling-------
# Fish shell like syntax highlightling for Zsh.

if [ -f /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

#End zsh-users/zsh-syntax-highlightling---

# ghcup-env
[ -f "/Users/nis/.ghcup/env" ] && source "/Users/nis/.ghcup/env"

# Shell統合
# https://www.rasukarusan.com/entry/2019/04/13/180443
source ~/.iterm2_shell_integration.zsh

#Terminalの現在行をエディタで編集して実行する---
# Reference: rasukarasan.com/entry/2020/04/20/083000

edit_current_line() {
        local tmpfile=$(mktemp)
        echo "$BUFFER" > $tmpfile
        vim $tmpfile -c "normal $" -c "set filetype=zsh"
        BUFFER="$(cat $tmpfile)"
        CURSOR=${#BUFFER}
        rm $tmpfile
        zle reset-prompt
}
zle -N edit_current_line
# Ctrl w -> vim open
bindkey '^w' edit_current_line

#End Terminalの現在行をエディタで編集して実行する---

#Complie .zshrc file----------------------
# Reference: https://qiita.com/vintersnow/items/7343b9bf60ea468a4180
if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
        zcompile ~/.zshrc
fi

#End Complie .zshrc file------------------

#Setup ssh-agent--------------------------
# refs: https://h2plus.biz/hiromitsu/entry/791
if [ -f ~/.ssh-agent ]; then
    . ~/.ssh-agent
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
    ssh-agent > ~/.ssh-agent
    . ~/.ssh-agent
fi
ssh-add -l >& /dev/null || ssh-add

#End Setup ssh-agent----------------------


#iTermil2のウィンドウとタブに自動的に名前をつける---
# https://qiita.com/junkoda/items/8c0c209edbbabfd27f29
function precmd() {
  # カレントディレクトリを $HOME を ~ として表示
  local wname=`pwd | sed -e "s|$HOME|~|"`
  # カレントディレクトリ名
  local tname=`pwd | sed -e 's|^.*/||'`

  echo -ne "\033]2;$wname\007" # window title
  echo -ne "\033]1;$tname\007" # tab title
}

#End iTermil2のウィンドウとタブに自動的に名前をつける---

# 不要なcommandをhistoryから除外する
# refs: https://www.m3tech.blog/entry/dotfiles-bonsai
zshaddhistory() {
    local line="${1%%$'\n'}"
    [[ ! "$line" =~ "^(cd|la|ll|ls|rm|rmdir)($| )" ]]
}


### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

# -------------------------------------------------------------------
# Zinit Plugins
# -------------------------------------------------------------------
# zsh-completions
zinit light zsh-users/zsh-completions

# anyframe
zinit light mollifier/anyframe

# -------------------------------------------------------------------
# End Zinit Plugins
# -------------------------------------------------------------------
#
#Zinitでターミナルをカスタマイズする------
# refs: https://zenn.dev/nagan/articles/8168ba0ca407ad
# ディレクトリの移動履歴を表示
bindkey '^xd' anyframe-widget-cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# put history
bindkey '^xhi' anyframe-widget-put-history

# Git switch branch
bindkey '^x^b' anyframe-widget-checkout-git-branch

#End Zinitでターミナルをカスタマイズする--

# -------------------------------------------------------------------
# fzf
# -------------------------------------------------------------------

# fzf via Homebrew
if [ -e /usr/local/opt/fzf/shell/completion.zsh ]; then
        source /usr/local/opt/fzf/shell/key-bindings.zsh
        source /usr/local/opt/fzf/shell/completion.zsh
fi

# fzf via local installation
if [ -e ~/.fzf ]; then
        _append_to_path ~/.fzf/bin
        source ~/.fzf/shell/key-bindings.zsh
        source ~/.fzf/shell/completion.zsh
fi

# fzf + ag configuration
# refs: https://qiita.com/kompiro/items/a09c0b44e7c741724c80
# refs: https://qiita.com/yuucu/items/03baae12d40f9699ec59
if _has fzf && _has ag; then
        # export FZF_DEFAULT_COMMAND='ag --nocolor -g ""'
        export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git/**"'
        export FZF_CTRL_T_COMMAND='rg --files --hidden --follow --glob "!.git/**"'
        export FZF_CTRL_T_OPTS='--preview "bat  --color=always --style=header,grid --line-range :100 {}"'
        export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
        export FZF_DEFAULT_OPTS='
        --color fg:242,bg:236,hl:65,fg+:15,bg+:239,hl+:108
        --color info:108,prompt:109,spinner:108,pointer:168,marker:168
        --height 40%
        --reverse
        --border
        --inline-info
'
fi

# fzf + tmux configuration
# Ctrl-r -> fzf-tmux
export FZF_TMUX=1
export FZF_TMUX_HEIGHT=20


#fzfを活用してTerminalの作業効率を高める------
# fvim: ファイル名検索+Vimで開くファイルをカレントディレクトリからfzfで検索可能に
# refs: https://yiskw713.hatenablog.com/entry/2022/01/12/200000
# refs: https://momozo.tech/2021/03/10/fzf%E3%81%A7zsh%E3%82%BF%E3%83%BC%E3%83%9F%E3%83%8A%E3%83%AB%E4%BD%9C%E6%A5%AD%E3%82%92%E5%8A%B9%E7%8E%87%E5%8C%96/
# vim version
fvim() {
  local file
  file=$(
         rg --files --hidden --follow --glob "!**/.git/*" | fzf \
             --preview 'bat  --color=always --style=header,grid {}' --preview-window=right:60%
     )
  vim "$file"
}
alias fv="fvim"

# nvim version
fnvim() {
  local file
  file=$(
         rg --files --hidden --follow --glob "!**/.git/*" | fzf \
             --preview 'bat  --color=always --style=header,grid {}' --preview-window=right:60%
     )
  nvim "$file"
}
alias fn="fnvim"

# かつていたことのあるディレクトリに移動する
# refs: https://yiskw713.hatenablog.com/entry/2022/01/12/200000
# refs: https://qiita.com/kamykn/items/aa9920f07487559c0c7e
fzf-z-search() {
    local res=$(z | sort -rn | cut -c 12- | fzf)
    if [ -n "$res" ]; then
        BUFFER+="cd $res"
        zle accept-line
    else
        return 1
    fi
}

zle -N fzf-z-search
bindkey '^z' fzf-z-search

# fbr
# fbrコマンドで今ローカルに存在するbranchを選択して切り替えられる
# refs: https://qiita.com/kamykn/items/aa9920f07487559c0c7e
# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch -vv) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# fcd - cd to selected directory
fcd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# rupa/z
. ~/z/z.sh

# ｚの履歴をfzfで検索してからcdする
fzf-z-search() {
    local res=$(z | sort -rn | cut -c 12- | fzf)
    if [ -n "$res" ]; then
        BUFFER+="cd $res"
        zle accept-line
    else
        return 1
    fi
}

zle -N fzf-z-search
bindkey '^f' fzf-z-search

#End fzfを活用してTerminalの作業効率を高める------

#fzf（fuzzy finder）の便利な使い方をREADME, Wikiを読んで学ぶ------
# refs: https://wonderwall.hatenablog.com/entry/2017/10/06/063000
# Alt-cでディレクトリツリーを表示する
# export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

#End fzf（fuzzy finder）の便利な使い方をREADME, Wikiを読んで学ぶ--

# -------------------------------------------------------------------
# End fzf
# -------------------------------------------------------------------

# ghcup-env
[ -f "/Users/nis/.ghcup/env" ] && source "/Users/nis/.ghcup/env" # ghcup-env

# Don't end with errors.
# true

# -------------------------------------------------------------------
# END OF FILE: .zshrc
# -------------------------------------------------------------------

