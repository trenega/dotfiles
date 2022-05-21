#####################################################################
#
#  File name     :  ~/.zshrc
#  Copyright     :  nis
#  Modified      :  2022/03/23 16:35
#
#  .zshrc file
#  initial setup file for only interactive zsh
#  This file is read after .zshenv file is read.
#  Reference: http://www.gentei.org/~yuuji/support/zsh/files/zshrc
#
#####################################################################

# >>> テキスト処理のための標準的なコマンド群のmacOSへの導入手順 >>>
# Reference:
# @eumesy 2019/04/01
# GNU/Linux Commands
# https://qiita.com/eumesy/items/3bb39fc783c8d4863c5f
# in ~/.zshenv, executed `unsetopt GLOBAL_RCS` and ignored /etc/zshrc
[ -r /etc/zshrc ] && . /etc/zshrc
# <<< テキスト処理のための標準的なコマンド群のmacOSへの導入手順 <<<

# >>> Sample .zshrc file >>>
# Reference: http://www.gentei.org/~yuuji/support/zsh/files/zshrc
# -------------------------------------------------------------------
# SET SHELL VARIABLE
# -------------------------------------------------------------------
# WORDCHARS=$WORDCHARS:s,/,,
HISTSIZE=200 HISTFILE=~/.zhistory SAVEHIST=180

# -------------------------------------------------------------------
# SET SHELL OPTIONS
# -------------------------------------------------------------------

# 有効にしてあるのは副作用の少ないもの
setopt auto_cd auto_pushd auto_remove_slash auto_name_dirs
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys pushd_ignore_dups
setopt correct
setopt PUSHD_IGNORE_DUPS
# 便利だが副作用の強いものはコメントアウト
#setopt auto_menu  correct rm_star_silent sun_keyboard_hack
#setopt share_history inc_append_history

# -------------------------------------------------------------------
# ALIAS AND FUNCTIONS
# -------------------------------------------------------------------

alias copy='cp -ip' del='rm -i' move='mv -i'
alias fullreset='echo "\ec\ec"'
h () 		{history $* | less}
alias ja='LANG=ja_JP.eucJP XMODIFIERS=@im=kinput2'
alias fl='fc -l'
alias ls='ls -F --color=auto' la='ls -a --color=auto' ll='ls -alF --color=auto' lr='ls -lR --color=auto' l1='ls -1a --color=auto'
alias d='cd ~/dotfiles'
alias tes='cd ~/pl/test'
# alias t='cd ~/pl/test'
alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
mdcd ()		{mkdir -p "$@" && cd "$*[-1]"}
mdpu ()		{mkdir -p "$@" && pushd "$*[-1]"}
alias psd=pushd ppd=popd ds='dirs -v'
# Use Neovim instead of Vim
alias vim=nvim
alias vi=nvim
# set view mode 'vv' is "Vim View"
alias vv='nvim -R'
# Use Vim instead of v
alias v=/usr/local/bin/vim
# Edit .zshrc
alias zshrc='nvim ~/dotfiles/.zshrc'
# alias of script/note.py
alias note=/Users/nis/script/note.py
# pylint
alias pylint='pylint --max-line-length=79'
# grep, fgrep, egrep color options
# Reference: https://kaworu.jpn.org/linux/grepのGREP_OPTIONSは廃止されました
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
# mcb.pyw - Multi Clip Board.pyw alias
# Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.207
alias mcb=mcb.pyw
# pw.py - Passwords locker
# Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.144
alias pw=pw.py
alias pwu=pwu.py
# mapIt.py - open map
# Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.260-263
alias mapIt=mapIt.py
# ggl.py - Google search
# Reference: AUTOMATE THE BORING STUFF WITH PYTHON by Al Sweigart p.278
alias ggl=ggl.py
# note.pyの辞書をクリップボードにコピーする
alias note=note.py
# 雑記帳をVimで開く
alias zak=zak.sh
# edit ~/.config/nvim
alias vimcf='nvim ~/.config/nvim'
# Activate Sourcetree
alias st='open -a Sourcetree'
# ghc
alias ghci='stack ghci'
alias ghc='stack ghc --'
alias runghc='stack runghc --'
# Git
alias g='git'
# tmux
alias t='tmux'
# newrm (by Dave Taylor "Wicked Cool Shell Scripts" 2004)
alias rm='~/script/newrm'
# formatdir (by Dave Taylor "Wicked Cool Shell Scripts" 2004)
alias fd='~/script/formatdir'
# irb simple prompt
alias irbs='irb --simple-prompt'
alias ruby='ruby -w'
alias ru='ruby -w'
alias gcc='gcc -fno-pic -fomit-frame-pointer'
# vim current file complement
alias vimf='vim -o `fzf`'
alias e='emacs'

# -------------------------------------------------------------------
# Suffix aliases(起動コマンドは環境によって変更する)
# -------------------------------------------------------------------

# alias -s pdf=acroread dvi=xdvi
# alias -s {odt,ods,odp,doc,xls,ppt}=soffice
# alias -s {tgz,lzh,zip,arc}=file-roller

# -------------------------------------------------------------------
# BINDING KEYS
# -------------------------------------------------------------------

# emacs keymap
# bindkey -e
# bindkey '^p'	history-beginning-search-backward
# bindkey '^n'	history-beginning-search-forward

# viins keymap
bindkey -v
# viins (like emacs-mode)
# https://qiita.com/b4b4r07/items/8db0257d2e6f6b19ecb9
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

# zplug
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

# zsh-vimode-visual
zplug "b4b4r07/zsh-vimode-visual"

# -------------------------------------------------------------------
# 補完システムを利用: 補完の挙動が分かりやすくなる2つの設定のみを記述
# -------------------------------------------------------------------

zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''

# >>> Complement for git commands >>>
# Reference: https://blog.qnyp.com/2013/05/14/zsh-git-completion/
fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
autoload -U compinit
compinit -u
# <<< Complement for git commands <<<

# <<< Sample .zshrc file <<<

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

# >>> pyenv environment variable>>>
#export PYENV_ROOT="$HOME/.pyenv"
# idle3 change path
# Reference:https://teratail.com/questions/211641
# export PYENV_ROOT=/usr/local/var/pyenv
# export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
fi
# <<< pyenv environment variable <<<

# python3.8/site-packages PATH
# export PYTHONPATH="$PYENV_ROOT/versions/3.8.5/lib/python3.8/site-packages"
export PYTHONPATH="/usr/local/var/pyenv/versions/3.8.5/lib/python3.8/site-packages"

# certifi PATH
# export SSL_CERT_FILE="$PYENV_ROOT/versions/3.8.5/lib/python3.8/site-packages/certifi/cacert.pem"
export SSL_CERT_FILE="/usr/local/var/pyenv/versions/3.8.5/lib/python3.8/site-packages/certifi/cacert.pem"

# >>> python@3.9 PATH >>>
# If you need to have python@3.9 first in your PATH run:
export PATH="/usr/local/opt/python@3.9/bin:$PATH"

# For compilers to find python@3.9 you may need to set:
export LDFLAGS="-L/usr/local/opt/python@3.9/lib"

# For pkg-config to find python@3.9 you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/python@3.9/lib/pkgconfig"
# <<< python@3.9 path <<<

# >>> pyenv pyenv-virtualenv settings >>>
# HomebrewでmacOSにNeovimをインストールして、使えるように設定する方法
# https://yu8mada.com/2018/08/03/how-to-install-neovim-on-macos-using-homebrew-and-set-it-up-to-make-it-able-to-be-used/
eval "$(pyenv init -)"
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# rbenv
eval "$(rbenv init -)"

# nodenv
eval "$(nodenv init -)"
# <<< pyenv pyenv-virtualenv settings <<<

# >>> brew nano initialize >>>
export PATH="/usr/local/opt/ncurses/bin:$PATH"
#For compilers to find ncurses you may need to set:
#export LDFLAGS="-L/usr/local/opt/ncurses/lib"
#export CPPFLAGS="-I/usr/local/opt/ncurses/include"

#For pkg-config to find ncurses you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/ncurses/lib/pkgconfig"
# <<< brew nano initialize <<<

# >>> guile path >>>
export GUILE_LOAD_PATH="/usr/local/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/usr/local/lib/guile/3.0/extensions"
# <<< guile path <<<

# >>> ruby PATH >>>
# If you need to have ruby first in your PATH run:
export PATH="/usr/local/opt/ruby/bin:$PATH"

# For compilers to find ruby you may need to set:
export LDFLAGS="-L/usr/local/opt/ruby/lib"
export CPPFLAGS="-I/usr/local/opt/ruby/include"

# For pkg-config to find ruby you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"
# <<< ruby path <<<

# >>> ruby-build >>>
# ruby-build installs a non-Homebrew OpenSSL for each Ruby version installed
# and these are never upgraded.

# To link Rubies to Homebrew's OpenSSL 1.1 (which is upgraded) add the following
# to your ~/.zshrc:
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"

# Note: this may interfere with building old versions of Ruby (e.g <2.4) that use
# OpenSSL <1.1.
# <<< ruby-build <<<

# >>> openssl@1.1 path >>>
# If you need to have openssl@1.1 first in your PATH run:
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

# For compilers to find openssl@1.1 you may need to set:
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"

# For pkg-config to find openssl@1.1 you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"
# <<< openssl@1.1 path <<<

# >>> Upgrading grep 3.4 -> 3.5 >>>
# All commands have been installed with the prefix "g".
# If you need to use these commands with their normal names, you
# can add a "gnubin" directory to your PATH from your bashrc like:
export PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"
# <<< Upgrading grep 3.4 -> 3.5 <<<

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

# -------------------------------------------------------------------
# APPLICATION CUSTOMIZATIONS
# -------------------------------------------------------------------

# lightline color initialize
export TERM=xterm-256color

# >>> .zshrc by Ian Langworth >>>
# Reference: VIM AFTER 15 YEARS （2017-10-17） by Ian Langworth
# INTERNAL UTILITY FUNCTIONS
# https://postd.cc/vim3/
# Returns whether the given command is executable or aliased.

_has() {
  return $( whence $1 >/dev/null )
}

# Returns whether out terminal supports color.
_color() {
  return $( [ -z "$INSIDE_EMACS" -a -z "$VIMRUNTIME" ] )
}
# <<< .zshrc by Ian Langworth <<<

# GNU grep
if _color; then
  export GREP_COLOR='1;32'
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
if _has fzf && _has ag; then
        export FZF_DEFAULT_COMMAND='ag --nocolor -g ""'
        export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
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


# <<< .zshrc by Ian Langworth <<<

# >>> tmux making session >>>
# Reference: ターミナル（黒い画面）を分割して使うtmux超入門
# セッションなかったら作る
# https://girigiribauer.com/tech/20200427/

# if ! $(tmux has-session -t main 2> /dev/null)
# then
#   tmux new -s main
# fi

# # tmux からの起動じゃなかったら tmux attach
# if [ -z "$TMUX" ]
# then
#   tmux attach -t main
# fi

# <<< tmux making session <<<

# >>> tcl-tk initialize >>>
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
# <<< tcl-tk initialize <<<

# 簡易エディタ zed 利用のための準備
autoload zed

# mathfuncモジュールをロードする
zmodload zsh/mathfunc

# >>> Ctrl-Zを使ってVimにスイッチバックする >>>
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
# <<< Ctrl-Zを使ってVimにスイッチバックする <<<

# >>> sindresorhus/pure >>>
# Pretty, minimal and fast ZSH prompt

autoload -U promptinit; promptinit

# turn on git stash status
zstyle :prompt:pure:git:stash show yes

# change the color for both `prompt:success` and `prompt:error`
zstyle ':prompt:pure:prompt:*' color cyan

prompt pure
# <<< sindresorhus/pure <<<

# >>> zsh-users/zsh-syntax-highlightling >>>
# Fish shell like syntax highlightling for Zsh.

if [ -f /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# <<< zsh-users/zsh-syntax-highlightling <<<

[ -f "/Users/nis/.ghcup/env" ] && source "/Users/nis/.ghcup/env" # ghcup-env

source ~/.iterm2_shell_integration.zsh

# >>> Terminalの現在行をエディタで編集して実行する >>>
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
# <<< Terminalの現在行をエディタで編集して実行する <<<

# Complie .zshrc file
# Reference: https://qiita.com/vintersnow/items/7343b9bf60ea468a4180
if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
        zcompile ~/.zshrc
fi

# >>> Setup ssh-agent >>>
# Reference: https://h2plus.biz/hiromitsu/entry/791
if [ -f ~/.ssh-agent ]; then
    . ~/.ssh-agent
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
    ssh-agent > ~/.ssh-agent
    . ~/.ssh-agent
fi
ssh-add -l >& /dev/null || ssh-add
# <<< Setup ssh-agent <<<

# Use <C-q> push-line (zsh emacs keymap)
stty start undef

# Deno
export DENO_INSTALL="/Users/nis/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# Don't end with errors.
# true
