# -------------------------------------------------------------------
#
#  .zshenv file
#
#  initial setup file for both interactive and noninteractive zsh
#
# -------------------------------------------------------------------

#Sample .zshenv file----------------------
# Reference:
# http://www.gentei.org/~yuuji/support/zsh/files/zshenv
# zshの本 HIROSE Yuuji p.34

# Core file for max size
limit coredumpsize 0
# Setup command search path
typeset -U path
# (N-/) を付けることでことで存在しなければ無視してくれる
Path=($path /usr/*/bin(N-/) /usr/local/*/bin(N-/) /var/*/bin(N-/))
# $PATHに/Users/nis/scriptを追加する
PATH=$PATH":/Users/nis/script"
# リモートから起動するコマンド用の環境変数を設定（必要なら）
# export RSYNC_RSH=ssh
# export CVS_RSH=ssh

#End Sample .zshenv file------------------

# NeoVim environment variable
export XDG_CONFIG_HOME=~/.config
# Use Neovim as "preferred editor"
export VISUAL=nvim
# Open init.vim file of setting
export MYVIMRC=~/.config/nvim/init.vim

# Homebrew environment variable
export LDFLAGS="-L/usr/local/opt/gettext/lib"
export CPPFLAGS="-I/usr/local/opt/gettext/include"

# wget environment variable
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"

#Introduce Linux Commands-----------------
# Reference:
# テキスト処理のための標準的なコマンド群のmacOSへの導入手順
# GNU/Linux Commands
# @eumesy 2019/04/01
# https://qiita.com/eumesy/items/3bb39fc783c8d4863c5f
#
# Zsh の設定ファイルの調整が必要な理由
# Zsh は起動時に次の順番で設定ファイルを読み込みます．
#
# 1. /etc/zshenv ※ macOS には存在しません．
# 2. ~/.zshenv
# 3. /etc/zprofile （ログインシェルの場合）
# 4. ~/.zprofile (ログインシェルの場合)
# 5. /etc/zshrc (インタラクティブシェルの場合)
# 6. ~/.zshrc (インタラクティブシェルの場合)
# 7. /etc/zlogin （ログインシェルの場合） ※ macOS には存在しません．
# 8. ~/.zlogin （ログインシェルの場合）
#
# path などの設定は ~/.zshenv で，インタラクティブシェルの場合に限った設定
# （alias やプロンプトなど）は ~/.zshrc でをおこなえば良いように見えます．
# しかし，El Capitan 以後の macOS では，~/.zshenv よりも後から読み込まれる
# /etc/zprofile でシステムデフォルトの path が設定されます（正確には
# path_helper が実行されます．詳細は /etc/zprofile の中身および
# $ man path_helper 参照）．
# すなわち，ユーザが ~/.zshenv に登録した path よりも，システムデフォルトの
# path の優先順位が上がってしまいます．
# たとえば coreutils で導入した ls よりも，元々入っている ls が優先されてしまいます．
# この問題に対処するため，今回は
# (1) GLOBAL_RCS を用いて /etc/zprofile を 一旦無視し
# (2) path_helper は別途 ~/.zshenv から呼び出しました．

export PATH
export MANPATH
# -U: keep only the first occurrence of each duplicated value
# ref. http://zsh.sourceforge.net/Doc/Release/Shell-Builtin-Commands.html#index-typeset
typeset -U PATH path MANPATH manpath

# ignore /etc/zprofile, /etc/zshrc, /etc/zlogin, and /etc/zlogout
# ref. http://zsh.sourceforge.net/Doc/Release/Files.html
# ref. http://zsh.sourceforge.net/Doc/Release/Options.html#index-GLOBALRCS
unsetopt GLOBAL_RCS
# copied from /etc/zprofile
# system-wide environment settings for zsh(1)
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

path=(
    /usr/local/bin(N-/) # homebrew
    /usr/local/sbin(N-/) # homebrew
    ${path}
)
manpath=(
    /usr/local/share/man(N-/) # homebrew
    ${manpath}
)

path=(
    /usr/local/opt/coreutils/libexec/gnubin(N-/) # coreutils
    /usr/local/opt/ed/libexec/gnubin(N-/) # ed
    /usr/local/opt/findutils/libexec/gnubin(N-/) # findutils
    /usr/local/opt/gnu-sed/libexec/gnubin(N-/) # sed
    /usr/local/opt/gnu-tar/libexec/gnubin(N-/) # tar
    /usr/local/opt/grep/libexec/gnubin(N-/) # grep
    ${path}
)
manpath=(
    /usr/local/opt/coreutils/libexec/gnuman(N-/) # coreutils
    /usr/local/opt/ed/libexec/gnuman(N-/) # ed
    /usr/local/opt/findutils/libexec/gnuman(N-/) # findutils
    /usr/local/opt/gnu-sed/libexec/gnuman(N-/) # sed
    /usr/local/opt/gnu-tar/libexec/gnuman(N-/) # tar
    /usr/local/opt/grep/libexec/gnuman(N-/) # grep
    ${manpath}
)

#End Introduce Linux Commands-------------

# zeno
export ZENO_HOME=~/.config/zeno

# END OF FILE .zshenv ------------------------------------------------

