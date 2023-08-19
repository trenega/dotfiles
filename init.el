;;;; ~/.emacs.d/init.el

;;
;; Package Settigs
;;

;; user-emacs-directory Settings
;; refs: https://myemacs.readthedocs.io/ja/latest/el-get.html
(when load-file-name (setq user-emacs-directory (file-name-directory load-file-name)))

;;; load-pathを追加する関数を定義----------------------------
;; refs: 「Emacs実践入門」大竹智也 p.61
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;;; End load-pathを追加する関数を定義------------------------

;;; 設定を分割して管理する-----------------------------------
;;; package.elを使ったパッケージ管理-------------------------
;; refs: 「Emacs実践入門」大竹智也[著] p.81
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "conf" "public_repos")

;; Emacsが自動的に書き込む設定をcustom.elに保存する
;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

;;; End 設定を分割して管理する-------------------------------



;;; Evil Settings-----------------------------------------
;; refs: https://tarao.hatenablog.com/entry/20130303/evil_intro
;; Emacs directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize)

(defun package-install-with-refresh (package)
  (unless (assq package package-alist)
    (package-refresh-contents))
  (unless (package-installed-p package)
    (package-install package)))

;; Install evil
(package-install-with-refresh 'evil)

;; Enable evil
(require 'evil)
(evil-mode 1)

;;; End Evil Settings-------------------------------------

;;
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; パッケージ情報の更新
(package-refresh-contents)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    ))

;; my/favarite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; End package.elを使ったパッケージ管理----------------------

;; el-get Initial settings--------------------------------
;; refs: https://myemacs.readthedocs.io/ja/latest/el-get.html
;; el-getのディレクトリをload-pathに追加
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; el-get.elを読み込ませる
(require 'el-get)
;; el-getでダンロードしたパッケージの保存先
(setq el-get-dir "~/.emacs.d/elisp")
;; End el-get Initial settings----------------------------

;; -------------------------------------------------------
;; -------------------------------------------------------
;; Write here! el-get Install packages

;; counsel.elはivyの骨格関数
(el-get-bundle counsel)

;; Map pairs of simultaneously pressed keys to commands
(el-get-bundle key-chord)

;; comment out
(el-get-bundle comment-dwim-2)

;; End Write here! el-get Install packages
;; -------------------------------------------------------
;; -------------------------------------------------------

;; End el-get Initial settings----------------------------

;; SLIME settings-----------------------------------------
;; refs: http://modern-cl.blogspot.com/2011/04/3-slime.html
;; SBCL をデフォルトのCommon Lisp処理系に設定
(setq inferior-lisp-program "sbcl")
;; ~/.emacs.d/slimeをload-pathに追加
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
;; SLIMEのロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner slime-indentation))
;; SLIMEからの入力をUTF-8に設定
(setq slime-net-coding-system 'utf-8-unix)

;; refs: https://asukiaaa.blogspot.com/2017/12/emacsslimeroswell.html
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
; (package-initialize)

(load (expand-file-name "~/.roswell/helper.el"))
;; End SLIME settings------------------

;; company Setting----------------------------------------
;; 補完用パッケージ
;; refs: https://qiita.com/sune2/items/b73037f9e85962f5afb7
(require 'company)
;; (global-company-mode t) ; 全バッファで有効にする
(add-hook 'after-init-hook 'global-company-mode) ; refs: http://company-mode.github.io/
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

;; End company Setting------------------------------------

;; ivy Setting--------------------------------------------
;; refs: https://takaxp.github.io/articles/qiita-helm2ivy.html
(when (require 'ivy nil t)

  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  (setq ivy-use-virtual-buffers t)

  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．

  ;; ESC連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  ;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
  (setq ivy-truncate-lines nil)

  ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  (setq ivy-wrap t)

  ;; アクティベート
  (ivy-mode 1))

;; End ivy Setting----------------------------------------

;; counsel Setting----------------------------------------
(when (require 'counsel nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-M-f") 'counsel-ag)

  ;; アクティベート
  (counsel-mode 1))

;; counsel-recentf 再定義
;; ファイルの表示を`~`から初める設定
;; refs: https://takaxp.github.io/articles/qiita-helm2ivy.html#org87d665a3
(defun ad:counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: "
            (progn
              (mapcar #'substring-no-properties recentf-list) ;; no need?
              (mapcar #'abbreviate-file-name recentf-list)) ;; ~/
            :action (lambda (f)
                      (with-ivy-window
                        (find-file f)))
            :require-match t
            :caller 'counsel-recentf))
(advice-add 'counsel-recentf :override #'ad:counsel-recentf)

;; End counsel Setting------------------------------------

;; swiper Setting-----------------------------------------
(when (require 'swiper nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))

;; End swiper Setting-------------------------------------

;; use-package, bind-key Setting--------------------------
;; http://malkalech.com/emacs_c-h_backspace
;; add MELPA and initialize
;; use-package
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; install use-package if not there
  (unless (require 'use-package nil t)
    (unless package-archive-contents (package-refresh-contents))
    (package-install 'use-package)))

;; bind-key
(unless (require 'bind-key nil t)
  (package-refresh-contents)
  (package-install 'bind-key))

;; End use-package, bind-key Setting----------------------

;;
;; End Package Settigs
;;

;;
;; 基本設定
;;

;;; Evil: EmacsをVimのごとく使う-設定編-----------------------
;; refs: https://tarao.hatenablog.com/entry/20130304/evil_config#emacs-evilize
;; カスタマイズオプションで設定できる範囲で、できる限りVimに近づける
(setq evil-want-C-u-scroll t
      evil-search-module 'evil-search
      evil-ex-search-vim-style-regexp t)

;; ファイル末尾で必ず改行する
(setq require-final-newline t)

;; バッファの終端を明示する
(setq indicate-buffer-boundaries 'left)

;; バッファの終端に[EOF]を表示する
;; end-mark.el
; (add-to-list 'load-path "~/.emacs.d/elisp/end-mark")
; (require 'end-mark)
; (global-end-mark-mode)

;;; End Evil: EmacsをVimのごとく使う-設定編-------------------

;; Alt key -> Meta key setting
;; refs: https://qiita.com/hayamiz/items/0f0b7a012ec730351678
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; 「状態を脱出する」コマンド
;;	リージョン選択状態
;;	ミニバッファでの入力状態
;;	ウィンドウの分割状態
;; http://dev.ariel-networks.com/articles/emacs/part4/
(global-set-key (kbd "C-M-g") 'keyboard-escape-quit)

;; "jj" to Esc
;; Exit instert mode by pressing j and then j quickly
;; https://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
(key-chord-mode 1)
(setq key-chord-tow-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;;http://www1.meijo-u.ac.jp/~kohara/cms/internal/emacs_setting
;;UTF-8の設定
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 括弧の対応関係をハイライト表示
(show-paren-mode nil)

;; スタートアップ画面を表示しないようにする
(setq inhibit-startup-message t)

;; ツールバーを表示しないようにする（Official Emacs の場合は 0）
(tool-bar-mode 0)

;; ウィンドウ（フレーム）のサイズ設定する
;;(setq default-frame-alist '((width . 84) (height . 38)))
(setq default-frame-alist '((width . 125) (height . 38)))

;;; 著者が勧める時間節約法---------------------------------
;; refs: UNIX POWER TOOLS 19.7 著者が勧める時間節約法 p.468
;; CTRL-hが前の文字を削除するように定義する
;; 通常このキーシーケンスは、ユーザを「ヘルプ」システムに案内する。
       ;;(define-key global-map "\C-h" 'delete-backward-char)

;; CTRL-hが検索でも確実に機能するようにする。
;;(setq search-delete-char (string-to-char "\C-h"))

;; 他の「ヘルプ」機能をバインドする (CTRL-_)。
;; 注記：一部の端末ではCTRL-_は定義されていない。
(define-key global-map "\C-_" 'help-command) ;; replacement
(setq help-char (string-to-char "\C-_"))

;; ESC-hが前の単語を削除するように定義する。
;;(define-key global-map "\M-h" 'backword-kill-word)

;; CTRL-x CTRL-uを「アンドウ」のコマンドにする。
;; これはCtrlキーを離す必要がないので、「CTRL-x u」よりも便利。
(define-key global-map "\C-x\C-u" 'undo)

;; CTRL-zおよびESC-zで、画面を1行「上」または「下」にスクロールする。
; (defun scroll-up-one ( ) "Scroll up 1 line." (interactive)
;   (scroll-up (prefix-numeric-value current-prefix-arg)))
; (defun scroll-down-one ( ) "Scroll down 1 line." (interactive)
;   (scroll-dowon (prefix-numeric-value current-prefix-arg)))
; (define-key global-map "\C-z" 'scroll-up-one)
; (define-key global-map "\M-z" 'scroll-down-one)

;; カレントファイルを画面に残しまま、CTRL-x CTRL-v
;; を使って新しいファイルにアクセスする。
(define-key global-map "\C-x\C-v" 'find-file-other-window)

;;; End 著者が勧める時間節約法-----------------------------

;; C-hをBackspaceに変更、C-?にhelpをmapping---------------
;; refs: malkalech.com/emacs_c-h_backspac:
;; C-h -> delete-backward-char
(define-key key-translation-map [?\C-h] [?\C-?])
;; C-h -> Backspace
(require 'bind-key)
(bind-key* "C-h" 'delete-backward-char)
;; C-? -> help
; (global-set-key (kbd "C-?") 'help-for-help)
(bind-key* "C-?" 'help-for-help)

;; End C-hをBackspaceに変更、C-?にhelpをmapping-----------

;; slime-compile Settings-------------------------------
(require 'bind-key)
(bind-key "C-c C-c" 'slime-compile-defun)
(bind-key "C-c C-k" 'slime-compile-and-load-file)

;;; Evil Leader-----------------------------------------
;; Evil Leader provides the <leader> feature from Vim that
;; provides an easy way to bind keys under a variable prefix key.
;; For an experienced Emacs User it is nothing more than
;; a convoluted key map, but for a Evil user coming from
;; Vim it means an easier start.
;; refs: https://github.com/cofi/evil-leader
;;
;; このevil-leaderのパッケージはel-getではなく、package.elでインストール
;; する。(el-getでインストールできない)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "s" 'switch-to-buffer             ; Switch to buffer
  "t" 'find-file                    ; find file Table
  "w" 'save-buffer                  ; Wrote <file>
  "k" 'kill-buffer                  ; Kill buffer
  "q" 'save-buffers-kill-emacs      ; Quit save buffers kill emacs
  "e" 'eval-last-sexp               ; Eval last sexp
  "c" 'slime-compile-defun          ; slime Compile defun
  "l" 'slime-compile-and-load-file  ; slime compile and Load file
  "x" 'other-window                 ; eXchange window
  "2" 'split-window-vertically      ; split window vertically
  "3" 'split-window-horizontally    ; vertically split
  "0" 'delete-window                ; delete window
  "1" 'delete-other-windows)        ; delete other window "only one"

;;; End Evil Leader-------------------------------------

;;; relative numbering----------------------------------
;; refs: https://www.reddit.com/r/emacs/comments/l7f85b/how_to_toggle_absolute_and_relative_numbering_in/
(defun my/display-set-relative ()
  (setq display-line-numbers 'relative))    ; or 'visual

(defun my/display-set-absolute ()
  (setq display-line-numbers t))

(add-hook 'evil-insert-state-entry-hook #'my/display-set-absolute)
(add-hook 'evil-insert-state-exit-hook #'my/display-set-relative)

;;; End relative numbering------------------------------

;; Tab Setting
;; Tab stop '8'
;; refs: https://emacs.stackexchange.com/questions/27869/how-to-make-evil-mode-tab-key-indent-not-re-indent-based-on-context
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;;;「Emacs実践入門」大竹智也[著]---------------------------
;; 行の折り返し表示を切り替える
;; refs: 「Emacs実践入門」大竹智也[著] p.81
(require 'bind-key)
(bind-key "C-c l" 'toggle-truncate-lines)

;; カラム番号も表示する
(column-number-mode t)

;; タイトルバーにファイルのフルパスを表示する
(setq frame-title-format "%f")

;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺にする
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を青にする
    (((class color) (background light))
     (:background "LightSkyBlue" t))
   (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)


;;; End「Emacs実践入門」大竹智也[著]-----------------------

;; comment out
;; comment-dwim-2
(bind-key "M-;" 'comment-dwim-2)

;;; clipboard Setting-----------------------------------
;; Emacsから他のエディターにAlt+vでペーストはできるが、その逆にEmacsへは
;; ペーストできない。
;; refs: saitodev.co/article/Emacsでクリップボードを使ってコピペしたい/
(cond (window-system
  (setq x-select-enable-clipboard t)))

;;; End clipboard Setting-------------------------------

;;
;; DDSKK setting
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-leader queue company ac-slime ddskk))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 240 :width normal :foundry "nil" :family "UDEV Gothic 35NF")))))

(global-set-key (kbd "C-x C-j") 'skk-mode)

;; https://github.com/skk-dev/ddskk/blob/master/etc/dot.emacs
;;; dot.emacs --- SKK related customization in ~/.emacs  -*- mode: emacs-lisp; coding: utf-8 -*-

;;; Commentary:

;; ~/.emacs.d/init.el に追加するための設定例です。

;;; 注意:

;; SKK の設定は、~/.skk の方が優先されます。
;; 下記の設定は、特殊な事情があるため ~/.skk ではうまく機能しない設定を
;; 集めていますので、下記以外は ~/.skk で設定することをお勧めします。

;;; Code:

;; @@ 基本の設定
(unless window-system
  frame-background-mode 'dark)		; or 'light

;; ~/.skk にいっぱい設定を書いているのでバイトコンパイルしたい
(setq skk-byte-compile-init-file t)
;; 注) 異なる種類の Emacsen を使っている場合は nil にします

;; SKK を Emacs の input method として使用する
;;   `toggle-input-method' (C-\) で DDSKK が起動します
(setq default-input-metod
      "japanese-skk"			; (skk-mode 1)
;;    "japanese-skk-auto-fill"		; (skk-auto-fill-mode 1)
      )

;; SKK を起動していなくても、いつでも skk-isearch を使う
(setq skk-isearch-mode-enable 'always)

;; @@ 応用的な設定

;; ~/.skk* なファイルがたくさんあるので整理したい
(setq skk-user-directory "~/.ddskk")
;; 注 1) 上記の設定をした場合、~/.skk や ~/.skk-jisyo の代わりに
;;       ~/.ddskk/init や ~/.ddskk/jisyo が使われます。ただし、
;;       これらのファイル名を個別に設定している場合はその設定が優先
;;       されるので注意してください。また、~/.skk や ~/.skk-jisyo を
;;       既にもっている場合は手動でコピーする必要があります。
;;       -- 影響を受ける変数の一覧 --
;;          skk-init-file, skk-jisyo, skk-backup-jisyo
;;          skk-emacs-id-file. skk-record-file,
;;          skk-study-file, skk-study-backup-file
;; 注 2) SKK の個人辞書は skkinput などのプログラムでも参照しますから、
;;       上記の設定をした場合はそれらのプログラムの設定ファイルも書き
;;       換える必要があります。

;; skk-jisyo は (path . coding) 形式のコンス・セルも受け付けます。
;;   $ mv jisyo jisyo.euc-jisx0213
;;   $ iconv -f euc-jisx0213 -t utf8 -o jisyo jisyo.euc-jisx0213
;;   同様に skk-study の学習結果ファイルも変換すること
(setq skk-jisyo (cons (expand-file-name "jisyo" skk-user-directory) 'utf-8))

;; migemo を使うから skk-isearch にはおとなしくしていて欲しい
(setq skk-isearch-start-mode 'latin)

;; YaTeX のときだけ句読点を変更したい
(add-hook 'yatex-mode-hook
	  (lambda ()
	    (require 'skk)
	    (setq skk-kutouten-type 'en)))

;; 文章系のバッファを開いた時には自動的に英数モード(「SKK」モード)に入る
(let ((function #'(lambda ()
		    (require 'skk)
		    (skk-latin-mode-on))))
  (dolist (hook '(find-file-hooks
		  ;; ...
		  mail-setup-hook
		  message-setup-hook))
    (add-hook hook function)))

;; Emacs 起動時に SKK を前もってロードする
;; (setq skk-preload t)
;; 注) skk.el をロードするだけなら (require 'skk) でもよい。上記設定の
;; 場合は、skk-search-prog-list に指定された辞書もこの時点で読み込んで
;; 準備する。Emacs の起動は遅くなるが，SKK を使い始めるときのレスポンス
;; が軽快になる。

;; End DDSKK setting-------------------

;;;; ~/.emacs.d/init.el ends here

