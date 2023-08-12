;;; ~/.emacs.d/init.el


;;
;; 基本設定
;;

;; refs: https://qiita.com/hayamiz/items/0f0b7a012ec730351678
;; Alt key -> Meta key setting
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

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
(setq default-frame-alist '((width . 84) (height . 38)))

;;左側に行番号表示をする
(require 'linum)
(global-linum-mode)

;;著者が勧める時間節約法------------------
;; refs: UNIX POWER TOOLS 19.7 著者が勧める時間節約法 p.468
;; CTRL-hが前の文字を削除するように定義する
;; 通常このキーシーケンスは、ユーザを「ヘルプ」システムに案内する。
;;(define-key global-map "\C-h" 'delete-backward-char)

;; CTRL-hが検索でも確実に機能するようにする。
(setq search-delete-char (string-to-char "\C-h"))

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

;;End 著者が勧める時間節約法--------------



;;
;; DDSKK setting
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ido-vertical-mode company ac-slime ddskk))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 240 :width normal :foundry "nil" :family "UDEV Gothic 35NF")))))

;;(global-set-key (kbd "C-x C-j") 'skk-mode)

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
(setq skk-preload t)
;; 注) skk.el をロードするだけなら (require 'skk) でもよい。上記設定の
;; 場合は、skk-search-prog-list に指定された辞書もこの時点で読み込んで
;; 準備する。Emacs の起動は遅くなるが，SKK を使い始めるときのレスポンス
;; が軽快になる。


;;End DDSKK setting-------------------

;;SLIME setting-----------------------
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
(package-initialize)

(load (expand-file-name "~/.roswell/helper.el"))
;;End SLIME setting-------------------

;;company----------------------------
;; refs: https://qiita.com/sune2/items/b73037f9e85962f5afb7
(require 'company)
; (global-company-mode t) ; 全バッファで有効にする
(add-hook 'after-init-hook 'global-company-mode) ; refs: http://company-mode.github.io/
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る


;;End company-------------------------

;;; ~/.emacs.d/init.el ends hereh

