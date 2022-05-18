;; 基本設定

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
;; ツールバーを表示しないようにする（Official Emacs の場合は 0）
;; ウィンドウ（フレーム）のサイズ設定する
(setq default-frame-alist '((width . 100) (height . 60)))
;;左側に行番号表示をする
(require 'linum)
(global-linum-mode)
; ホームディレクトリを初期ディレクトリにする
(cd "~/")

;;ディスプレイ関係
(if window-system (progn
;; 文字の色を設定します。 
(add-to-list 'default-frame-alist '(foreground-color . "white")) 
;; 背景色を設定します。 
(add-to-list 'default-frame-alist '(background-color . "navy")) 
;; カーソルの色を設定します。 
(add-to-list 'default-frame-alist '(cursor-color . "yellow")) 
;; マウスポインタの色を設定します。 
(add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2")) 
;; 選択中のリージョンの色を設定します。 
(set-face-background 'region "deeppink1") 
)
)
;; デフォルトの透明度を設定する (85%) 
(add-to-list 'default-frame-alist '(alpha . 85))
;; カレントウィンドウの透明度を変更する (85%) 
(set-frame-parameter nil 'alpha 0.85) 

;; package.elがパッケージをMELPAから取得できるように
;; 変数package-archivesを設定します。
;; https://github.com/skk-dev/ddskk/blob/master/READMEs/INSTALL.MELPA.md
(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t))

;; >>> DDSKK setting >>>
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ddskk)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (global-set-key (kbd "C-x C-j") 'skk-mode)

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

;;; dot.emacs ends hereh
;; <<< DDSKK setting <<<
