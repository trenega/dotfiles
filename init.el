;;;; ~/.emacs.d/init.el

; ;;; load-pathを追加する関数を定義----------------------------
; ;; refs: 「Emacs実践入門」大竹智也 p.61
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
;; ~/.emacs.d/straight/repos/melpa/recipes
(add-to-load-path "straight/repos/melpa/recipes")

;;; End load-pathを追加する関数を定義------------------------


;;;-------------------------------------------------------
;;; 初期設定
;;;-------------------------------------------------------

;; ウィンドウ（フレーム）のサイズ設定する
;; [重要]: (height . 38) を (height . 39) に変更しないこと！！
;; Emacs が立ち上がらなくなる！！
;;(setq default-frame-alist '((width . 84) (height . 38)))
(setq default-frame-alist '((width . 125) (height . 38)))

;; Alt key -> Meta key setting
;; refs: https://qiita.com/hayamiz/items/0f0b7a012ec730351678
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; rers: emacs.rubikitch.com/sd1407/
;; 右から左に読む言語に対応させないことで描写高速化
(setq-default bidi-display-reordering nil)

;; splash scrrenを無効にする
(setq inhibit-splash-screen t)

;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-U C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;; 複数のディレクトリで同じファイル名のファイルを開いた時のバッファ名を調整する
(require 'uniquify)  ;filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;; 以前開いたファイルを再度開いた時、元のカーソル位置を復元する
;; refs: http://www.emacswiki.org/emacs/SavePlace
;; refs: emacs.rubikitch.com/save-place-mode-emacs25/
;; sakashita-net.jp/2017/08/emacs.html
(save-place-mode 1)

;; インデントにTabを使わないようにする
(setq-default indent-tabs-mode nil)

;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; ログの記録行数を増やす
(setq message-log-max 10000)

;; 履歴をたくさん保存する
(setq history-length 1000)

;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; End 初期設定-------------------------------------------

;;; custom-set--------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist '((height . 38) (width . 125) (left . 0) (top . 0))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "UDEV Gothic 35NF" :foundry "nil" :slant normal :weight regular :height 240 :width normal)))))

;;; End custom-set----------------------------------------

;;; straight.el-------------------------------------------
;;; Next-generation, purely functional package manager for the Emacs hacker.
;;; refs: github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Emacs version 27 以上のユーザーは以下を追加
(setq package-enable-at-startup nil)

;; use-package との統合
(straight-use-package 'use-package)

;; el-patch
;; Future-proof your Emacs Lisp customizations!
;; Like the advice system, el-patch provides a way to customize
;; the behavior of Emacs Lisp functions that do not provide
;; enough variables and hooks to let you make them do what you
;; want.
(straight-use-package 'el-patch)

;; To install a package Write Here!-----------------------

;; zenburn-theme
(straight-use-package 'zenburn-theme)

;; Evil
;; Evil is an extensible vi layer for Emacs.
;; Also see our page on EmacsWiki.
(straight-use-package 'evil)
(evil-mode 1)

;; ivy
;; Ivy - a generic completion frontend for Emacs,
(straight-use-package 'ivy)

;; swiper
;; Swiper - isearch with an overview, and more.
(straight-use-package 'swiper)

;; counsel
;; Just call one of the interactive functions in this file
;; to complete the corresponding thing using `ivy'.
(straight-use-package 'counsel)

;; End To install a package Write Here!-------------------

;;; End straight.el---------------------------------------

;;;-------------------------------------------------------
;;; Pagckage Settings
;;;-------------------------------------------------------

;; ivy
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
;;;-------------------------------------------------------
;;; End Pagckage Settings
;;;-------------------------------------------------------

;;;-------------------------------------------------------
;;; Color
;;;-------------------------------------------------------

;; 画面を黒く設定する
;; refs: kei10in.hatenablog.jp/entry/20101101/1288617632
(setq default-frame-alist
      (append
       (list
	'(background-color . "Black")
	'(foreground-color . "Lightgray")
	'(cursor-color . "Gray")
	)
       default-frame-alist))

;; zenburn-theme
;; To Customize just the lighter background colors,
;; you could add to your init file:
;; refs: github.com/bbatsov/zenburn-emacs
(setq zenburn-override-colors-alist
      '(("zenburn-bg+05" . "#282828")
        ("zenburn-bg+1"  . "#2F2F2F")
        ("zenburn-bg+2"  . "#3F3F3F")
        ("zenburn-bg+3"  . "#4F4F4F")))

(load-theme 'zenburn t)
