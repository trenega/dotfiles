;;;; nis's ~/.emacs.d/init.el
;; Takashi Niijima
;; 2023-09-05

;;;;------------------------------------------------------
;;;; Define function
;;;;------------------------------------------------------

;;; load-pathを追加する関数を定義
;;  refer: 「Emacs実践入門」大竹智也 p.61
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
;; ~/.emacs.d/public-repos
(add-to-load-path "straight/repos/melpa/recipes"
                  "public-repos")

;;; other-window-backwordを定義する
;; refer: 「GNU Emacs 拡張ガイド」p.16
(defun other-window-backword (&optional n)    ; version 3
  "Select Nth previous window."
  (interactive "p")
  (if n
      (other-window (- n))  ; nがnilでないとき
    (other-window -1)))     ; nがnilのとき

;;; 一回に1行のスクロールする関数を定義する
;; 関数の名前をわかりやすい名前で参照する
;; refer: 「GNU Emacs 拡張ガイド」p.24
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-one-line-ahead ()
  "Scroll ahead one line."
  (interactive)
  (scroll-ahead 1))

(defun scroll-one-line-behind ()
  "Scroll behind one line."
  (interactive)
  (scroll-behind 1))

;;; カーソルをウィンドウの左上隅にジャンプさせる関数
;; refer: 「GNU Emacs 拡張ガイド」p.26
(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))

;;; カーソルをウィンドウの左下隅にジャンプさせる関数
;; refer: 「GNU Emacs 拡張ガイド」p.26
(defun point-to-bottom ()
  "Put point at beginning of last visible line."
  (interactive)
  (move-to-window-line -1))

;;; 現在カーソルのある行がウィンドウの最初の行になるようにスクロールさせる
;; refer: 「GNU Emacs 拡張ガイド」p.26
(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

;;;; End Define function----------------------------------

;;;;------------------------------------------------------
;;;; Initialization
;;;;------------------------------------------------------

;; 起動画面を表示しない
(setq inhibit-startup-screen t)

;; I use 'eval-expressin'
;; ミニバッファでLisp式の入力を促し、与えられた式を評価して結果を表示する
(put 'eval-expression 'disabled nil)

;; ウィンドウ（フレーム）のサイズ設定する
;; [重要]: (height . 38) を (height . 39) に変更しないこと！！
;; Emacs が立ち上がらなくなる！！
;;(setq default-frame-alist '((width . 84) (height . 38)))
;;(setq default-frame-alist '((width . 125) (height . 38)))

;; 起動時に fullscreen にする
(if (or (eq window-system 'ns) (eq window-system 'darwin))
    (add-hook 'window-setup-hook
              (lambda ()
                (set-frame-parameter nil 'fullscreen 'fullboth))))


;; Alt key -> Meta key setting
;; refer: https://qiita.com/hayamiz/items/0f0b7a012ec730351678
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; rers: emacs.rubikitch.com/sd1407/
;; 右から左に読む言語に対応させないことで描写高速化
(setq-default bidi-display-reordering nil)

;; splash scrrenを無効にする
(setq inhibitrsplash-screen t)

;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-U C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;; 複数のディレクトリで同じファイル名のファイルを開いた時のバッファ名を調整する
(require 'uniquify)  ;filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;; 以前開いたファイルを再度開いた時、元のカーソル位置を復元する
;; refer: http://www.emacswiki.org/emacs/SavePlace
;; refer: emacs.rubikitch.com/save-place-mode-emacs25/
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
;;(menu-bar-mode -1)
(menu-bar-mode t)
(tool-bar-mode -1)
;;(tool-bar-mode t)
(scroll-bar-mode -1)

;;; relative numbering
;; refer: https://www.reddit.com/r/emacs/comments/l7f85b/how_to_toggle_absolute_and_relative_numbering_in/
(defun my/display-set-relative ()
  (setq display-line-numbers 'relative))    ; or 'visual

(defun my/display-set-absolute ()
  (setq display-line-numbers t))

(add-hook 'evil-insert-state-entry-hook #'my/display-set-absolute)
(add-hook 'evil-insert-state-exit-hook #'my/display-set-relative)

;;;「Emacs実践入門」大竹智也[著]
;; 行の折り返し表示を切り替える
;; refer: 「Emacs実践入門」大竹智也[著] p.81
(require 'bind-key)
(bind-key "C-c l" 'toggle-truncate-lines)

;; カラム番号も表示する
(column-number-mode t)

;; タイトルバーにファイルのフルパスを表示する
(setq frame-title-format "%f")

;;; clipboard Setting
;; Emacsから他のエディターにAlt+vでペーストはできるが、その逆にEmacsへは
;; ペーストできない。
;; refer: saitodev.co/article/Emacsでクリップボードを使ってコピペしたい/
(cond (window-system
  (setq x-select-enable-clipboard t)))

;; 対応する括弧を光らせる
(show-paren-mode 1)

;;; End Initialization------------------------------------

;;; custom-set--------------------------------------------
;;  These 'custom-set are from `Emacs'! I don't write here.
;;
;;  highlight-indent-guides
;;  emacs-jp.github.io/packages/package
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'top)
 '(initial-frame-alist '((height . 38) (width . 125) (left . 0) (top . 0)))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "UDEV Gothic 35NF" :foundry "nil" :slant normal :weight regular :height 240 :width normal))))
 '(ivy-current-match ((((class color) (background light)) :background "#FFF3F3" :distant-foreground "#000000") (((class color) (background dark)) :background "#404040" :distant-foreground "#abb2bf")))
 '(ivy-minibuffer-match-face-1 ((((class color) (background light)) :foreground "#666666") (((class color) (background dark)) :foreground "#999999")))
 '(ivy-minibuffer-match-face-2 ((((class color) (background light)) :foreground "#c03333" :underline t) (((class color) (background dark)) :foreground "#e04444" :underline t)))
 '(ivy-minibuffer-match-face-3 ((((class color) (background light)) :foreground "#8585ff" :underline t) (((class color) (background dark)) :foreground "#7777ff" :underline t)))
 '(ivy-minibuffer-match-face-4 ((((class color) (background light)) :foreground "#439943" :underline t) (((class color) (background dark)) :foreground "#33bb33" :underline t))))

;;; End custom-set----------------------------------------

;;;;------------------------------------------------------
;;;; Package Manager Settings
;;;;------------------------------------------------------

;;; straight.el
;;  Next-generation, purely functional package manager for the Emacs hacker.
;;  refer: github.com/radian-software/straight.el
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

;;--------------------------------------------------------
;; To install a package Write Here!
;;--------------------------------------------------------


;; zenburn-theme
(straight-use-package 'zenburn-theme)

;; Evil
;; Evil is an extensible vi layer for Emacs.
;; Also see our page on EmacsWiki.
(straight-use-package 'evil)

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

;; dired-recent
;; A history of paths visited with Emacs dired.
(straight-use-package 'dired-recent)

;; bind-key
(straight-use-package 'bind-key)

;; key-chord
;; Map pairs of simultaneously pressed keys to commands
(straight-use-package 'key-chord)

;; evil-leader
(straight-use-package 'evil-leader)

;; company
(straight-use-package 'company)

;; comment-dwim-2
(straight-use-package 'comment-dwim-2)

;; flycheck
(straight-use-package 'flycheck)

;; flycheck-pos-tip
(straight-use-package 'flycheck-pos-tip)

;; flycheck-haskell
(straight-use-package 'flycheck-haskell)

;; darkroom
(straight-use-package 'darkroom)

;; tempbuf.el
;; 不要なバッファを自動的にkillしてくれる
;; (straight-use-package 'tempbuf)

;; smex
;; M-xを超強化するsmexパッケージ
(straight-use-package 'smex)

;; ido-vertical-mode
;; smexパッケージといっしょに使う
(straight-use-package 'ido-vertical-mode)

;; smartrep
;; プレフィクスキーを省略させる
;; ウィンドウ操作をひとまとめにする
(straight-use-package 'smartrep)

;; evil-surround
(straight-use-package 'evil-surround)

;; ediprolog
(straight-use-package 'ediprolog)

;; clojure-mode
(straight-use-package 'clojure-mode)

;; cider
;; CIDER extends Emacs with support for interactive programming
;; in Clojure.
(straight-use-package 'cider)

;; rainbow-delimiters
(straight-use-package 'rainbow-delimiters)

;; ParEdit
(straight-use-package 'paredit)

;; gauche-mode
(straight-use-package 'gauche-mode)

;; highlight-indent-guides
;; emacs-jp.github.io/packages/package
(straight-use-package 'highlight-indent-guides)

;;--------------------------------------------------------
;; End To install a package Write Here!
;;--------------------------------------------------------

;;; End straight.el---------------------------------------

;;; End Package Manager Settings--------------------------

;;;;------------------------------------------------------
;;;; Pagckage Settings
;;;;------------------------------------------------------

;; Evil Settig
(evil-mode 1)

;; dired-recent
(dired-recent-mode 1)

;;; ivy Settings------------------------------------------
;; refer: https://takaxp.github.io/articles/qiita-helm2ivy.html
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

;; 検索語のハイライト
;; rers: https://takaxp.github.io/articles/qiita-helm2ivy.html


;;; End ivy Settings--------------------------------------

;; counsel Settings
(when (require 'counsel nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
;;  (global-set-key (kbd "C-M-f") 'counsel-ag)

  ;; アクティベート
  (counsel-mode 1))

;; swiper Settings
(when (require 'swiper nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))

;; counsel-recentf 再定義
;; ファイルの表示を`~`から初める設定
;; refer: https://takaxp.github.io/articles/qiita-helm2ivy.html#org87d665a3
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


;;; Evil Leader-----------------------------------------
;; Evil Leader provides the <leader> feature from Vim that
;; provides an easy way to bind keys under a variable prefix key.
;; For an experienced Emacs User it is nothing more than
;; a convoluted key map, but for a Evil user coming from
;; Vim it means an easier start.
;; refer: https://github.com/cofi/evil-leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "s" 'switch-to-buffer             ; Switch to buffer
  "t" 'find-file                    ; find file Table
  "r" 'insert-file                  ; insert-file
  "w" 'save-buffer                  ; Wrote <file>
  "k" 'kill-buffer                  ; Kill buffer
  "q" 'save-buffers-kill-emacs      ; Quit save buffers kill emacs
  "e" 'eval-last-sexp               ; Eval last sexp
  "c" 'slime-compile-defun          ; slime Compile defun
  "l" 'slime-compile-and-load-file  ; slime compile and Load file
  "n" 'other-window                 ; move to next window
  "p" 'other-window-backword        ; move to previous window
  "2" 'split-window-vertically      ; split window vertically
  "3" 'split-window-horizontally    ; vertically split
  "0" 'delete-window                ; delete window
  "1" 'delete-other-windows         ; delete other window "only one"
  "d" 'scroll-one-line-ahead        ; one line scroll up
  "u" 'scroll-one-line-behind       ; one line scroll down
  ">" 'scroll-right                 ; window scroll to right
  "<" 'scroll-left                  ; window scroll to left
  "," 'point-to-top                 ; cursor goto top left corner
  "." 'point-to-bottom              ; cursor goto bottom left corner
  "!" 'line-to-top                  ; Move current line to top of window
  "^" 'enlarge-window               ; window hight up one line
  "-" 'shrink-window                ; window hight down one line
  "}" 'enlarge-window-horizontally  ; window wide one enlargefd
  "{" 'shrink-window-horizontally   ; window wide one shrink
  "+" 'balance-windows              ; windows same size
  "f" 'flycheck-list-errors         ; pop-up errors list
  "a" 'beginning-of-line            ; go to beginning of line
  ";" 'end-of-line                  ; got to end of line
  )

;;; End Evil Leader-------------------------------------

;; smartrep
;; プレフィクスキーを省略させる
;; ウィンドウ操作をひとまとめにする
;; 始めに"C-x"キーを押してから、"一文字"を入力する
(require 'smartrep)
(smartrep-define-key global-map "C-x"
  '(("x" . (other-window))
    ("0" . (delete-window))
    ("1" . (delete-other-windows))
    ("2" . (split-window-below))
    ("3" . (split-window-right))
    ("{" . (shrink-window-horizontally))
    ("}" . (enlarge-window-horizontally))
    ("+" . (balance-windows))
    ("^" . (enlarge-window))
    ("-" . (shrink-window))))

;;; SLIME settings----------------------------------------
;; SBCL をデフォルトのCommon Lisp処理系に設定
;; refer: http://modern-cl.blogspot.com/2011/04/3-slime.html
;; (setq inferior-lisp-program "sbcl")  ;Steel Bank Comoon Lisp
(setq inferior-lisp-program "ccl")      ;Clozure CL
;; cclをload-pathに追加
(add-to-list 'load-path (expand-file-name
             "~/.roswell/impls/x86-64/darwin/ccl-bin/1.12.2/dx86cl64"))

;; ~/.emacs.d-straight/slimeをload-pathに追加
(add-to-list 'load-path (expand-file-name "~/.emacs.d-straight/slime"))
;; SLIMEのロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner slime-indentation))
;; SLIMEからの入力をUTF-8に設定
(setq slime-net-coding-system 'utf-8-unix)

;; ros install slime したので、追記
(load (expand-file-name "~/.roswell/helper.el"))

;;; company Setting---------------------------------------
;; 補完用パッケージ
;; refer: https://qiita.com/sune2/items/b73037f9e85962f5afb7
(require 'company)
;; (global-company-mode t) ; 全バッファで有効にする
(add-hook 'after-init-hook 'global-company-mode) ; refer: http://company-mode.github.io/
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

;; 色付けをauto-complete風に変更する
;; refer: https://qiita.com/syohex/items/8d21d7422f14e9b53b17
(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "orange")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")

;; Keybind
;; refer: https://qiita.com/syohex/items/8d21d7422f14e9b53b17
(global-set-key (kbd "C-M-i") 'company-complete)

;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; C-sで絞り込む
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

;; TABで候補を設定
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;;; End company Settings-----------------------------------

;;; Flycheck Settings--------------------------------------
;; MacOS $PATH環境変数を修正する
;; refer: https://www.flycheck.org/en/latest/
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (require 'bind-key)
;; (bind-key "M-n" 'flycheck-next-error)
;; (bind-key "M-p" 'flycheck-previous-error)

;; flycheck-pos-tip
;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;; Disply Flycheck error list window
;; refer: blog.3qe.us/entry/2022/09/29/124700
;; (add-to-list 'display-buffer-alist
;; 	     `(,(rx bos "*Flycheck errors*" eos)
;;               (display-buffer-reuse-window
;;                display-buffer-in-side-window)
;;               (side            . bottom)
;;               (reusable-frames . visible)
;;               (window-height   . 0.2)))

;;; End Flycheck Settings---------------------------------

;; tempbuf
;; automatically kill unnecessary buffers
;; refer: shigemk2.com/entry/20120908/1347090453
;; (require 'tempbuf)
;; (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;; smex
;; M-x を超強化するsmexパッケージ
(setq ido-max-window-height 0.75)
(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(smex-initialize)
(require 'bind-key)
(bind-key "M-x" 'smex)
(bind-key "M-X" 'smex-major-mode-commands)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; CIDER
(require 'cider)
;; clojure-modeでCIDERを有効にする
(add-hook 'clojure-mode-hook 'cider-mode)
;; CIDERのstart bannerを消す
(setq-default cider-repl-display-help-banner nil)

;; ParEdit
(require 'paredit)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

;; highlight-indent-guides
;; emacs-jp.github.io/packages/package
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;; End Pagckage Settings-------------------------------

;;;;----------------------------------------------------
;;; Custom Keybind
;;;;----------------------------------------------------

;; C-hをBackspaceに変更、C-?にhelpをmapping
;; refer: malkalech.com/emacs_c-h_backspac:
;; C-h -> delete-backward-char
(define-key key-translation-map [?\C-h] [?\C-?])

;; C-h -> Backspace
(require 'bind-key)
(bind-key* "C-h" 'delete-backward-char)

;; C-? -> help
;; (global-set-key (kbd "C-?") 'help-for-help)
(bind-key* "C-?" 'help-for-help)

;; "fd" to Esc
;; Exit instert mode by pressing f and then d quickly
;; https://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
(key-chord-mode 1)
(setq key-chord-two-keys-delay           0.15
      key-chord-safety-interval-backward 0.1
      key-chord-safety-interval-forward  0.25)
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

;; A TWO-key chord-------------------------------------
(key-chord-mode 1)
(setq key-chord-two-keys-delay           0.15
      key-chord-safety-interval-backward 0.1
      key-chord-safety-interval-forward  0.25)

;; Don't use shift key. But I can type " ~ ! @ # $ % ^ & * ( ) _ + : " ".
;; (key-chord-define-global ";`"  "~")
;; (key-chord-define-global ";1"  "!")
;; (key-chord-define-global ";2"  "@")
;; (key-chord-define-global ";3"  "#")
;; (key-chord-define-global ";4"  "$")
;; (key-chord-define-global ";5"  "%")
;; (key-chord-define-global ";6"  "^")
;; (key-chord-define-global "a7"  "&")
;; (key-chord-define-global "a8"  "*")
;; (key-chord-define-global "a9"  "(")
;; (key-chord-define-global "a0"  ")")
;; (key-chord-define-global "a-"  "_")
;; (key-chord-define-global "a="  "+")

;; scheme-load-file
(key-chord-define-global "sl" 'scheme-load-file)

;; switch-to-scheme
(key-chord-define-global "go" 'switch-to-scheme)  ; "go" -> "gosh>"

;; indent-sexp
(key-chord-define-global "is" 'indent-sexp)

;; End A TWO-key chord----------------------------------

;; 著者が勧める時間節約法
;; refer: UNIX POWER TOOLS 19.7 著者が勧める時間節約法 p.468
;; CTRL-zおよびESC-zで、画面を1行「上」または「下」にスクロールする。
(defun scroll-up-one ( ) "Scroll up 1 line." (interactive)
       (scroll-up (prefix-numeric-value current-prefix-arg)))
(defun scroll-down-one ( ) "Scroll down 1 line." (interactive)
  (scroll-dowon (prefix-numeric-value current-prefix-arg)))
;;(define-key global-map "\C-z" 'scroll-up-one)
;;(define-key global-map "\M-z" 'scroll-down-one)

;; comment out
;; comment-dwim-2
(require 'bind-key)
(bind-key "M-;" 'comment-dwim-2)

;; C-u -> scroll up
;; org-modeと関連パッケージには、C-uに多くの機能が付属してます。
;; refer: stackoverflow.com/questions/14302171/ctrlu-in-emacs-when-using-evil-key-bindings
;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-insert-state-map (kbd "C-u")
;; 	    (lambda ()
;; 	      (interactive)
;; 	      (evil-delete (point-at-bol) (point))))

;; M-x zap-up-to-char -> M-z
;; refer: emacs.rubikitch.com/sd1507-builtin/
;; 指定した文字の直前までを削除する
;; (require 'misc)
;; (bind-key "M-z" 'zap-up-to-char)
;; 削除対象をハイライトしてくれる
(require 'bind-key)
(bind-key "M-z" 'zop-up-to-char)

;; 単語移動の亜種
;; refer: emacs.rubikitch.com/sd1507-builtin/
(require 'misc)
(require 'bind-key)
(bind-key "M-f" 'forward-to-word)  ;移動先が先頭になる
(bind-key "M-b" 'backward-to-word) ;移動先が末尾になる

;; 設定ファイル用のメジャーモードの定義
;; refer: emacs.rubikitch.com/sd1508-emacs-column/
(require 'generic-x)

;; SWI-Prolog Settings
;; (global-set-key "\C-c\C-e" 'ediprolog-dwim)
;; (require 'ediprolog)
;;(setq ediprolog-system 'swi)
(setq prolog-system 'swi)  ; optional, the system you are using;
                            ; see `prolog-system' below for possible values
(setq auto-mode-alist (append '(("\\.pl\\'" . prolog-mode)
                                 ("\\.m\\'" . mercury-mode))
                                auto-mode-alist))

;; view-mode と通常モードの切り替えコマンド
;; https://mugijiru.github.io/.emacs.d/editing/view-mode/
(defun my/toggle-view-mode ()
  "view-mode と通常モードの切り替えコマンド"
  (interactive)
  (cond (view-mode
         (view-mode -1))
        (t
         (view-mode 1))))

;; whitespace-mode
;; https://mugijiru.github.io/.emacs.d/editing/view-mode/
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         spaces
                         empty))

;; 保存時に自動的に余計な空白を消す
;; https://mugijiru.github.io/.emacs.d/editing/view-mode/
(setq whitespace-action '(auto-cleanup))

;; whitespace-mode
;;(global-whitespace-mode 1)


;; alias--------------------------------------------------
;; alias my-keys
(defalias 'my-key 'describe-personal-keybindings)

;; alias bindings
(defalias 'bindings 'describe-bindings)

;; alias indent-sexp
(defalias 'is 'indent-sexp)

;; End alias----------------------------------------------

;;; End Custom Keybind------------------------------------

;;;;------------------------------------------------------
;;;; Customize Settings
;;;;------------------------------------------------------

;; cideraで補完候補をfuzzy matchさせる
;; refer: docs.cider.mx/cider/usage/code_completion.html#fuzzy-condidate-matching
(add-hook 'cider-repl-mode-hook
          #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook
          #'cider-company-enable-fuzzy-completion)

;; Mac OS のクリップボードと同期する
;; https://suwaru.tokyo/%E3%80%90%E7%B0%A1%E5%8D%98%E3%80%91emacs%E5%9F%BA%E6%9C%AC%E7%9A%84%E3%81%AA%E8%A8%AD%E5%AE%9A%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%81%AE%E6%9B%B8%E3%81%8D%E6%96%B9%E3%80%90init-el%E3%80%91/
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; "yes or no" の選択を "y or n" にする
;;(fset 'yes-or-no-p 'y-or-n-p)

;; End suraru.tokyo

;;; Gauche------------------------------------------------
;; Schemeの処理系
;; refer: https://haskell.hatenablog.com/entry/Settings-to-use-Gauche_Scheme-with-Emacs
;; 日本語を扱うことを可能にする
(modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))

;; Gauche Settings
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for scheme." t)
(autoload 'run-scheme "cmuscheme" "Run a n inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
    (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

;;(require 'gauche-mode-autoloads)

;;; End Gauche--------------------------------------------

;;; Schemeの式をEmacsで簡単に実行する------------------------
;; refer: https://ryu-tk.hatenablog.com/entry/2021/12/21/013537
;; (defun my-execute-scheme ()
;;   (interactive)
;;   (if (string= mode-name "Scheme")
;;       (progn (run-scheme scheme-program-name)
;;              (end-of-buffer)
;;              (other-window -1)
;;              (scheme-send-last-sexp))
;;     )
;;   )

;; (add-hook 'scheme-mode-hook
;;           '(lambda ()
;;              (define-key scheme-mode-map (kbd "C-c C-j") 'my-execute-scheme)))


;; (defun my-execute-scheme-all ()
;;   (interactive)
;;   (if (string= mode-name "Scheme")
;;       (progn (run-scheme scheme-program-name)
;;              (end-of-buffer)
;;              (other-window -1)
;;              (scheme-load-file (buffer-file-name (current-buffer))))))
;; ;;(global-set-key (kbd "C-c C-a") 'my-execute-scheme-all)
;; (add-hook 'scheme-mode-hook
;;           '(lambda ()
;;              (define-key scheme-mode-map (kbd "C-c C-a") 'my-execute-scheme-all)))

;;; End Schemeの式をEmacsで簡単に実行する--------------------


;;; End Customize Settings--------------------------------

;;;;------------------------------------------------------
;;;; Color
;;;;------------------------------------------------------

;;; 画面を黒く設定する
;; refer: kei10in.hatenablog.jp/entry/20101101/1288617632
(setq default-frame-alist
      (append
       (list
        '(background-color . "Black")
        '(foreground-color . "Lightgray")
        '(cursor-color . "Gray")
        )
       default-frame-alist))

;;; zenburn-theme
;; To Customize just the lighter background colors,
;; you could add to your init file:
;; refer: github.com/bbatsov/zenburn-emacs
(setq zenburn-override-colors-alist
      '(("zenburn-bg+05" . "#282828")
        ("zenburn-bg+1"  . "#2F2F2F")
        ("zenburn-bg+2"  . "#3F3F3F")
        ("zenburn-bg+3"  . "#4F4F4F")))

(load-theme 'zenburn t)

;;; visual modeの範囲指定を見易くする
;; refs :https://cortyuming.hateblo.jp/entry/20140218/p1
(set-face-attribute 'highlight nil :foreground 'unspecified)

;;; Emacsで背景色の透明度を変更する------------------------
;; http://osanai.org/17/
;; (if window-system (progn
;;   (set-background-color "Black")
;;   (set-foreground-color "LightGray")
;;   (set-cursor-color "Gray")
;;   (set-frame-parameter nil 'alpha 50))) ;透明度

;;; Emacsの画面に透明度を設定する
(defun set-transparency ()
  "set frame transparency"
  (set-frame-parameter nil 'alpha 50))  ;透明度

;;; 透明度を変更するコマンド M-x set-alpha
;; refer: http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(50))))

;;; End Emacsで背景色の透明度を変更する--------------------

;;;; End Color-----------------------------------------

;;; ~/.emacs.d/init.el ends here
