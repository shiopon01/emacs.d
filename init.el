;;; init.el --- Summary
;;; Commentary:
;;; Code:

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; Now you can use leaf!
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

;; Native Compile
(eval-and-compile
  (leaf *byte-compile
    :custom
    (byte-compile-warnings . '(not free-vars))
    (debug-on-error        . nil)))
(leaf *native-compile
  :doc "Native Compile by gccemacs"
  :url "https://www.emacswiki.org/emacs/GccEmacs"
  :if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  :custom
  (comp-deferred-compilation . nil)
  (comp-speed                . 5)
  (comp-num-cpus             . 4)
  :config
  (native-compile-async "~/.emacs.d/early-init.el" 4 t)
  (native-compile-async "~/.emacs.d/init.el" 4 t)
  (native-compile-async "~/.emacs.d/elpa/" 4 t)
  (native-compile-async "~/.emacs.d/el-get/" 4 t))

;; -----------------------------------------------------------------------------------------
;;
;; Global Settings
;;
;; -----------------------------------------------------------------------------------------

;; 日本語設定
(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; Builtin Properties
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((user-full-name . "shiopon01")
           (user-mail-address . "shio.0323@gmail.com")
           (user-login-name . "shiopon01")
           (truncate-lines . t) ;; デフォルトで折り返さない
           (menu-bar-mode . t) ;; メニューバー有効
           (tool-bar-mode . nil) ;; ツールバー無効
           (scroll-bar-mode . nil) ;; スクロールバー有効
           (indent-tabs-mode . nil)
           ;; additional
           ;; (initial-scratch-message . "") ;; scratchのメッセージを消す
           ;; (debug-on-error . t) ;; エラー時にデバッグ
           ;; (frame-resize-pixelwise . t) ;; フレームサイズの指定
           ;; (enable-recursive-minibuffers . t) ;; 
           ;; (history-length . 1000)
           ;; (history-delete-duplicates . t)
           ;; (scroll-preserve-screen-position . t)
           ;; (scroll-conservatively . 100)
           ;; (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
           ;; (ring-bell-function . 'ignore)
           ;; (text-quoting-style . 'straight)
           ;; (use-dialog-box . nil)
           ;; (use-file-dialog . nil)
           ;; (show-paren-mode . t)     ;; 対応する括弧をハイライト表示させる
           ;; (blink-cursor-mode . nil) ;; カーソルの点滅を止める
           ;; (global-hl-line-mode . t) ;; 現在行を目立たせる
           ;; (line-number-mode . t)    ;; カーソルの位置が何行目かを表示する
           (column-number-mode . t) ;; 列番号を表示する
           (vc-follow-symlinks . t) ;; シンボリックリンクをたどるときの質問を回避
           (completion-ignore-case . t)) ;; ファイル名の補完で大文字と小文字の区別をなくす
  :config
  (defalias 'yes-or-no-p 'y-or-n-p) ;; yes/noをy/nにする
  (keyboard-translate ?\C-h ?\C-?)) ;; C-?をヘルプにする

;; Key Binding
(leaf *global-bindings
  :init
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  :bind
  (("C-h" . delete-backward-char)
   ("C-m" . electric-newline-and-maybe-indent)
   ("C-j" . newline-and-indent)
   ("M-:" . comment-dwim)
   ;; Move
   ;; ("M-p" . (lambda () (interactive) (forward-line -10)))
   ;; ("M-n" . (lambda () (interactive) (forward-line 10)))
   ;; Move Buffer
   ("M-," . previous-buffer)
   ("M-." . next-buffer)
   ;; Shell Command
   ("M-!" . async-shell-command)
   ("M-@" . shell-command)
   ;; Browse
   ("C-t" . other-window)
   ("C-c C-o" . browse-url-or-copy)
   ("<f9>"  . sr-speedbar-toggle)
   ("<f12>" . eval-buffer))) ;; Reload config

;; バックアップ・ロック関連
;; (leaf *files
;;   doc "file input and output commands for Emacs"
;;   :tag "builtin"
;;   :custom ((make-backup-files . nil) ;; *.~ バックアップファイルを作成しない
;;            (backup-inhibited . t)    ;; *.~ バックアップファイルを作成しない
;;            (auto-save-default . nil) ;; .#* ロックファイルを作成しない
;;            (create-lockfiles . nil) ;; .#* ロックファイルを作成しない
;;            (auto-save-timeout . 15)
;;            (auto-save-interval . 60)
;;            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
;;            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
;;                                        (,tramp-file-name-regexp . nil)))
;;            (version-control . t)
;;            (delete-old-versions . t)))

;; ;; emacs内でのMacキー設定
;; (leaf *macos
;;   doc "macosのキー設定"
;;   :custom ((ns-command-modifier . (quote meta))
;;            (ns-right-command-modifier . (quote hyper))
;;            (ns-alternate-modifier . (quote super))
;;            (ns-right-alternate-modifier . (quote alt))))

;; (leaf *window-maximizer
;;   :doc "Maximize current window"
;;   :custom
;;   (is-window-maximized . nil)
;;   :preface
;;   (defun toggle-window-maximize ()
;;     (interactive)
;;     (progn
;;       (if is-window-maximized
;;           (balance-windows)
;;         (maximize-window))
;;       (setq is-window-maximized
;;             (not is-window-maximized)))))

;; (leaf startup
;;   :doc "process Emacs shell arguments"
;;   :tag "builtin" "internal"
;;   :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file ".cache/.saves-"))))

;; -----------------------------------------------------------------------------------------
;;
;; Emacs Theme
;;
;; -----------------------------------------------------------------------------------------

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :defer-config
  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
    (setq standard-display-table display-table))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; フレーム設定
(leaf *frame-setting
  :init
  (set-frame-parameter nil 'alpha 97)              ;; 透明度設定
  (set-frame-parameter nil 'fullscreen 'maximized) ;; 起動時に最大化
  :preface
  (defun set-alpha (alpha-num)
    (interactive "nAlpha: ")
    (set-frame-parameter nil 'alpha (cons alpha-num '(97)))))

;; -----------------------------------------------------------------------------------------
;;
;; Package Settings
;;
;; -----------------------------------------------------------------------------------------

;; バッファーを自動再読み込み
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

;; check the spells
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :config
  (leaf flycheck-package
    :doc "A Flycheck checker for elisp package authors"
    :ensure t
    :config
    (flycheck-package-setup))

  (leaf flycheck-elsa
    :doc "Flycheck for Elsa."
    :emacs>= 25
    :ensure t
    :config
    (flycheck-elsa-setup))
  ;; ...
  )

(leaf dmacro
  :ensure t
  :custom `((dmacro-key . ,(kbd "C-c m")))
  :global-minor-mode global-dmacro-mode)

;; -----------------------------------------------------------------------------------------
;;
;; custom-set-
;;
;; -----------------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(flycheck-elsa flycheck-package flycheck transient-dwim leaf-convert leaf-tree blackout el-get hydra leaf-keywords leaf)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
