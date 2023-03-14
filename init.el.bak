;;; init.el --- ladicle's init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
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

;; You can also configure builtin package via leaf!

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
;; Generic Configurations
;;
;; -----------------------------------------------------------------------------------------

;; 日本語設定
(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; ビルドイン設定
(leaf *cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((user-full-name . "shiopon01")
           (user-mail-address . "shio.0323@gmail.com")
           (user-login-name . "shiopon01")
           (initial-scratch-message . "") ;; scratchのメッセージを消す
           (truncate-lines . t) ;; デフォルトで折り返さない
           (menu-bar-mode . t)   ;; メニューバー有効
           (tool-bar-mode . nil) ;; ツールバー無効
           (scroll-bar-mode . t) ;; スクロールバー有効
           (debug-on-error . t) ;; エラー時にデバッグ
           (frame-resize-pixelwise . t) ;; フレームサイズの指定
           (enable-recursive-minibuffers . t) ;; 
           (history-length . 1000)
           (history-delete-duplicates . t)
           (scroll-preserve-screen-position . t)
           (scroll-conservatively . 100)
           (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
           (ring-bell-function . 'ignore)
           (text-quoting-style . 'straight)
           (truncate-lines . t)
           (use-dialog-box . nil)
           (use-file-dialog . nil)
           (menu-bar-mode . t)
           (tab-bar-mode . t)
           (scroll-bar-mode . nil)  ;; 
           (show-paren-mode . t)     ;; 対応する括弧をハイライト表示させる
           (blink-cursor-mode . nil) ;; カーソルの点滅を止める
           (global-hl-line-mode . t) ;; 現在行を目立たせる
           (line-number-mode . t)    ;; カーソルの位置が何行目かを表示する
           (column-number-mode . t)  ;; 列番号を表示する
           (inhibit-startup-message . t) ;; 起動メッセージを削除
           (indent-tabs-mode . nil))  ;; 
  :config
  (defalias 'yes-or-no-p 'y-or-n-p) ;; yes/noをy/nにする
  (keyboard-translate ?\C-h ?\C-?)) ;; C-?をヘルプにする

;; バックアップ・ロック関連
(leaf *files
  doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom ((make-backup-files . nil) ;; *.~ バックアップファイルを作成しない
           (backup-inhibited . t)    ;; *.~ バックアップファイルを作成しない
           (auto-save-default . nil) ;; .#* ロックファイルを作成しない
           (create-lockfiles . nil) ;; .#* ロックファイルを作成しない
           (auto-save-timeout . 15)
           (auto-save-interval . 60)
           (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
           (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                       (,tramp-file-name-regexp . nil)))
           (version-control . t)
           (delete-old-versions . t)))

;; emacs内でのMacキー設定
(leaf *macos
  doc "macosのキー設定"
  :custom ((ns-command-modifier . (quote meta))
           (ns-right-command-modifier . (quote hyper))
           (ns-alternate-modifier . (quote super))
           (ns-right-alternate-modifier . (quote alt))))

(leaf *window-maximizer
  :doc "Maximize current window"
  :custom
  (is-window-maximized . nil)
  :preface
  (defun toggle-window-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
          (balance-windows)
        (maximize-window))
      (setq is-window-maximized
            (not is-window-maximized)))))

;; ファイル名の補完で大文字と小文字の区別をなくす
(setq completion-ignore-case t)

;; 前回の編集場所を記録する
(load "saveplace")
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.emacs-places")

(setq bookmark-default-file "~/.emacs.d/.emacs.bmk") ;; bookmark
(global-auto-revert-mode 1) ;; バッファ自動再読み込み
(setq vc-follow-symlinks t) ;; パッケージ管理下のシンボリックリンクをたどるときの質問を回避する

;; (leaf startup
;;   :doc "process Emacs shell arguments"
;;   :tag "builtin" "internal"
;;   :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file ".cache/.saves-"))))

(leaf package-utils
  :doc "Interactive package manager"
  :url "https://github.com/Silex/package-utils"
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *formatting
  :custom
  (truncate-lines        . t)
  (require-final-newline . t)
  (tab-width             . 2)
  (indent-tabs-mode      . nil))

;; バッファ自動再読み込み
(leaf *autorevert
  :doc "Revert changes if local file is updated"
  :global-minor-mode global-auto-revert-mode
  :custom (auto-revert-interval . 0.1)
  :config (global-auto-revert-mode 1))

(leaf *recovery
  :doc "Save place of cursor"
  :global-minor-mode save-place-mode)

(leaf *tramp
  :doc "Edit remote file via SSH or SCP"
  :custom
  (tramp-auto-save-directory . "~/.emacs.d/.cache/tramp/")
  (tramp-chunksize           . 2048))

(leaf *savehist
  :doc "save history of minibuffer"
  :global-minor-mode savehist-mode)

;; recentfに保存する履歴などの設定
(leaf *recentf
  :doc "Record open files history"
  :global-minor-mode recentf-mode
  :custom
  (recentf-max-saved-items . 20000)
  (recentf-max-menu-items  . 20000)
  (recentf-save-file       . "~/.emacs.d/.recentf")
  (recentf-auto-cleanup    . 'never)
  (recentf-exclude
   . '((expand-file-name package-user-dir)
		   ".cache"
		   "cache"
       "bookmarks"
		   "recentf"
       "*.png"
       "*.jpeg"
       ".org_archive"
		   "COMMIT_EDITMSG\\'")))

(leaf *large-file
  :doc "Adjust large file threshold"
  :custom
  (large-file-warning-threshold . 1000000))

;; Basic Editing Operation
(leaf *delsel
  :doc "Replace the region just by typing text, or delete just by hitting the DEL key"
  :global-minor-mode delete-selection-mode)

(leaf undo-fu
  :doc "Redo and Undo operations"
  :url "https://github.com/emacsmirror/undo-fu"
  :ensure t
  :bind*
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))

;; mysetting

(global-display-line-numbers-mode t)                         ;; 行番号表示(Emacs26以降)
(custom-set-variables '(display-line-numbers-width-start t)) ;; 行番号表示(Emacs26以降)


(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

;; -----------------------------------------------------------------------------------------
;;
;; Global Key Bindings
;;
;; -----------------------------------------------------------------------------------------

(leaf *global-bindings
  :init
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  :bind
  (("C-h" . delete-backward-char)
   ("C-m" . electric-newline-and-maybe-indent)
   ("C-j" . newline-and-indent)
   ("M-:" . comment-dwim)
   ;; Move
   ("M-p" . (lambda () (interactive) (previous-line 10)))
   ("M-n" . (lambda () (interactive) (next-line 10)))
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

;; -----------------------------------------------------------------------------------------
;;
;; Window Layout
;;
;; -----------------------------------------------------------------------------------------

(leaf ace-window
  :doc "Select window like tmux"
  :url "https://github.com/abo-abo/ace-window"
  :ensure t
  :bind
  ("C-o" . ace-window)
  :custom
  (aw-keys . '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :custom-face
  (aw-leading-char-face . '((t (:height 4.0 :foreground "#f1fa8c")))))

(leaf rotate
  :doc "Rotate the layout like tmux panel"
  :url "https://github.com/daichirata/emacs-rotate"
  :el-get "daichirata/emacs-rotate"
  :require t)

(leaf presentation
  :doc "Scale all for presentation"
  :url "https://github.com/zonuexe/emacs-presentation-mode"
  :ensure t)

(leaf *window-maximizer
  :doc "Maximize current window"
  :custom
  (is-window-maximized . nil)
  :preface
  (defun toggle-window-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
          (balance-windows)
        (maximize-window))
      (setq is-window-maximized
            (not is-window-maximized)))))

(leaf *window-transparency
  :doc "Set window transparency level"
  :hook (after-init-hook . toggle-window-transparency)
  :custom
  (window-transparency . 88)
  :preface
  (defun toggle-window-transparency ()
    "Cycle the frame transparency from default to transparent."
    (interactive)
    (let ((transparency window-transparency)
          (opacity 100))
      (if (and (not (eq (frame-parameter nil 'alpha) nil))
               (< (frame-parameter nil 'alpha) opacity))
          (set-frame-parameter nil 'alpha opacity)
        (set-frame-parameter nil 'alpha transparency)))))

;; -----------------------------------------------------------------------------------------
;;
;; Error Checker
;;
;; -----------------------------------------------------------------------------------------

(leaf flycheck
  :doc "Syntax checker"
  :url "https://www.flycheck.org/en/latest/"
  :ensure t
  :global-minor-mode global-flycheck-mode
  :custom
  (flycheck-display-errors-delay . 0))

;; -----------------------------------------------------------------------------------------
;;
;; Completion
;;
;; -----------------------------------------------------------------------------------------

(leaf yasnippet
  :doc "Template system"
  :url "https://github.com/joaotavora/yasnippet"
  :ensure t
  :hook   (prog-mode-hook . yas-minor-mode)
  :custom (yas-snippet-dirs . '("~/.emacs.d/snippets"))
  :config (yas-reload-all))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2022-10-30"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))
  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf company
  :doc "Modular in-buffer completion framework"
  :url "http://company-mode.github.io/"
  :ensure t
  :hook (prog-mode-hook . company-mode)
  :bind
  ((:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("<tab>" . company-complete-common-or-cycle))
   (:company-search-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)))
  :custom
  (company-idle-delay  . 0)
  (company-echo-delay  . 0)
  (company-ignore-case . t)
  (company-selection-wrap-around . t)
  (company-minimum-prefix-length . 1)
  :custom-face
  (company-tooltip          . '((t (:background "#323445"))))
  (company-template-field   . '((t (:foreground "#ff79c6"))))
  (yas-field-highlight-face . '((t (:foreground "#ff79c6")))))

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; -----------------------------------------------------------------------------------------
;;
;; Programming Mode
;;
;; -----------------------------------------------------------------------------------------

(leaf quickrun
  :doc "Run program quickly"
  :url "https://github.com/emacsorphanage/quickrun"
  :ensure t
  :require t)

;; LSP
(leaf lsp-mode
  :doc "Client for language server protocol"
  :url "https://emacs-lsp.github.io"
  :ensure t
  :custom
  ;; General settings
  (lsp-auto-guess-root                . t)
  (lsp-modeline-diagnostics-enable    . t)
  (lsp-headerline-breadcrumb-enable   . nil)
  ;; Performance tuning
  (lsp-log-io               . nil)
  (lsp-print-performance    . nil)
  (lsp-completion-provider  . :none)
  (lsp-enable-file-watchers . nil)
  (lsp-idle-delay           . 0.500)
  (gc-cons-threshold        . 100000000)
  (read-process-output-max  . 1048576)
  :bind
  (:lsp-mode-map
   ("C-c r"   . lsp-rename)
   ("C-c C-c" . lsp-execute-code-action))
  :hook
  (lsp-mode-hook
   . (lambda ()
       (setq-local company-backends '((company-yasnippet company-capf :separate))))))
(leaf lsp-ui
  :doc "UI integrations for lsp-mode"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :custom
  (lsp-ui-flycheck-enable     . t)
  (lsp-ui-sideline-enable     . t)
  (lsp-ui-sideline-show-hover . nil)
  (lsp-ui-imenu-enable        . nil)
  (lsp-ui-peek-fontify        . 'on-demand)
  (lsp-ui-peek-enable         . t)
  (lsp-ui-doc-enable          . nil)
  (lsp-ui-doc-max-height      . 12)
  (lsp-ui-doc-max-width       . 56)
  (lsp-ui-doc-position        . 'at-point)
  (lsp-ui-doc-border          . "#323445")
  :custom-face
  (lsp-ui-doc-background . '((t (:background "#282a36"))))
  (lsp-ui-doc-header     . '((t (:foreground "#76e0f3" :weight bold))))
  (lsp-ui-doc-url        . '((t (:foreground "#6272a4"))))
  :bind
  ((:lsp-mode-map
    ("C-c C-r"   . lsp-ui-peek-find-references)
    ("C-c C-j"   . lsp-ui-peek-find-definitions)
    ("C-c C-M-j" . xref-find-definitions-other-window)
    ("C-c i"     . lsp-ui-peek-find-implementation)
    ("C-c m"     . counsel-imenu)
    ("C-c M"     . lsp-ui-imenu)
    ("C-c s"     . toggle-lsp-ui-sideline)
    ("C-c d"     . toggle-lsp-ui-doc))
   (:lsp-ui-doc-mode-map
    ("q"         . toggle-lsp-ui-doc)
    ("C-i"       . lsp-ui-doc-focus-frame)))
  :init
  (defun toggle-lsp-ui-sideline ()
    (interactive)
    (if lsp-ui-sideline-show-hover
        (progn
          (setq lsp-ui-sideline-show-hover nil)
          (message "sideline-hover disabled :P"))
      (progn
        (setq lsp-ui-sideline-show-hover t)
        (message "sideline-hover enabled :)"))))
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame)
          (message "lsp-ui-doc disabled :P"))
      (progn
        (lsp-ui-doc-mode 1)
        (message "lsp-ui-doc enabled :)")))))

;; Debugger
(leaf dap-mode
  :doc "Client for Debug Adapter Protocol"
  :url "https://emacs-lsp.github.io/dap-mode/"
  :ensure t
  :defer-config
  (require 'dap-hydra)
  (require 'dap-go)
  (require 'dap-gdb-lldb))

;; Rust
(leaf rustic
  :doc "Rust development environment"
  :url "https://github.com/brotzeit/rustic"
  :ensure t
  :require t
  :hook (rust-mode-hook . lsp-deferred)
  :bind
  (:rust-mode-map
   ("C-c C-n" . rust-run)
   ("C-c C-a" . quickrun-with-arg))
  :custom
  (rustic-format-trigger . 'on-save)
  (rustic-babel-auto-wrap-main . t))

;; Go
(leaf go-mode
  :doc "Go development environment"
  :url "https://github.com/dominikh/go-mode.el"
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode-hook
   . (lambda ()
       (lsp-deferred)
       (add-hook 'before-save-hook #'lsp-format-buffer t t)
       (add-hook 'before-save-hook #'lsp-organize-imports t t)))
  :bind
  (:go-mode-map
   ("C-c C-n" . go-run)
   ("C-c v"   . go-mod-vendor))
  :preface
  (defun go-mod-vendor ()
    "Run 'go mod vendor' at repository root."
    (interactive)
    (progn
      (call-process-shell-command (concat "cd " (vc-root-dir) "; go mod vendor") nil 0)
      (message "Run 'go mod vendor'!"))))
(leaf gotest
  :doc "Run Go unit-tests"
  :url "https://github.com/nlamirault/gotest.el"
  :ensure t
  :require t
  :bind
  (:go-mode-map
   ("C-c t" . go-test-clean-and-current-test)
	 ("C-c f" . go-test-clean-and-current-file)
   ("C-c T" . go-test-current-test)
	 ("C-c F" . go-test-current-file)
	 ("C-c a" . go-test-current-project))
  :preface
  (defun go-test-clean-and-current-test()
    (interactive)
    (progn
      (call-process-shell-command "go clean -testcache" nil 0)
      (go-test-current-test)))
  (defun go-test-clean-and-current-file()
    (interactive)
    (progn
      (call-process-shell-command "go clean -testcache" nil 0)
      (go-test-current-file))))
(leaf go-gen-test
  :doc "Generate tests for go code"
  :url "https://github.com/s-kostyaev/go-gen-test"
  :ensure t)
(leaf go-eldoc
  :doc "Show eldoc for Go functions"
  :url "https://github.com/emacsorphanage/go-eldoc"
  :ensure t
  :hook (go-mode-hook . go-eldoc-setup))
(leaf go-tag
  :doc "Generate & Edit field tags for golang struct fields"
  :url "https://github.com/brantou/emacs-go-tag"
  :ensure t)

;; Javascript
(leaf js2-mode
  :doc "Improved JavaScript editing mode"
  :url "https://github.com/mooz/js2-mode"
  :ensure t
  :mode "\\.js\\'")
(leaf typescript-mode
  :doc "Major-mode for editing Typescript-files"
  :url "https://github.com/emacs-typescript/typescript.el"
  :ensure t
  :mode "\\.ts\\'")

;; ;; Shell Script
(leaf fish-mode
  :doc "Major-mode for fish shell scripts"
  :url "https://github.com/wwwjfy/emacs-fish"
  :ensure t
  :mode "\\.fish\\'")

;; speedbar
(leaf sr-speedbar
  :doc "Same frame speedbar"
  :tag "sr-speedbar.el" "speedbar"
  :url "http://www.emacswiki.org/emacs/download/sr-speedbar.el"
  :ensure t
  :custom
  (sr-speedbar-right-side . nil)
  (speedbar-show-unknown-files . t)
  (speedbar-directory-unshown-regexp . "^\(\.\.*$\)\'"))

;; -----------------------------------------------------------------------------------------
;;
;; Configuration Language
;;
;; -----------------------------------------------------------------------------------------

(leaf yaml-mode
  :doc "Major mode for editing files in the YAML data serialization format"
  :url "https://github.com/yoshiki/yaml-mode"
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :defvar yaml-file-threshold
  :custom
  (lsp-yaml-schemas . '(:file:///Users/aigarash/zap-standalone/all.json "/*.yaml"))
  (yaml-file-threshold . 100000)
  :hook
  (yaml-mode-hook
   . (lambda ()
       (if (< (buffer-size (current-buffer)) yaml-file-threshold)
           (progn
             (if (window-system)
                 (highlight-indent-guides-mode))
             (yas-minor-mode)
             (lsp-deferred)))))
  :custom-face
  (font-lock-variable-name-face . '((t (:foreground "violet")))))

(leaf systemd
  :doc "Major mode for editing systemd units"
  :url "https://github.com/holomorph/systemd-mode"
  :ensure t
  :mode
  ("\\.service\\'" "\\.timer\\'" "\\.target\\'" "\\.mount\\'"
   "\\.automount\\'" "\\.slice\\'" "\\.socket\\'" "\\.path\\'"
   "\\.netdev\\'" "\\.network\\'" "\\.link\\'"))

(leaf protobuf-mode
  :doc "Major mode for protobuf file editing"
  :url "https://github.com/protocolbuffers/protobuf/blob/master/editors/protobuf-mode.el"
  :ensure t
  :mode "\\.proto\\'")

(leaf dockerfile-mode
  :doc "Mode for handling Dockerfiles"
  :url "https://github.com/spotify/dockerfile-mode"
  :ensure t
  :mode "\\Dockerfile\\'")

(leaf terraform-mode
  :doc "Major mode of Terraform configuration file"
  :url "https://github.com/emacsorphanage/terraform-mode"
  :ensure t
  :mode "\\.tf\\'")


;; -----------------------------------------------------------------------------------------
;;
;; Widgets
;;
;; -----------------------------------------------------------------------------------------

(leaf projectile
  :doc "Project navigation and management library"
  :url "https://github.com/bbatsov/projectile"
  :ensure t
  :global-minor-mode projectile-mode)

(leaf imenu-list
  :doc "Show the current buffer's imenu entries in a seperate buffer"
  :url "https://github.com/Ladicle/imenu-list"
  :el-get "Ladicle/imenu-list"
  :bind ("<f10>" . imenu-list-smart-toggle)
  :hook (imenu-list-major-mode-hook . neo-hide-nano-header)
  :custom
  (imenu-list-auto-resize . t)
  (imenu-list-focus-after-activation . t)
  (imenu-list-entry-prefix   . "•")
  (imenu-list-subtree-prefix . "•")
  :custom-face
  (imenu-list-entry-face-1          . '((t (:foreground "white"))))
  (imenu-list-entry-subalist-face-0 . '((nil (:weight normal))))
  (imenu-list-entry-subalist-face-1 . '((nil (:weight normal))))
  (imenu-list-entry-subalist-face-2 . '((nil (:weight normal))))
  (imenu-list-entry-subalist-face-3 . '((nil (:weight normal)))))

(leaf vterm
  :doc "Fully-featured terminal emulator"
  :req "emacs-25.1"
  :tag "terminals" "emacs>=25.1"
  :url "https://github.com/akermu/emacs-libvterm"
  :added "2022-10-30"
  :emacs>= 25.1
  :ensure t)

;; 複数文字列を置換
(leaf anzu
  :doc "Displays current match and total matches information"
  :url "https://github.com/emacsorphanage/anzu"
  :ensure t
  :bind ("M-r" . anzu-query-replace-regexp))

;; -----------------------------------------------------------------------------------------
;;
;; Accessibility
;;
;; -----------------------------------------------------------------------------------------

(leaf which-key
  :doc "Displays available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :global-minor-mode which-key-mode)

;; fill-column
(leaf visual-fill-column
  :doc "Centering & Wrap text visually"
  :url "https://codeberg.org/joostkremers/visual-fill-column"
  :ensure t
  :hook ((markdown-mode-hook org-mode-hook) . visual-fill-column-mode)
  :custom
  (visual-fill-column-width . 100)
  (visual-fill-column-center-text . t))

(leaf display-fill-column-indicator-mode
  :doc "Indicate maximum colum"
  :url "https://www.emacswiki.org/emacs/FillColumnIndicator"
  :hook ((markdown-mode-hook git-commit-mode-hook) . display-fill-column-indicator-mode))

(leaf display-line-numbers
  :doc "Display line number"
  :url "https://www.emacswiki.org/emacs/LineNumbers"
  :hook (terraform-mode-hook . display-line-numbers-mode))

(leaf rainbow-mode
  :doc "Color letter that indicate the color"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :ensure t
  :hook (emacs-lisp-mode-hook . rainbow-mode))

(leaf rainbow-delimiters
  :doc "Display brackets in rainbow"
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(provide 'init)
;;; init.el ends here
