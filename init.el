;;; init.el
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

;; You can also configure builtin package via leaf!
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((user-full-name . "Naoya Yamashita")
           (user-mail-address . "conao3@gmail.com")
           (user-login-name . "conao3")
           (truncate-lines . t)
           (menu-bar-mode . t)
           (tool-bar-mode . nil)
           (scroll-bar-mode . nil)
           (indent-tabs-mode . nil)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

;; Nest package configurations
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

;; my packages

(leaf dmacro
  :ensure t
  :custom `((dmacro-key . ,(kbd "C-S-e")))
  :global-minor-mode global-dmacro-mode)

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
;; (leaf *frame-setting
;;   :init
;;   (set-frame-parameter nil 'alpha 97)              ;; 透明度設定
;;   (set-frame-parameter nil 'fullscreen 'maximized) ;; 起動時に最大化
;;   :preface
;;   (defun set-alpha (alpha-num)
;;     (interactive "nAlpha: ")
;;     (set-frame-parameter nil 'alpha (cons alpha-num '(97)))))




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
   ("M-p" . (lambda () (interactive) (forward-line -10)))
   ("M-n" . (lambda () (interactive) (forward-line 10)))
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
