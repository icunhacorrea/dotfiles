;; icorrea emacs Configuration

;; Env
(setenv "LSP_USE_PLISTS" "true")

;; Gui

(global-display-line-numbers-mode +1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode 1)
(set-face-attribute 'default nil :family "Menlo" :height 140)

;; General configs
(setq inhibit-startup-message t
      frame-inhibit-implied-resize t
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      lsp-use-plists t
      text-scale-mode-step 1.05)

;; Package
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package gcmh
  :ensure t
  :straight t
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode))))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :straight t
  :config (which-key-mode))

(use-package evil-nerd-commenter
  :straight t
  :bind (("M-/" . evilnc-comment-or-uncomment-lines)))

(use-package go-mode
  :straight t)

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 1)
  (global-company-mode t))

(use-package company-go
  :straight (:build t)
  :after (company go))

;; underwater
;; oceanic
;; subatomic

;; (use-package catppuccin-theme
;;   :straight t
;;   :config
;;   (setq catppuccin-flavor 'mocha))
;; (load-theme 'catppuccin t)

(use-package spacegray-theme
  :straight t)
(load-theme 'spacegray t)

(use-package flx
  :straight t)

(use-package magit
  :straight t)

(use-package diff-hl
  :straight t
  :custom
  (diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook
            'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook
            'diff-hl-magit-post-refresh))

(use-package diff-hl-flydiff
  :config
  (diff-hl-flydiff-mode))

(use-package swiper
  :straight t
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
		  enable-recursive-minibuffers t
		  ivy-re-builders-alist
		  '((t . ivy--regex-fuzzy))))
  (global-set-key "\C-f" 'swiper))

(use-package counsel
  :straight t)

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-file-suffixes '(".log" ".tmp" ".bak"))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (add-hook 'projectile-mode-hook 'auto-revert-mode))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package rg
  :straight t)

(use-package breadcrumb
  :straight t
  :config
  (breadcrumb-mode t))

;; Terminal
(use-package vterm
  :straight t
  :config
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package fancy-compilation
  :straight t
  :commands (fancy-compilation-mode)
  :config
  (setq fancy-compilation-override-colors nil))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;; LSP

;; (use-package eglot
;;   :ensure t
;;   :custom
;;   (eglot-autoshutdown t)
;;   (eglot-events-buffe-size 0)
;;   (eglot-extend-to-xref nil)
;;   (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
;; 					    :hoverProvider))
;;   (setq eglot-send-changes-idle-time 0.1)
;;   (fset #'jsonrpc--log-event #'ignore)
;;   :config 'eglot-server-programs
;; 		   `(python-mode python-ts-mode . ("pyright-langserver" "--stdio"))
;; 	           `(go-mode . ("gopls")))


(use-package eglot
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :hoverProvider))
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.1))

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
		   `(python-mode python-ts-mode . ("pyright-langserver" "--stdio"))
	           `(go-mode . ("gopls"))))


(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package dockerfile-mode
  :straight t)

(defun my/vterm-disable-line-numbers ()
  (display-line-numbers-mode -1))

(defun my-open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my-open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun run-make-tests ()
  (interactive)
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (compile "make test")))

(defun run-make-format ()
  (interactive)
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (compile "make format")))

(defun delete-current-line ()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun select-current-line ()
  (interactive)
  (move-end-of-line nil)
  (push-mark (line-beginning-position) nil t)
  (activate-mark))

(fset #'jsonrpc--log-event #'ignore)

;; Hooks

(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'vterm-mode-hook #'my/vterm-disable-line-numbers)

(global-set-key (kbd "C-o") 'my-open-line-below)
(global-set-key (kbd "C-S-o") 'my-open-line-above)
(global-set-key (kbd "C-c m t") 'run-make-tests)
(global-set-key (kbd "C-c m f") 'run-make-format)
(global-set-key (kbd "C-c k") 'delete-current-line)
(global-set-key (kbd "C-c l") 'select-current-line)
