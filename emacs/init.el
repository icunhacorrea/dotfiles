;; icorrea emacs Configuration

;; Ido mode
(ido-mode 1)

;; Env
(setenv "LSP_USE_PLISTS" "true")


;; gui

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
      ido-enable-flex-matching t
      ido-everywhere t
      tab-width 4
      python-indent-offset 4
      lsp-use-plists t)

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

(use-package underwater-theme
  :straight t)
(load-theme 'underwater t)

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

;; Terminal
(use-package vterm
  :straight t)

;; LSP

;; (use-package eglot
;;   :config
;;   (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :hoverProvider))
;;   (setq eglot-send-changes-idle-time 0.1))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;; 		   `(python-mode python-ts-mode . ("pyright-langserver" "--stdio"))
;; 	           `(go-mode . ("gopls"))))


;; (use-package eglot-booster
;;   :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config (eglot-booster-mode))

;; Hooks

;; (add-hook 'go-mode-hook #'eglot-ensure)
;; (add-hook 'python-mode-hook #'eglot-ensure)
;; (add-hook 'python-ts-mode-hook #'eglot-ensure)

(use-package lsp-mode
  :straight t
  :hook ((python-mode . lsp-deferred)
	 (python-ts-mode . lsp-deferred)
	 (go-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

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

;; Hooks

(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'vterm-mode-hook #'my/vterm-disable-line-numbers)

(global-set-key (kbd "C-o") 'my-open-line-below)
(global-set-key (kbd "C-S-o") 'my-open-line-above)
(global-set-key (kbd "C-c m t") 'run-make-tests)
(global-set-key (kbd "C-c m f") 'run-make-format)
(global-set-key (kbd "C-c k") 'delete-current-line)
(global-set-key (kbd "C-c l") 'select-current-line)
