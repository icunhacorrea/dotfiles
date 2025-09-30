;; icorrea emacs configuration

(global-display-line-numbers-mode +1)
(global-visual-line-mode -1)
(tool-bar-mode -1)
(electric-pair-mode 1)
(which-key-mode 1)
(scroll-bar-mode -1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 130)

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
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gcmh
  :ensure t
  :init
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (setq gcmh-idle-delay 5)
  :config
  (gcmh-mode 1))

(setq read-process-output-max (* 4 1024 1024))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'frappe))
(load-theme 'catppuccin :no-confirm)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package evil-nerd-commenter
  :ensure t
  :bind (("M-/" . evilnc-comment-or-uncomment-lines)))

(use-package go-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t))

(use-package cape
  :ensure t
  :init
  (setq dabbrev-min-length 1)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-initialism
                               orderless-flex))
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package diff-hl
  :ensure t
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

(use-package rg
  :ensure t)

(use-package consult
  :ensure t
  :config
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c p f" . project-find-file)
	 ("C-c p 4 f" . my/project-find-file-other-window-always-split)
         ("C-c p p" . project-switch-project)
         ("C-c p b" . consult-project-buffer)
	 ("C-c p 4 b" . my/consult-project-buffer-other-window)
         ("C-c p s" . consult-ripgrep)
	 ("C-c p r" . consult-recent-file)))

;; (use-package consult
;;   :ensure t
;;   :config
;;   (define-prefix-command 'my/project-prefix)
;;   (global-set-key (kbd "s-p") 'my/project-prefix)
;;   :bind (:map my/project-prefix
;;             ("f" . project-find-file)
;;             ("p" . project-switch-project)
;;             ("b" . consult-project-buffer)
;;             ("s" . consult-ripgrep)
;;             ("r" . consult-recent-file)
;;             ("k" . project-kill-buffers)))

(use-package vterm
  :ensure t
  :config
  (setq vterm-term-environment-variable "xterm-256color"))

(use-package fancy-compilation
  :ensure t
  :commands (fancy-compilation-mode)
  :config
  (setq fancy-compilation-override-colors nil))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

(use-package expand-region
  :ensure t
  :bind ("C-0" . er/expand-region))

(use-package dockerfile-mode
  :ensure t)

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;; LSP

(use-package eglot
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (go-mode . eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :hoverProvider))
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.1)
  (add-to-list 'eglot-server-programs
	       `(python-mode python-ts-mode . ("basedpyright" "--stdio"))
	       `(go-mode . ("gopls"))))

(use-package eldoc-box
  :ensure t
  :custom
  (eldoc-box-hover-at-point-mode +1)
  (setq eldoc-box-delay 0.1)
  (setq eldoc-box-max-height 10)
  :hook
  (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode t))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((python-mode . lsp-deferred)
;; 	 (python-ts-mode . lsp-deferred)
;; 	 (go-mode . lsp-deferred))
;;   :commands (lsp lsp-deferred))

;; (use-package lsp-pyright
;;   :ensure t
;;   :custom
;;   (lsp-pyright-langserver-command "basedpyright")
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp-deferred))))

;; (setq lsp-auto-guess-root t)

;; Personal defs

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
  (let ((compile-command "make test")
        (compilation-read-command nil))
    (project-compile)))

(defun run-make-format ()
  (interactive)
  (let ((compile-command "make format")
        (compilation-read-command nil))
    (project-compile)))

(defun delete-current-line ()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun select-current-line ()
  (interactive)
  (move-end-of-line nil)
  (push-mark (line-beginning-position) nil t)
  (activate-mark))

(defun scroll-half-page-down ()
  (interactive)
  (scroll-up-command (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  (interactive)
  (scroll-down-command (/ (window-body-height) 2)))

(defun my/pop-to-vterm ()
  (interactive)
  (let ((buf (get-buffer "*vterm*")))
    (if buf
        (pop-to-buffer buf)
      (vterm))))

(defun my/project-find-file-other-window-always-split ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (call-interactively #'project-find-file))

(defun my/consult-project-buffer-other-window ()
  (interactive)
  (unless (> (length (window-list)) 1)
    (split-window-right))
  (other-window 1)
  (call-interactively #'consult-project-buffer))

;; Hooks
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'vterm-mode-hook #'my/vterm-disable-line-numbers)

(global-set-key (kbd "C-v") 'scroll-half-page-down)
(global-set-key (kbd "M-v") 'scroll-half-page-up)
(global-set-key (kbd "C-o") 'my-open-line-below)
(global-set-key (kbd "C-S-o") 'my-open-line-above)
(global-set-key (kbd "C-c m t") 'run-make-tests)
(global-set-key (kbd "C-c m f") 'run-make-format)
(global-set-key (kbd "C-c k") 'delete-current-line)
(global-set-key (kbd "C-c l") 'select-current-line)
(global-set-key (kbd "C-c v") #'my/pop-to-vterm)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
