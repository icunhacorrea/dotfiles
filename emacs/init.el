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
(set-face-attribute 'default nil :family "Hack" :height 130)

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
  (setq gcmh-high-cons-threshold (* 256 1024 1024))
  (setq gcmh-idle-delay 5)
  :config
  (gcmh-mode 1))

(setq read-process-output-max (* 4 1024 1024))

;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   (setq catppuccin-flavor 'frappe))
;; (load-theme 'catppuccin :no-confirm)

(use-package subatomic-theme
  :ensure t)
(load-theme 'subatomic :no-confirm)

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
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t))

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
                               orderless-flex))
  (completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

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
         ("C-c p k" . project-kill-buffers)
	 ("C-c p r" . consult-recent-file)))

;; (use-package diff-hl
;;   :ensure t
;;   :custom
;;   (diff-hl-draw-borders nil)
;;   :config
;;   (global-diff-hl-mode))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-draw-borders nil)
  :config
  (set-face-attribute 'diff-hl-insert nil :background "#388E3C")
  (set-face-attribute 'diff-hl-delete nil :background "#D32F2F")
  (set-face-attribute 'diff-hl-change nil :background "#FBC02D")
  (global-diff-hl-mode))

(use-package rg
  :ensure t)

(use-package flx
  :ensure flx)

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

(use-package expand-region
  :ensure t
  :bind ("C-0" . er/expand-region))

(use-package dockerfile-mode
  :ensure t)

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode t))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; LSP

(use-package eglot
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider: :hoverProvider))
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.5)
  (add-to-list 'eglot-server-programs
	       `(python-mode python-ts-mode . ("pyright-langserver" "--stdio"))
	       `(go-mode . ("gopls"))))

(use-package eglot-booster
  :after eglot
  :config
  (setq eglot-booster-io-only t)
  (eglot-booster-mode))

;; Personal defs
(global-set-key (kbd "C-c k") 'kill-whole-line)

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
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'vterm-mode-hook #'my/vterm-disable-line-numbers)
(add-hook 'eglot-managed-mode-hook 'flymake-ruff-load)
(add-hook 'magit-pre-refresh-hook
          'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook
          'diff-hl-magit-post-refresh)

(global-set-key (kbd "C-v") 'scroll-half-page-down)
(global-set-key (kbd "M-v") 'scroll-half-page-up)
(global-set-key (kbd "C-o") 'my-open-line-below)
(global-set-key (kbd "C-S-o") 'my-open-line-above)
(global-set-key (kbd "C-c m t") 'run-make-tests)
(global-set-key (kbd "C-c m f") 'run-make-format)
(global-set-key (kbd "C-c l") 'select-current-line)
(global-set-key (kbd "C-c v") #'my/pop-to-vterm)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("24fba8d15d029ca2ed94dc4722459e9b64d679d7ae14b77b61412e2c85b3b641"
     "806dd05c68b646416d686fc45d1ed7e6a173511e2548cd62150473fe5149f66c"
     default))
 '(package-selected-packages
   '(breadcrumb catppuccin-theme company consult corfu diff-hl
		dockerfile-mode eglot-booster evil-nerd-commenter
		exec-path-from-shell expand-region fancy-compilation
		flx flymake-ruff gcmh go-mode magit marginalia
		mood-line move-text oceanic-theme orderless rg s
		spacegray-theme subatomic-theme vertico vterm
		yaml-mode yasnippet zenburn-theme))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url
		    "https://github.com/jdtsmith/eglot-booster"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
