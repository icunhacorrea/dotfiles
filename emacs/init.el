;; icorrea basic emacs config

;; modes
(ido-mode 1)

;; Gui

(global-display-line-numbers-mode +1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode 1)
(set-face-attribute 'default nil :family "Menlo" :height 140)
(setq-default message-log-max nil)
(setq inhibit-startup-message t
	  frame-inhibit-implied-resize t
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      ido-enable-flex-matching t
      ido-everywhere t)

(setq scroll-step 5)
(setq-default tab-width 4)
(global-hl-line-mode)

;; Melpa

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gcmh
  :ensure t
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode))))

;; Web eglot

(use-package eglot
  :custom
  (fset #'jsonrpc--log-event #'ignore)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))

  :config
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

;; exclude modes from eglot
(defun maybe-start-eglot ()
  "Exlude some mode from eglot."
  (let ((disabled-modes '(emacs-lisp-mode dockerfile-ts-mode)))
    (unless (apply 'derived-mode-p disabled-modes)
      (eglot-ensure))))

(add-hook 'prog-mode-hook #'maybe-start-eglot)

;; My eglot

;; (use-package eglot
;;   :ensure t
;;   :hook ((python-mode . eglot-ensure))
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("pyright-langserver" "--stdio")))
;;   (setq eglot-autoshutdown t)
;;   (setq eglot-confirm-server-initiated-edits nil)
;;   (setq eglot-events-buffer-size 0)
;;   :custom
;;   (fset #'jsonrpc--log-event #'ignore))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 1)
  (global-company-mode t))

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure t
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
           enable-recursive-minibuffers t)
    (global-set-key "\C-f" 'swiper)))

(use-package counsel
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-file-suffixes '(".log" ".tmp" ".bak"))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (add-hook 'projectile-mode-hook 'auto-revert-mode))

(use-package magit
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t
  :bind (("M-/" . evilnc-comment-or-uncomment-lines)))

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

(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(solarized-theme diff-hl evil-nerd-commenter magit projectile counsel swiper ivy flx company which-key gcmh)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Custom commands

(defun my-open-line-below ()
  "Insert a new line below the current one and move the cursor to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my-open-line-above ()
  "Insert a new line above the current one and move the cursor to it."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-run-make-tests ()
  (interactive)
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (compile "make test")))

(defun my/increase-font-size ()
  "Increase font size by 10."
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ (face-attribute 'default :height) 10)))

(defun my/decrease-font-size ()
  "Decrease font size by 10."
  (interactive)
  (set-face-attribute 'default nil
                      :height (- (face-attribute 'default :height) 10)))


;; Custom shortcuts
(global-set-key (kbd "C-o") 'my-open-line-below)
(global-set-key (kbd "C-S-o") 'my-open-line-above)
(global-set-key (kbd "C-c m t") 'my-run-make-tests)
(global-set-key (kbd "C-=") 'my/increase-font-size)
(global-set-key (kbd "C--") 'my/decrease-font-size)
