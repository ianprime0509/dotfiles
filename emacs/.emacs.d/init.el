;; straight.el bootstrap
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

(straight-use-package 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Custom keys
(global-set-key (kbd "M-o") #'other-window)

;;; UI setup
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package base16-theme
  :straight t
  :config
  (load-theme 'base16-theme-manager t))

(add-to-list 'default-frame-alist '(alpha . (90 . 60)))

(menu-bar-mode -1)
(defun turn-off-scroll-bar (frame)
  "Turn off scroll bars for FRAME."
  (with-selected-frame frame
    (toggle-scroll-bar -1)))
(add-to-list 'after-make-frame-functions #'turn-off-scroll-bar)
(tool-bar-mode -1)

;;; Editor behavior
(setq backup-directory-alist '(("." . "~/.saves"))
      backup-by-copying t   ; don't clobber symlinks
      version-control t     ; use versioned backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq default-input-method 'japanese)

;;; Indentation
(setq indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;;; General programming stuff
(setq vc-follow-symlinks t)

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :straight t
  :config (global-company-mode))

(use-package rainbow-mode
  :straight t)

;;; Git
(use-package magit
  :straight t
  :bind ("C-x g" . magit))

;;; Lisp
(use-package smartparens
  :straight t
  :hook ((eval-expression-minibuffer-setup
          emacs-lisp-mode
          lisp-mode
          slime-repl-mode) . smartparens-strict-mode)
  :init
  (add-hook 'smartparens-mode-hook #'sp-use-paredit-bindings)
  :config
  (require 'smartparens-config))

(use-package slime
  :straight t
  :hook (lisp-mode . slime-mode)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(defun my-emacs-lisp-mode-config ()
  "Custom config for Emacs Lisp mode."
  (local-set-key (kbd "C-c b") #'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-config)

;;; JSON
(defun my-json-mode-config ()
  "Custom config for JSON mode."
  (setq tab-width 2)
  (setq js-indent-level 2))

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook #'my-json-mode-config))
