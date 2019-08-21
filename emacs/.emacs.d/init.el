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

;; Custom keys
(global-set-key (kbd "M-o") #'other-window)

;; UI setup
(use-package material-theme
    :straight t
    :config (load-theme 'material t))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; General programming stuff
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package company
  :straight t
  :config (global-company-mode))

;; Lisp
(use-package paredit
  :straight t
  :hook ((eval-expression-minibuffer-setup
	  emacs-lisp-mode
	  lisp-mode
	  slime-repl-mode) . enable-paredit-mode))

(defun override-slime-repl-bindings-with-paredit ()
  "Override SLIME's REPL DEL behavior.
Taken from https://www.emacswiki.org/emacs/ParEdit."
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(use-package slime
  :straight t
  :hook lisp-mode
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  (add-hook 'slime-repl-mode-hook #'override-slime-repl-bindings-with-paredit))
