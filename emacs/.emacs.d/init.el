;;; init.el --- custom init commands
;;; Commentary:
;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Stop putting custom stuff in my init file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; If use-package is not installed, install it.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; UI configuration
;; Color theme
(use-package nord-theme
  :ensure t
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (with-selected-frame frame
		    (load-theme 'nord t))))
    (load-theme 'nord t)))

;; UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;; Keys config
;; Evil mode (Vim bindings)
;; Use ESC to quit anything; thanks to https://stackoverflow.com/a/10166400.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(use-package evil
  :ensure t
  :config
  ;; Use ESC to quit most things.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  ;; Disable some unwanted keys.
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-insert-state-map (kbd "M-.") nil)
  ;; Enable Evil mode.
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-collection-init))

;;; Version control
;; Git setup
(setq vc-follow-symlinks t)		; Follow symlinks to Git files

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :ensure t
  :after magit)

;;; Editing tools
;; Company (autocomplete)
(use-package company
  :ensure t
  :demand
  :bind ("C-<tab>" . company-complete)
  :config
  (global-company-mode))

(use-package company-lsp
  :after company
  :ensure t
  :config
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-cache-candidates t))

;; Lisp-like language tools
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package evil-paredit
  :after paredit
  :ensure t
  :hook (emacs-lisp-mode . evil-paredit-mode))

;;; Language settings
;; LSP (language server protocol)
(use-package lsp-mode
  :ensure t
  :bind (("C-S-f" . lsp-format-buffer)
	 ("C-?" . lsp-describe-thing-at-point))
  :config
  (setq lsp-inhibit-message t))

;; Debugging support
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; Flycheck (syntax checking)
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; Java
(use-package lsp-java
  :after 'lsp-mode
  :ensure t
  :hook (java-mode . lsp-java-enable)
  :bind ("C-S-o" . lsp-java-organize-imports)
  :config
  (setq lsp-java-format-settings-url "https://github.com/google/styleguide/blob/gh-pages/eclipse-java-google-style.xml"))

(use-package dap-java
  :after 'lsp-java)

(provide 'init)
;;; init.el ends here
