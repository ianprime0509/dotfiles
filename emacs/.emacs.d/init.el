;;; init --- my custom Emacs config

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:
(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; I don't use Custom stuff so I want it to take lowest priority (load
;; it first)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Path setup
(defvar my-emacs-path '("~/bin"
                        "~/.local/bin"
                        "/usr/local/bin"
                        "/usr/bin")
  "Entries in the PATH environment variable used by Emacs.")

(let ((expanded-emacs-path (mapcar #'expand-file-name my-emacs-path)))
  (setenv "PATH" (string-join expanded-emacs-path ":"))
  (setq exec-path (append expanded-emacs-path (list exec-directory))))

;;; Custom keys
(global-set-key (kbd "M-o") #'other-window)

;;; UI setup
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package base16-theme
  :ensure t
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

(use-package whitespace
  :config
  (setq whitespace-style '(face indentation::space trailing))
  (global-whitespace-mode))

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

;;; Web browser
(setq browse-url-browser-function #'eww-browse-url)

;;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;;; General programming stuff
(setq vc-follow-symlinks t)

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package company
  :ensure t
  :demand
  :bind ("C-<tab>" . company-complete)
  :config (global-company-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((java-mode python-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable t))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package dap-mode
  :ensure t
  :after lsp-mode)

;;; Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit))

;;; C
(use-package google-c-style
  :ensure t
  :hook (c-mode-common . google-set-c-style))

;;; Lisp
(use-package smartparens
  :ensure t
  :hook ((eval-expression-minibuffer-setup
          emacs-lisp-mode
          lisp-mode
          slime-repl-mode) . smartparens-strict-mode)
  :init
  (add-hook 'smartparens-mode-hook #'sp-use-paredit-bindings)
  :config
  (require 'smartparens-config))

(use-package slime
  :ensure t
  :hook (lisp-mode . slime-mode)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  (setq common-lisp-hyperspec-root "file:///home/ian/HyperSpec/"))

(defun my-emacs-lisp-mode-config ()
  "Custom config for Emacs Lisp mode."
  (local-set-key (kbd "C-c b") #'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-config)

;;; Java
(defun my-java-mode-config ()
  "Custom config for Java mode."
  (dap-mode t)
  (dap-ui-mode t)
  (local-set-key (kbd "C-S-f") #'google-java-format-buffer))

(add-hook 'java-mode-hook #'my-java-mode-config)

(use-package google-java-format
  :commands (google-java-format-buffer google-java-format-region)
  :config
  (setq google-java-format-executable "~/bin/google-java-format"))

(use-package lsp-java
  :ensure t
  :after lsp-mode)

(use-package dap-java
  :after lsp-java)

;;; JSON
(defvar js-indent-level)

(defun my-json-mode-config ()
  "Custom config for JSON mode."
  (setq tab-width 2)
  (setq js-indent-level 2))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook #'my-json-mode-config))

;;; Python
(defun my-python-mode-config ()
  "Custom config for Python mode."
  (setq tab-width 4)
  (local-set-key (kbd "C-S-f") #'lsp-format-buffer))

(add-hook 'python-mode-hook #'my-python-mode-config)

(provide 'init)
;;; init.el ends here
