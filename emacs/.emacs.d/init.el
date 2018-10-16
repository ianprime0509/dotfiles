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
  (load-theme 'nord t))

;; UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Evil mode (Vim bindings)
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; Company (autocomplete)
(use-package company
  :ensure t
  :config
  (bind-key (kbd "TAB") #'company-complete)
  (global-company-mode))

(use-package company-lsp
  :after company
  :ensure t
  :config
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-cache-candidates t))

;; Language settings
;; LSP (language server protocol)
(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t)

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
  :ensure t
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :config
  (add-hook 'java-mode-hook 'lsp-java-enable)
  (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'java-mode-hook 'lsp-ui-mode))

(use-package dap-java
  :after 'lsp-java)

(provide 'init)
;;; init.el ends here
