;;; .emacs --- Summary
;;; My custom Emacs configuration.

;;; Commentary:
;;; Nothing to see here!

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Stop polluting this file with the "custom" stuff; shove those
;; variables elsewhere. This MUST come before other settings, so that
;; whatever shit gets shoved in the custom file doesn't overwrite my
;; actual settings... this has happened before.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :init
  (setq use-package-always-ensure t))

;; Appearance
;; Interface
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode) ; Show line numbers on the side
(column-number-mode) ; Show column number in mode line
;; Options for new frames
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 82))
;; Try to focus the new frame on creation
(add-to-list 'after-make-frame-functions 'select-frame-set-input-focus)
;; Highlight (some) whitespace
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)
;; Theme
(use-package moe-theme
  :init
  (add-hook 'before-make-frame-hook 'moe-dark)
  :config
  (moe-theme-set-color 'green)
  (moe-dark))


;; Ido mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset tab-width)
(setq-default backward-delete-char-untabify-method 'hungry) ; Delete entire tabs
(use-package hungry-delete
  :config
  (setq-default hungry-delete-chars-to-skip " \t")
  (global-hungry-delete-mode))

;; Text
(setq sentence-end-double-space nil) ; Don't use two spaces after period
(setq mode-require-final-newline t)
(setq-default fill-column 79)

;; Globally useful packages
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3))

;; Org mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done 'time)               ; Log a time stamp for DONE
(setq org-agenda-files (list
                        "~/org/personal.org"
                        "~/org/facets.org"
                        "~/org/professional.org"))
;; Emacs Lisp setup
(defun my-emacs-lisp-setup ()
  "Custom setup for Emacs Lisp."
  (local-set-key (kbd "C-c C-e") 'eval-buffer))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-setup)

;; C/C++ setup
(defun my-c-setup ()
  "Custom setup for 'c-mode' and 'c++-mode'."
  (add-hook 'before-save-hook #'clang-format-buffer nil t))
(add-hook 'c-mode-hook #'my-c-setup)
(add-hook 'c++-mode-hook #'my-c-setup)
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(use-package irony
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (use-package company-irony
    :init
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  (use-package flycheck-irony
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))
(use-package clang-format)

;; Go setup
(use-package go-mode
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (setq gofmt-command "goimports")
  (use-package company-go))

;; Markdown setup
(use-package markdown-mode
  :config
  (setq-default markdown-command "cmark"))

;; Rust setup
(defun my-rust-setup ()
  "Custom setup for 'rust-mode'."
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column 100))

(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'my-rust-setup)
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
  (use-package racer
    :init
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)))

;; TeX setup
(use-package tex
  :ensure auctex)

;; Git setup
(use-package magit
  :bind ("C-x g" . magit-status))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))

;; Misc. file formats
(use-package yaml-mode)

;; Email setup (gnus)
(require 'gnus)
(require 'gnus-group)
(require 'smtpmail)
;; Keyboard shortcut to open gnus
(global-set-key (kbd "C-c g") 'gnus)
;; Account setup
(setq user-mail-address "ianprime0509@gmail.com"
      user-full-name "Ian Johnson")
(setq gnus-select-method
      '(nnimap "personal"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)
               (nnimap-authinfo-file "~/.authinfo.gpg")))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "professional"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo.gpg")))
(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq-default gnus-permanently-visible-groups ".*")
;; Parameters
(setq nnmail-expiry-wait 'immediate
      nnmail-expiry-target 'delete)
(setq gnus-posting-styles
      '(((header "to" "ianprime0509@gmail.com")
         (address "ianprime0509@gmail.com"))
        ((header "cc" "ianprime0509@gmail.com")
         (address "ianprime0509@gmail.com"))
        ((header "to" "iantimothyjohnson@gmail.com")
         (address "iantimothyjohnson@gmail.com"))
        ((header "cc" "iantimothyjohnson@gmail.com")
         (address "iantimothyjohnson@gmail.com"))))
;; Appearance
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")
(setq gnus-summary-line-format
              "%U%R%z%I%(%[%d: %-23,23f%]%) %s\n")


;; Misc configuration
;; Move save files somewhere else
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(provide '.emacs)
;;; .emacs ends here
