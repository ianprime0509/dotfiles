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
(menu-bar-mode -1)
(setq initial-scratch-message "")
(global-linum-mode) ; Show line numbers on the side
(column-number-mode) ; Show column number in mode line
;; Options for new frames
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 85))
;; Try to focus the new frame on creation
(add-to-list 'after-make-frame-functions #'select-frame-set-input-focus)
;; Highlight (some) whitespace
(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail trailing))
  (global-whitespace-mode t))
;; Theme
(use-package moe-theme
  :functions (moe-theme-set-color moe-dark)
  :hook ((before-make-frame . moe-dark))
  :config
  (moe-theme-set-color 'green)
  (moe-dark))

;; Window navigation
(global-set-key (kbd "M-o") #'other-window)

;; Ido mode
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

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

;; Spell-checking
(use-package ispell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq ispell-dictionary "en"))

;; Globally useful packages
(use-package flycheck
  :hook ((after-init . global-flycheck-mode))
  :config
  (use-package flycheck-irony
    :after irony-mode
    :config
    (flycheck-irony-setup))
  (use-package flycheck-rust
    :after rust-mode
    :config
    (flycheck-rust-setup)))

(use-package company
  :demand
  :bind
  ("C-<tab>" . company-complete)
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (global-company-mode)
  (use-package company-go
    :after go-mode)
  (use-package company-irony
    :after irony-mode
    :init
    (add-to-list 'company-backends 'company-irony)))

;; Org mode
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq org-log-done 'time)               ; Log a time stamp for DONE
  (setq org-agenda-files (list
                          "~/org/personal.org"
                          "~/org/facets.org"
                          "~/org/professional.org")))

;; General Lisp setup
(use-package paredit
  :hook
  ((lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    ielm-mode
    eval-expression-minibuffer-setup
    scheme-mode
    geiser-repl-mode) . enable-paredit-mode))
(use-package highlight-parentheses
  :hook
  ((lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    ielm-mode
    eval-expression-minibuffer-setup
    scheme-mode
    geiser-repl-mode) . highlight-parentheses-mode))

;; Emacs Lisp setup
(defun my-emacs-lisp-setup ()
  "Custom setup for Emacs Lisp."
  (local-set-key (kbd "C-c C-b") 'eval-buffer))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-setup)

;; Scheme setup
(use-package geiser
  :defines geiser-active-implementations
  :config
  (setq geiser-active-implementations '(guile)))

;; C/C++ setup
(defun my-c-setup ()
  "Custom setup for 'c-mode' and 'c++-mode'."
  (add-hook 'before-save-hook #'clang-format-buffer nil t))
(add-hook 'c-mode-hook #'my-c-setup)
(add-hook 'c++-mode-hook #'my-c-setup)
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(use-package irony
  :hook ((c-mode c++-mode) . irony-mode)
  :config
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
(use-package clang-format)

;; Go setup
(use-package go-mode
  :functions gofmt-before-save
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))

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
  (use-package racer
    :init
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)))

;; TeX setup
(use-package tex
  :ensure auctex)

;; Git setup
(use-package magit
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :bind ("C-x g" . magit-status))
(setq vc-follow-symlinks t)             ; disable annoying question

;; Misc. file formats
(use-package yaml-mode)

;; Email setup (gnus)
(setq user-mail-address "ianprime0509@gmail.com"
      user-full-name "Ian Johnson")
(use-package sendmail
  :config
  (setq send-mail-function 'sendmail-send-it)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/bin/msmtp"))
(use-package gnus
  :bind ("C-c g" . gnus)
  :defines (nnmail-expiry-wait
            nnmail-expiry-target
            gnus-posting-styles
            gnus-group-line-format)
  :functions gnus-topic-mode
  :config
  ;; Account setup
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
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "uva"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port "imaps")
                        (nnimap-stream ssl)
                        (nnimap-authinfo-file "~/.authinfo.gpg")))
  (setq-default gnus-permanently-visible-groups ".*")
  ;; Parameters
  (setq nnmail-expiry-wait 'immediate
        nnmail-expiry-target 'delete)
  (setq gnus-auto-expirable-newsgroups ".*")
  ;; Set "posting styles" appropriately for each email
  ;; Behold my first nontrivial elisp code and marvel at how absolute shit it
  ;; looks :)
  (setq gnus-posting-styles
        (let ((my-emails '("ianprime0509@gmail.com"
                           "iantimothyjohnson@gmail.com"
                           "ij6fd@virginia.edu")))
          (apply 'append
                 (mapcar (lambda (addr)
                           (mapcar (lambda (str)
                                     `((header ,str ,addr) (address ,addr)))
                                   '("to" "cc")))
                         my-emails))))
  ;; Appearance
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")
  (setq gnus-summary-line-format "%U%R%z%I%(%[%d: %-23,23f%]%) %s\n"))

;; BBDB (address book)
(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq bbdb-add-mails 'query)
  (setq bbdb-update-records-p 'query)
  (setq bbdb-mua-pop-up nil)
  (add-hook 'bbdb-after-change-hook #'bbdb-save))


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
