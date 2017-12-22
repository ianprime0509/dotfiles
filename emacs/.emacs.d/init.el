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
  :config
  (setq use-package-always-ensure t))

;; Appearance
;; Interface
(set-frame-font "Monospace 11" nil t)
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
(use-package color-theme-solarized
  :config
  (load-theme 'solarized t)
  (setq-default frame-background-mode 'dark))

;; Misc. keybindings
(global-unset-key (kbd "<insert>"))     ; I hate overwrite mode
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-i") #'imenu)

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

;; Dired
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh"))

;; Shell
(use-package shell
  :ensure nil
  :commands shell
  :bind ("C-c s" . shell)
  :config
  ;; From lisp/comint.el in Emacs git as of commit
  ;; 506270f9c80bf9bd7dad35a2f0aa6f477da6490b, modified to better handle French
  ;; spaces between punctuation
  (setq comint-password-prompt-regexp
        (concat
         "\\(^ *\\|"
         (regexp-opt
          '("Enter" "enter" "Enter same" "enter same" "Enter the" "enter the"
            "Old" "old" "New" "new" "'s" "login"
            "Kerberos" "CVS" "UNIX" " SMB" "LDAP" "PEM" "SUDO"
            "[sudo]" "Repeat" "Bad" "Retype")
          t)
         " +\\)"
         "\\(?:" (regexp-opt password-word-equivalents) "\\|Response\\)"
         "\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?"
         ;; "[[:alpha:]]" used to be "for", which fails to match non-English.
         "\\(?: [[:alpha:]]+ .+\\)?[\\s  ]*[:：៖][\\s  ]*\\'")))

;; GnuPG
(use-package epa
  :ensure nil
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;; Text
(setq sentence-end-double-space nil) ; Don't use two spaces after period
(setq-default require-final-newline t)
(setq-default fill-column 79)

;; Spell-checking
(use-package ispell
  :ensure nil
  :config
  (setq ispell-dictionary "en"))
(use-package flyspell
  :ensure nil
  :after ispell
  :hook ((text-mode . flyspell-mode)))

;; Globally useful packages
(use-package flycheck
  :demand
  :config
  (global-flycheck-mode))
(use-package flycheck-irony
  :after flycheck
  :hook (irony-mode . flycheck-irony-setup))
(use-package flycheck-rust
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))

(use-package company
  :demand
  :bind
  ("C-<tab>" . company-complete)
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (global-company-mode))
(use-package company-go
  :after company
  :hook (go-mode . (lambda ()
                     (add-to-list 'company-backends 'company-go))))
(use-package company-irony
  :after company
  :hook (irony-mode . (lambda ()
                        (add-to-list 'company-backends 'company-irony))))

;; Org mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
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
;; Random useful packages for Elisp programming
(use-package f)

;; Scheme setup
(use-package geiser
  :defines (geiser-active-implementations geiser-guile-load-path)
  :config
  (setq geiser-active-implementations '(guile)))

;; C/C++ setup
(defun my-clang-format-buffer ()
  "Formats the current buffer with clang-format.

The formatting will only occur if there is a .clang-format file
somewhere in one of the parent directories of the current
buffer."
  (when (f-traverse-upwards
         (lambda (path)
           (f-exists? (f-expand ".clang-format" path)))
         (f-dirname (buffer-file-name)))
    (clang-format-buffer)))
(defun my-c-setup ()
  "Custom setup for 'c-mode' and 'c++-mode'."
  (add-hook 'before-save-hook #'my-clang-format-buffer nil t))
(add-hook 'c-mode-hook #'my-c-setup)
(add-hook 'c++-mode-hook #'my-c-setup)
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(use-package ggtags
  ;; Note: you need ctags and GNU Global installed to use this
  :hook (c-mode-common . ggtags-mode))

(use-package irony
  :hook ((c-mode c++-mode) . irony-mode)
  :config
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
(use-package clang-format
  :commands clang-format-buffer)

;; Go setup
(defun my-go-setup ()
  "Custom setup for go-mode."
  (add-hook 'before-save-hook #'gofmt-before-save nil t))
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook #'my-go-setup)
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
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'my-rust-setup)
  :config
  (setq rust-format-on-save t))
(use-package racer
  :hook (rust-mode . racer-mode)
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode))

;; TeX setup
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . tex-mode))

;; Git setup
(use-package magit
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :bind ("C-x g" . magit-status))
(setq vc-follow-symlinks t)             ; disable annoying question

;; Email setup (gnus)
(setq user-mail-address "ianprime0509@gmail.com"
      user-full-name "Ian Johnson")
(use-package sendmail
  :ensure nil
  :config
  (setq send-mail-function 'sendmail-send-it)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/bin/msmtp"))
(use-package gnus
  :ensure nil
  :bind ("C-c g" . gnus)
  :defines (nnmail-expiry-wait
            nnmail-expiry-target
            gnus-save-newsrc-file
            gnus-posting-styles
            gnus-group-line-format)
  :functions gnus-topic-mode
  :config
  ;; Account setup
  (setq gnus-save-newsrc-file nil)
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
  (setq bbdb-update-records-p 'search)
  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-mua-pop-up nil)
  (add-hook 'bbdb-after-change-hook #'bbdb-save))

;; Useful applications
;; Transmission
(use-package transmission)


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
;; Don't try to use the clipboard manager, since it causes slowdown
(setq x-select-enable-clipboard-manager nil)

(provide 'init)
;;; init.el ends here
