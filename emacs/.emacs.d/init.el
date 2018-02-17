;;; .emacs --- my custom Emacs configuration

;;; Commentary:

;;; This is all my custom Emacs setup.

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


;;; Appearance
;;; Interface
;;; Fonts
(set-frame-font "monospace 11" nil t)
(when (member "Noto Emoji" (font-family-list))
  (set-fontset-font t 'unicode  "Noto Emoji" nil 'prepend))

;;; Various interface elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq initial-scratch-message "")
(column-number-mode) ; Show column number in mode line

;;; Try to focus the new frame on creation
(add-to-list 'after-make-frame-functions #'select-frame-set-input-focus)

;;; Highlight (some) whitespace
(defun prevent-whitespace-mode-for-magit ()
  ;; From https://emacs.stackexchange.com/a/38778
  "Prevent whitespace mode from interfering with Magit."
  (not (derived-mode-p 'magit-mode)))

(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail trailing))
  (add-function
   :before-while whitespace-enable-predicate
   #'prevent-whitespace-mode-for-magit)
  (global-whitespace-mode))

;;; Theme
(use-package base16-theme
  :config
  (load-theme 'base16-gruvbox-dark-soft t))


;;; Editor behavior
;;; Misc. keybindings
(global-unset-key (kbd "<insert>"))     ; I hate overwrite mode
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-i") #'imenu)

;;; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset tab-width)
(setq-default backward-delete-char-untabify-method 'hungry) ; Delete entire tabs
(use-package hungry-delete
  :config
  (setq-default hungry-delete-chars-to-skip " \t")
  (global-hungry-delete-mode))

;;; Ido mode
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

;;; Common editing settings
(setq-default sentence-end-double-space t)
(setq-default require-final-newline t)
(setq-default fill-column 79)

;;; Auto-pair delimiters in programs
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-local-mode)))

;;; Auto-trim trailing whitespace
(defvar my-auto-trim-whitespace t
  "Auto-trim trailing whitespace on save.")
(make-variable-buffer-local 'my-auto-trim-whitespace)
(add-hook 'before-save-hook
          (lambda ()
            (when my-auto-trim-whitespace
              (delete-trailing-whitespace))))


;;; Utility modes (e.g. dired, shell)
;;; Dired
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh"))

;;; Git
(use-package magit
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :bind ("C-x g" . magit-status))
(setq vc-follow-symlinks t)             ; disable annoying question

;;; GnuPG
(use-package epa
  :ensure nil
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;;; Shell
(use-package shell
  :ensure nil
  :commands shell
  :bind ("C-c s" . shell)
  :config
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
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


;;; Utilities
;;; Auto-complete
(use-package company
  :demand
  :bind
  ("C-<tab>" . company-complete)
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-tooltip-align-annotations t)
  (global-company-mode))

;;; Spell-checking
(use-package ispell
  :ensure nil
  :config
  (setq ispell-dictionary "en"))
(use-package flyspell
  :ensure nil
  :after ispell
  :hook ((text-mode . flyspell-mode)))

;;; Syntax checking
(use-package flycheck
  :demand
  :config
  (global-flycheck-mode))

;;; Transmission (torrent client)
(use-package transmission)


;;; Major modes for file editing

;;; C/C++
(defun my-c-setup ()
  "Custom setup for 'c-mode' and 'c++-mode'."
  (add-hook 'before-save-hook #'my-clang-format-buffer nil t))
(add-hook 'c-mode-hook #'my-c-setup)
(add-hook 'c++-mode-hook #'my-c-setup)
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(use-package irony
  :hook ((c-mode c++-mode) . irony-mode)
  :config
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package flycheck-irony
  :after flycheck
  :hook (irony-mode . flycheck-irony-setup))
(use-package company-irony
  :after company
  :hook (irony-mode . (lambda ()
                        (add-to-list 'company-backends 'company-irony))))

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

(use-package clang-format
  :commands clang-format-buffer)

(use-package ggtags
  ;; Note: you need ctags and GNU Global installed to use this
  :hook (c-mode-common . ggtags-mode))

;;; CSS
(use-package css-mode
  :ensure nil
  :mode "\\.css\\'"
  :config
  (setq-default css-indent-offset 2))

;;; Go
(defun my-go-setup ()
  "Custom setup for go-mode."
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook #'my-go-setup)
  :config
  (setq gofmt-command "goimports"))

(use-package company-go
  :after company
  :hook (go-mode . (lambda ()
                     (add-to-list 'company-backends 'company-go))))

;;; HTML (web-mode)
(use-package web-mode
  :mode "\\.\\(?:html?\\|hbs\\)\\'"
  :config
  (setq web-mode-auto-close-style 1)
  (setq web-mode-markup-indent-offset 2))

;;; Javascript
(defun my-javascript-setup ()
  "Custom setup for JavaScript."
  (my-use-eslint-from-node-modules))

(defun my-use-eslint-from-node-modules ()
  ;; From https://emacs.stackexchange.com/a/21207
  "Tell Flycheck to use local eslint if installed."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package js2-mode
  :config
  (add-hook 'js2-mode-hook #'my-javascript-setup)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq-default js2-basic-offset 2))

(use-package rjsx-mode
  :after js2-mode
  :mode "\\.jsx?\\'")

(use-package npm-mode)

(use-package prettier-js
  :hook ((css-mode js2-mode json-mode) . prettier-js-mode))

;;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;;; Lisp (general)
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

;;; Lisp (Emacs)
(defun my-emacs-lisp-setup ()
  "Custom setup for Emacs Lisp."
  (local-set-key (kbd "C-c C-b") 'eval-buffer))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-setup)
;;; Random useful packages for Elisp programming
(use-package f)

;; Lisp (Scheme)
(use-package geiser
  :defines (geiser-active-implementations geiser-guile-load-path)
  :config
  (setq geiser-active-implementations '(guile)))

;;; Markdown
(use-package markdown-mode
  :config
  (setq-default markdown-command "cmark"))

;;; Meson
(use-package meson-mode
  :mode "\\`\\(meson\\.build\\|meson_options\\.txt\\)\\'")

;;; Rust
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

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))

(use-package racer
  :hook (rust-mode . racer-mode)
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode))

;;; SCSS (SASS)
(use-package scss-mode
  :mode "\\.scss\\'")

;;; TeX
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . tex-mode))

;;; TOML
(use-package toml-mode
  :mode "\\.toml\\'")

;;; Typescript
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :config
  (setq-default typescript-indent-level 2))

(defun my-tide-setup ()
  "Custom setup for tide."
  (add-hook 'before-save-hook #'tide-format-before-save nil t)
  (eldoc-mode))

(use-package tide
  :hook ((typescript-mode js2-mode) . tide-setup)
  :config
  (add-hook 'tide-mode-hook #'my-tide-setup))

;;; Yaml
(use-package yaml-mode
  :mode "\\.yml\\'")


;;; Org mode
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


;;; Gnus (email)
(setq user-mail-address "ianprime0509@gmail.com"
      user-full-name "Ian Johnson")
(use-package smtpmail-multi
  :config
  (setq smtpmail-multi-accounts
        '((personal . ("ianprime0509@gmail.com"
                        "smtp.gmail.com"
                        587
                        "ianprime0509@gmail.com"
                        nil nil nil nil))
          (professional . ("iantimothyjohnson@gmail.com"
                           "smtp.gmail.com"
                           587
                           "iantimothyjohnson@gmail.com"
                           nil nil nil nil))
          (uva . ("ij6fd@virginia.edu"
                  "smtp.gmail.com"
                  587
                  "ij6fd@virginia.edu"
                  nil nil nil nil))))
  (setq smtpmail-multi-associations
        '(("ianprime0509@gmail.com" personal)
          ("iantimothyjohnson@gmail.com" professional)
          ("ij6fd@virginia.edu" uva)))
  (setq smtpmail-multi-default-account 'personal))
(use-package sendmail
  :ensure nil
  :config
  (setq send-mail-function 'smtpmail-multi-send-it)
  (setq message-send-mail-function 'smtpmail-multi-send-it))
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

;;; BBDB (address book)
(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq bbdb-add-mails 'query)
  (setq bbdb-update-records-p 'search)
  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-mua-pop-up nil)
  (add-hook 'bbdb-after-change-hook #'bbdb-save))


;;; Miscellaneous
;;; Move save files somewhere else
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;;; Don't try to use the clipboard manager, since it causes slowdown
(setq x-select-enable-clipboard-manager nil)

(provide 'init)
;;; init.el ends here
