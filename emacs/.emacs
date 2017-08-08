(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq use-package-always-ensure t)

(use-package evil
  :config
  (evil-mode 1))

;; Appearance
(use-package base16-theme
  :config
  (load-theme 'base16-phd t))
(add-to-list 'default-frame-alist '(font . "Inconsolata 12"))
(global-linum-mode) ; Show line numbers on the side
(column-number-mode) ; Show column number in mode line
;; Highlight (some) whitespace
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)

;; Interface
(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;; This disables a setting which doesn't work well on Wayland
(setq select-enable-clipboard nil)

;; Evil (vim) mode
(evil-set-initial-state 'term-mode 'emacs)

;; Indentation
(setq-default tab-width 8)
(setq-default indent-tabs-mode t)
(setq-default evil-shift-width tab-width)
(setq-default c-basic-offset tab-width)
(setq-default backward-delete-char-untabify-method 'hungry) ; Delete entire tabs
(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++))

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

;; Mail (mutt)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; C/C++ setup
(defun my-c-setup ()
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


;; Git setup
(use-package magit
  :config
  (use-package evil-magit)
  :bind ("C-x g" . magit-status))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))

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
