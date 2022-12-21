;;; banert --- My emacs configuration

;;; Commentary:
;;; Code:
;; Keep the customize variables out of this file.
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Set up the package manager.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if it is not installed.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; My name and e-mail
(let ((first-name "Sebastian")
      (last-name "Banert")
      (domain-name "control.lth.se")
      (klammeraffe (list 64)))
  (setq user-full-name (concat first-name " " last-name)
	user-mail-address (concat (downcase first-name)
				  "."
				  (downcase last-name)
				  klammeraffe
				  domain-name)))

(load-file "~/.emacs.d/look.el")     ;; Contains commands for themes and look and feel.
(defun banert/open-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; Hints for keybindings
(use-package which-key :config (which-key-mode))
;; Keybindings
(use-package general
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ae" 'eshell
   "as" 'shell

   ;; Buffers
   "b" '(:ignore t :which-key "Buffer")
   "bb" 'counsel-switch-buffer
   "bk" 'kill-this-buffer

   ;; Files
   "f" '(:ignore t :which-key "File")
   "fd" 'dired
   "fi" 'banert/open-init-file
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fs" 'save-buffer
   "fS" 'write-file

   ;; Help
   "h" '(:ignore t :which-key "Help")
   "hb" 'describe-bindings
   "hf" 'counsel-describe-function
   "hF" 'counsel-describe-face
   "hk" 'describe-key
   "ho" 'counsel-describe-symbol
   "hv" 'counsel-describe-variable

   ;; Projects
   "p" '(:ignore t :which-key "Project")
   "pc" 'projectile-compile-project
   "pf" 'counsel-projectile-find-file
   "pk" 'projectile-kill-buffers
   "po" 'org-projectile/goto-todos
   "pr" 'projectile-recentf

   ;; Packages
   "P" '(:ignore t :which-key "Packages")
   "Pr" 'package-refresh-contents
   "Pl" 'package-list-packages

   ;; Quit
   "q" '(:ignore t :which-key "Quit")
   "qq" 'save-buffers-kill-terminal

   ;; Windows
   "w" '(:ignore t :which-key "Window")
   )
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'org-mode-map
   :prefix ","
   :non-normal-prefix "C-,"
   "b" 'org-latex-export-to-pdf)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'LaTeX-mode-map
   :prefix ","
   :non-normal-prefix "C-,"
   "b" 'TeX-command-run-all)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'Lilypond-mode-map
   :prefix ","
   :non-normal-prefix "C-,"
   "b" 'LilyPond-command-lilypond
   ))

;; Emacs completion framework
;; TODO: Set up sensible keybindings for navigation in ivy buffer.
(use-package swiper)
(use-package ivy
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package lsp-mode)
(use-package lsp-ui)
(use-package projectile
  :init
  (projectile-mode +1))
(use-package org-projectile
  :init
  (org-projectile-per-project)
  :config
  (setq org-projectile-per-project-filepath "TODOs.org"))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; Evil mode (vim keybindings)
(use-package undo-tree
  :config
  (global-undo-tree-mode))
(use-package evil
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (setq evil-want-C-u-scroll t))
;; Use the keybindings gc and gcc to comment/uncomment.
(use-package evil-commentary :config (evil-commentary-mode))
;; Parentheses management
(use-package evil-surround :config (global-evil-surround-mode 1))

(use-package vterm)

;; IDE-type features
(use-package magit)
(use-package company
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  ;; TODO: Maybe vim-style keybindings.
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion
  (setq company-idle-delay 0.3)
  ;; Use company mode everywhere
  (global-company-mode t))
(use-package flycheck
  :custom (flycheck-haskell-ghc-executable "ghc -dynamic")
  :init (global-flycheck-mode))

(use-package paredit
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
	 ("M-{" . paredit-wrap-curly)))
  

;; Recent buffers in a new Emacs session
(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
	recentf-max-saved-items 1000
	recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)

;; Language-specific packages
(use-package proof-general)
(use-package company-coq)
(use-package julia-mode)              ;; Julia language major mode
(use-package ledger-mode)             ;; Ledger mode for .ledger files
(use-package haskell-mode)            ;; Haskell mode
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))                ;; Nix mode
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))
(use-package rust-mode)               ;; Rust support
(use-package cargo-mode)
(use-package json-mode)
(use-package lsp-haskell
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode. lsp))
(load-library "lilypond-init")        ;; Lilypond-mode
(use-package auctex
  :defer t
  :config
  (setq TeX-PDF-mode t)         ;; LaTeX support
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq-default TeX-interactive-mode t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
	    #'TeX-revert-document-buffer)
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
	TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))
(use-package tablist)
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (pdf-tools-install)
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))
(use-package org
  :init
  (setq org-list-allow-alphabetical t)
  :config
  ;; Default LaTeX packages for org-mode.
  ;; TODO: Use cells of appropriate format instead of raw strings.
  ;; TODO: Embed the AMS packages in the banert package.
  (setq org-latex-packages-alist '("\\usepackage{amsmath}" "\\usepackage{amsthm}" "\\usepackage{banert}"))
  ;; Highlight LaTeX in org mode.
  (setq org-highlight-latex-and-related '(latex script entities)))

;; Package-independent settings

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
