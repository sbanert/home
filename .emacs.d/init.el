;; Keep the customize variables out of this file.
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Set up the package manager.
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)
(package-initialize)

;; Install use-package if it is not installed.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Hints for keybindings
(use-package which-key :config (which-key-mode))

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

;; Gruvbox theme
(use-package doom-themes :config (load-theme 'doom-gruvbox t))
(use-package all-the-icons)               ;; Needed for doom-modeline
(use-package doom-modeline :config (doom-modeline-mode 1))

;; Evil mode (vim keybindings)
(use-package evil
  :config
  (evil-mode 1)
  (setq evil-want-C-u-scroll t))
;; Use the keybindings gc and gcc to comment/uncomment.
(use-package evil-commentary :config (evil-commentary-mode))
;; Parentheses management
(use-package evil-surround :config (global-evil-surround-mode 1))

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

;; Recent buffers in a new Emacs session
(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
	recentf-max-saved-items 1000
	recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)

;; Language-specific packages
(use-package julia-mode)              ;; Julia language major mode
(use-package ledger-mode)             ;; Ledger mode for .ledger files
(use-package auctex
  :defer t
  :config
  (setq TeX-PDF-mode t)         ;; LaTeX support
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
(use-package org
  :config
  ;; Default LaTeX packages for org-mode.
  ;; TODO: Use cells of appropriate format instead of raw strings.
  ;; TODO: Embed the AMS packages in the banert package.
  (setq org-latex-packages-alist '("\\usepackage{amsmath}" "\\usepackage{amsthm}" "\\usepackage{banert}"))
  (use-package tablist)
  (use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-tools-install)
    :bind ("C-c C-g" . pdf-sync-forward-search)
    :defer t
    :config
    (pdf-tools-install)
    (setq mouse-wheel-follow-mouse t)
    (setq pdf-view-resize-factor 1.10))

  ;; Highlight LaTeX in org mode.
  (setq org-highlight-latex-and-related '(latex script entities)))

;; Package-independent settings
;; Highlight the current line
(global-hl-line-mode 1)
;; Do not show the startup screen
(setq inhibit-startup-screen t)
;; Show the toolbar and the menu bar, but not the scroll bar.
(tool-bar-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
;; Show line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
;; Highlight matching parentheses
(show-paren-mode 1)
;; Flash if something goes wrong
(setq visible-bell t)
;; Font: Fira Code needs to be installed on the system
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
