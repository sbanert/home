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

;; Hints for keybindings
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Gruvbox theme
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-gruvbox t))
(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Evil mode (vim keybindings)
(use-package evil
  :ensure t
  :config
  (evil-mode 1))
;; Use the keybindings gc and gcc to comment/uncomment.
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))
;; Parentheses management
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Julia language major mode
(use-package julia-mode
  :ensure t)

(use-package auctex
  :defer t
  :ensure t)
(use-package auctex-latexmk
  :ensure t)

(use-package magit
  :ensure t)

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

;; Install every package that I use with use-package
;; TODO

;; Default LaTeX packages for org-mode.
;; TODO: Use cells of appropriate format instead of raw strings.
;; TODO: Embed the AMS packages in the banert package.
(setq org-latex-packages-alist '("\\usepackage{amsmath}" "\\usepackage{amsthm}" "\\usepackage{banert}"))
;; Highlight LaTeX in org mode.
(setq org-highlight-latex-and-related '(latex script entities))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-C-u-scroll t)
 '(package-selected-packages
   '(evil-surround evil-commentary magit all-the-icons doom-modeline doom-themes zenburn-theme which-key use-package julia-mode auctex auctex-latexmk evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
