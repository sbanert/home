;; Gruvbox theme
(use-package doom-themes
  :config (load-theme 'doom-gruvbox t)
  :custom (doom-gruvbox-dark-variant "hard"))
(use-package all-the-icons)               ;; Needed for doom-modeline
(use-package doom-modeline :config (doom-modeline-mode 1))

;; Parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(show-paren-mode 1)  ;; Highlight matching parentheses

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
(column-number-mode)

;; Do not show line numbers in certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Flash if something goes wrong
(setq visible-bell t)

;; Font: Fira Code needs to be installed on the system
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)
;; (use-package fira-code-mode :config (global-fira-code-mode)
;;   :custom
;;   (fira-code-mode-disabled-ligatures '(; "[]" ;; List of ligatures to turn off
;; 					; "#{"
;; 					; "#("
;; 					; "#_"
;; 					; "#_("
;; 				       "x")))
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono for Powerline" :height 110)
