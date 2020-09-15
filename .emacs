(require 'package)
(require 'evil)
(require 'julia-mode)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(evil-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-C-u-scroll t)
 '(package-selected-packages '(julia-mode auctex auctex-latexmk evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
