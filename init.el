 (package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/awesome-tray"))
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/awesome-tab"))
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/netease-cloud-music.el"))


;; Package Management
;; ------------------------------------------------------
(require 'init-packages)

;; const vars
;; ------------------------------------------------------
(require 'init-const)

;; UI Management
;; ------------------------------------------------------
(require 'init-ui)

;; Default Management
;; ------------------------------------------------------
(require 'init-better-default)

;; Org-mode Management
;; ------------------------------------------------------
(require 'init-org)

;; Keybindings Management
;; ------------------------------------------------------
(require 'init-keybindings)

;; web config
;; ------------------------------------------------------
(require 'init-web)

;; pyim
;; ------------------------------------------------------
(require 'init-pyim)

;; markdown
;; ------------------------------------------------------
(require 'init-markdown)

;; eshell
;; ------------------------------------------------------
(require 'init-eshell)
;; (require 'init-shell)

;; flycheck
;; ------------------------------------------------------
;; (require 'init-flycheck)

;; theme
;; ------------------------------------------------------
(require 'init-theme)

;; netease
;; ------------------------------------------------------
(require 'init-netease)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(flycheck-checker-error-threshold 1500)
 '(package-selected-packages
   (quote
    (company-web company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell popwin monokai-theme emmet-mode js2-refactor web-mode expand-region iedit helm-ag pyim prettier-js typescript-mode flycheck magit use-package rjsx-mode tide)))
 '(popwin:popup-window-position (quote right))
 '(popwin:popup-window-width 60))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(iedit-occurrence ((t (:inherit region))))
 '(js2-error ((t (:foreground "red"))))
 '(js2-external-variable ((t (:foreground "dark gray"))))
 '(js2-function-call ((t (:foreground "yellow green")))))
