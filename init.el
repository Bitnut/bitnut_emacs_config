 (package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/packages/tide")

;; Package Management
;; ------------------------------------------------------
(require 'init-packages)

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

;; tide config
;; ------------------------------------------------------
(require 'init-tide)

;; pyim
;; (require 'init-pyim) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(popwin:popup-window-position (quote right))
 '(popwin:popup-window-width 60))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(iedit-occurrence ((t (:inherit region))))
 '(js2-error ((t (:foreground "red"))))
 '(js2-external-variable ((t (:foreground "dark gray"))))
 '(js2-function-call ((t (:foreground "yellow green")))))
