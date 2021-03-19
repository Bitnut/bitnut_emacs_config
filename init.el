(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/awesome-tray"))
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/awesome-tab"))
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/auto-save"))
(add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/yasnippet-snippets"))

(setq pyim-dicts
      '((:name "dict1" :file "/home/picher/.emacs.d/pyim-another-dict.pyim")))
(add-hook 'emacs-startup-hook
          #'(lambda () (pyim-restart-1 t)))

;; Package Management
;; ------------------------------------------------------
(require 'init-packages)

;; utils
(require 'init-utils)

;; const vars
;; ------------------------------------------------------
(require 'init-const)

;; UI Management
;; ------------------------------------------------------
(require 'init-ui)

;; move-text
(require 'init-move-text)

;; Default Managementq
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
(require 'init-md)

;; eshell
;; ------------------------------------------------------
(require 'init-eshell)
(require 'init-shell)

;; todo
;; ------------------------------------------------------
;; (require 'init-todo)

;; awesome-tray
;; (require 'init-awesome-tray)

;; editorconfig
;; ------------------------------------------------------
(require 'init-editorconfig)

;; autosave
;; ------------------------------------------------------
(require 'init-autosave)

;; acewindow
;; ------------------------------------------------------
(require 'init-acewindow)

;; company
(require 'init-company)

;; lsp
(require 'init-lsp)

;; meow
(require 'init-meow)

;; projectile
;; ------------------------------------------------------
(require 'init-projectile)

;; theme
;; ------------------------------------------------------
(require 'init-theme)

;; yasnippet
;; ------------------------------------------------------
(require 'init-yasnippet)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(flycheck-checker-error-threshold 1500)
 '(package-selected-packages
   '(company-web company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell popwin monokai-theme emmet-mode js2-refactor web-mode expand-region iedit helm-ag pyim prettier-js typescript-mode flycheck magit use-package rjsx-mode tide))
 '(popwin:popup-window-position 'right)
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
 '(js2-function-call ((t (:foreground "yellow green"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning)))))
