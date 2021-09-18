;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize bitnut-emacs
;; Author: Bitnut
;; Copyright (C) 2021 Bitnut
;; Version: 1.0
;; URL: https://github.com/Bitnut/bitnut_emacs_config.git
;; Keywords: Bitnut .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; (when (version< emacs-version "25.1")
;;   (error "Requires Emacs 25.1 and above!"))

;; ;; for speedup, case-sensitive search is perfect
;; (setq auto-mode-case-fold nil)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             "Recover GC values after startup."
;;             (setq gc-cons-threshold 800000
;;                   gc-cons-percentage 0.1)))

;; ;; Load path
;; ;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; (defun update-load-path (&rest _)
;;   "Update `load-path'."
;;   (dolist (dir '("elisp" "lisp"))
;;     (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; (update-load-path)

(package-initialize)
;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp/")
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

  ;; recentf config
  ;; ------------------------------------------------------
  (require 'init-recentf)

  ;; recentf config
  ;; ------------------------------------------------------
  (require 'init-func)

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

  ;; ivy
  ;; ------------------------------------------------------
  (require 'init-ivy)

  ;; pyim
  ;; ------------------------------------------------------
  (require 'init-pyim)

  ;; markdown
  ;; ------------------------------------------------------
  (require 'init-md)

  ;; eshell
  ;; ------------------------------------------------------
  ;; (require 'init-eshell)
  (require 'init-shell)

  ;; editorconfig
  ;; ------------------------------------------------------
  (require 'init-editorconfig)

  ;; autosave
  ;; ------------------------------------------------------
  (require 'init-autosave)

  ;; acewindow
  ;; ------------------------------------------------------
  (require 'init-acewindow)

  ;; lsp
  (require 'init-lsp)

  ;; meow
  (require 'init-meow)

  ;; projectile
  ;; ------------------------------------------------------
  (require 'init-projectile)

  ;; treemacs
  ;; ------------------------------------------------------
  (require 'init-treemacs)

  ;; theme
  ;; ------------------------------------------------------
  (require 'init-theme)

  ;; yasnippet
  ;; ------------------------------------------------------
  (require 'init-yasnippet)

  ;; company
  ;; ------------------------------------------------------
  (require 'init-company)


  ;; rust-mode
  ;; ------------------------------------------------------
  ;; (require 'init-rust)

  ;; dashboard
  (require 'init-dashboard)

  ;; elfeed
  ;; ------------------------------------------------------
  ;; (require 'init-elfeed)
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  ;; (add-to-list `load-path (expand-file-name "~/.emacs.d/elisp"))
  ;; (add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/awesome-tray"))
  ;; (add-to-list `load-path (expand-file-name "~/.emacs.d/elisp/awesome-tab"))
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

  ;; recentf config
  ;; ------------------------------------------------------
  (require 'init-recentf)

  ;; recentf config
  ;; ------------------------------------------------------
  (require 'init-func)

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

  ;; ivy
  ;; ------------------------------------------------------
  (require 'init-ivy)

  ;; pyim
  ;; ------------------------------------------------------
  (require 'init-pyim)

  ;; markdown
  ;; ------------------------------------------------------
  (require 'init-md)

  ;; eshell
  ;; ------------------------------------------------------
  ;; (require 'init-eshell)
  ;; (require 'init-shell)

  ;; editorconfig
  ;; ------------------------------------------------------
  (require 'init-editorconfig)

  ;; acewindow
  ;; ------------------------------------------------------
  (require 'init-acewindow)

  ;; lsp
  (require 'init-lsp)

  ;; meow
  (require 'init-meow)

  ;; projectile
  ;; ------------------------------------------------------
  (require 'init-projectile)

  ;; treemacs
  ;; ------------------------------------------------------
  (require 'init-treemacs)

  ;; theme
  ;; ------------------------------------------------------
  (require 'init-theme)

  ;; yasnippet
  ;; ------------------------------------------------------
  (require 'init-yasnippet)

  ;; company
  ;; ------------------------------------------------------
  (require 'init-company)

  ;; dashboard
  (require 'init-dashboard))

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
   '(esup org-pomodoro treemacs-persp treemacs-magit treemacs-projectile company-web company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell popwin monokai-theme emmet-mode js2-refactor web-mode expand-region iedit pyim prettier-js typescript-mode flycheck magit use-package rjsx-mode tide))
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
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error)))))
