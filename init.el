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

;;; gc part
;; set gc first, largest gc while starting emacs
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; recover GC values after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000  ;; ~781KB
                  gc-cons-percentage 0.1)))


;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp" "elisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; (package-initialize)

;; Package Management
;; ------------------------------------------------------
(require 'init-packages)

;; Basic settings
;; ------------------------------------------------------
(require 'init-basic)

;; Default Management
;; ------------------------------------------------------
(require 'init-better-default)

;; Keybindings Management
;; ------------------------------------------------------
(require 'init-keybindings)

;; UI Management
;; ------------------------------------------------------
(require 'init-ui)

;; functions
;; ------------------------------------------------------
(require 'init-func)

;; hydra
;; ------------------------------------------------------
(require 'init-hydra)

;; edit
;; ------------------------------------------------------
(require 'init-edit)

;; ivy
;; ------------------------------------------------------
(require 'init-ivy)

;; company
;; ------------------------------------------------------
(require 'init-company)

;; dired
;; ------------------------------------------------------
;; (require 'init-dired)

;; witch-mode
;; ------------------------------------------------------
(require 'init-witch)

;; Org-mode Management
;; ------------------------------------------------------
(require 'init-org)

;; markdown
;; ------------------------------------------------------
(require 'init-md)

;; shell
;; ------------------------------------------------------
;; (require 'init-shell)

;; ediff
;; ------------------------------------------------------
(require 'init-ediff)

;; editorconfig
;; ------------------------------------------------------
(require 'init-editorconfig)

;; autosave
;; ------------------------------------------------------
(require 'init-autosave)

;; acewindow
;; ------------------------------------------------------
(require 'init-acewindow)

;; web config
;; ------------------------------------------------------
(require 'init-web)

;; lsp
;; ------------------------------------------------------
(require 'init-lsp)

;; meow
;; ------------------------------------------------------
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

;; scdv
;;
;; (require 'init-sdcv)

;; rust-mode
;; ------------------------------------------------------
(require 'init-rust)

;; rime-mode
;; ------------------------------------------------------
(require 'init-rime)

;; dashboard
(require 'init-dashboard)

(require 'awesome-tray)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-idle-delay 0.08 t)
;;  '(company-minimum-prefix-length 1 t)
;;  '(custom-safe-themes
;;    '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
;;  '(flycheck-checker-error-threshold 1500)
;;  '(package-selected-packages
;;    '(org-superstar dired-git-info modus-themes esup org-pomodoro treemacs-persp treemacs-magit treemacs-projectile company-web company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell popwin monokai-theme emmet-mode js2-refactor web-mode expand-region iedit prettier-js typescript-mode flycheck magit use-package rjsx-mode tide))
;;  '(popwin:popup-window-position 'right)
;;  '(popwin:popup-window-width 60))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
;;  '(iedit-occurrence ((t (:inherit region))))
;;  '(js2-error ((t (:foreground "red"))))
;;  '(js2-external-variable ((t (:foreground "dark gray"))))
;;  '(js2-function-call ((t (:foreground "yellow green"))))
;;  '(lsp-ui-sideline-code-action ((t (:inherit warning))))
;;  '(org-ellipsis ((t (:foreground nil))))
;;  '(org-pomodoro-mode-line ((t (:inherit warning))))
;;  '(org-pomodoro-mode-line-break ((t (:inherit success))))
;;  '(org-pomodoro-mode-line-overtime ((t (:inherit error)))))
