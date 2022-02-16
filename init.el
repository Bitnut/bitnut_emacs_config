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
(require 'init-dired)

;; flycheck
;; ------------------------------------------------------
(require 'init-flycheck)

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
(require 'init-shell)

;; ediff
;; ------------------------------------------------------
(require 'init-ediff)

;; editorconfig
;; ------------------------------------------------------
(require 'init-editorconfig)

;; autosave
;; ------------------------------------------------------
(require 'init-autosave)

;; pangu
;; ------------------------------------------------------
(require 'init-pangu)

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
;; ------------------------------------------------------
(require 'init-dashboard)
