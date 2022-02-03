;; init-packages.el --- initialize packages from melpa -*- lexical-binding: t -*-
;;
;; Filename: init-packages.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes packages from melpa using use-package macro
;; as well as auto-package-update, diminish, gnu-elpa-keyring-update
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

(require 'init-const)
(require 'init-my-funcs)

;; Set ELPA packages
(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; ensure `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; globally hack use-package
;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
;; (use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :hook (after-init . paradox-enable)
  :init (setq paradox-execute-asynchronously t
              paradox-github-token t
              paradox-display-star-count nil)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                "Display `page-break-lines' in \"*Paradox Report*\"."
                (let ((buf (get-buffer "*Paradox Report*"))
                      (inhibit-read-only t))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (page-break-lines-mode 1)))))
              t)))

;; smartparens
(use-package smartparens
  :hook ((after-init . smartparens-global-mode)
         ;; (awk-mode (lambda () (setq-local smartparens-global-mode nil)))
         )
  :config
  )
;; (smartparens-global-mode t)
;; (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
;; (add-hook 'awk-mode-hook (lambda () (setq-local smartparens-global-mode nil)))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t))

;; (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; popwin
(use-package popwin
  :hook (after-init . popwin-mode))

(use-package all-the-icons)

;; code folding
;; (add-hook 'prog-mode-hook 'origami-mode)
;; (with-eval-after-load 'origami
;;   (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
;;   (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))


;; keyfreq
(use-package keyfreq
  :hook (after-init . keyfreq-mode)
  :config
  (keyfreq-autosave-mode 1)
  )
;; (require 'keyfreq)
;; (keyfreq-mode 1)

(provide 'init-packages)
