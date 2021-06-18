;;; init-package.el --- initialize packages from melpa -*- lexical-binding: t -*-
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


;; cl - Common Lisp Extension
(require 'cl)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(when (>= emacs-major-version 24)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; Add Packages
(defvar my/packages '(
		      ;; --- Auto-completion ---
		      company
                      company-quickhelp
		      ;; --- Better Editor ---
		      hungry-delete
		      swiper
		      counsel
		      smartparens
                      wgrep
                      keyfreq
                      meow
                      elfeed
                      elfeed-dashboard
                      elfeed-org
                      ;; --- Hydra ---
                      hydra
                      pretty-hydra
		      ;; --- Major Mode ---
		      js2-mode
                      go-mode
                      php-mode
                      json-mode
                      ;; dashboard
                      dashboard
                      page-break-lines
		      ;; --- Minor Mode ---
                      treemacs
                      treemacs-icons-dired
		      nodejs-repl
		      exec-path-from-shell
                      vterm
                      shell-pop
		      popwin
                      projectile
		      emmet-mode
		      js2-refactor
		      web-mode
		      expand-region
		      iedit
		      helm-ag
                      yasnippet
                      ivy-yasnippet
                      ivy-rich
                      all-the-icons-ivy-rich
                      hl-todo
                      ccls
                      ;; org
                      org-superstar
		      ;;typescritpt
		      typescript-mode
		      ;; magit
		      magit
                      git-timemachine
		      use-package
		      rjsx-mode
		      ;; tide
		      ;; pyim 弹窗
		      posframe
		      ;; markdown
		      markdown-mode
		      grip-mode
		      ;; editorconfig
		      editorconfig
                      ;; ace-window
                      ace-window
                      ;; diminish
                      diminish
		      ;; package groups
		      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
		      ;; code folding
		      origami
		      ;; icons
		      all-the-icons
		      spaceline-all-the-icons
		      all-the-icons-dired
		      ;; lsp
		      lsp-mode
                      lsp-ui
		      eglot
		      ;; shell
		      shell
		      xterm-color
		      shell-pop
		      ;; esh-doc
		      esh-help
		      eshell-z
		      eshell-prompt-extras
                      ;; which key
                      which-key
		      ;; flycheck
		      flycheck
		      flycheck-posframe
		      flycheck-pos-tip
		      flycheck-popup-tip
		      ;;pyim input method
		      pyim
		      pyim-basedict
		      ;; --- Themes ---
		      ;;monokai-theme
		      spacemacs-theme
		      ;; netease-music
		      request
		      async
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))
(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; requirements bellow
;; same order as above

;; hungry
(require 'hungry-delete)
(global-hungry-delete-mode t)

;; smartparens
(smartparens-global-mode t)
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
(add-hook 'awk-mode-hook (lambda () (setq-local smartparens-global-mode nil)))


;; js2-mode
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.js\\'" . js2-refactor-mode))
       auto-mode-alist))

(defun my-js2-mode-hook ()
  (progn
    (setq forward-sexp-function nil)
    ;; (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
    (set (make-local-variable 'semantic-mode) nil)
    ))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)


;; nodejs
(require 'nodejs-repl)
(global-set-key (kbd "<f5>") 'nodejs-repl-send-buffer)


;; popwin
(require 'popwin)
(popwin-mode 1)

(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
			       ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function 'js2-imenu-make-index)))

;; call imenu to list functions
(global-set-key (kbd "M-s i") 'counsel-imenu)



;; expand-region. = to expand, - to contract, 0 to reset
(global-set-key (kbd "C-=") 'er/expand-region)
;; iedit
(require 'iedit)

;; (use-package helm-ag
;;   :config
;;   (setq helm-ag-use-grep-ignore-list t)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-the icons/modeline;;remember to call 'M-x all-the-icons-install-fonts'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons)
;; (use-package spaceline-all-the-icons
;;   :config (spaceline-all-the-icons-theme)
;;   (setq spaceline-all-the-icons-separator-type 'slant)
;;   (setq separator-scale '1)
;;   )
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; code folding
(add-hook 'prog-mode-hook 'origami-mode)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))

;; org-prettier
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; eshell
(global-set-key (kbd "C-`") 'vterm)

;; keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(provide 'init-packages)
