;; cl - Common Lisp Extension
(require 'cl)


(when (>= emacs-major-version 24)
  ;;(require 'package)
 
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
;; Add Packages
(defvar my/packages '(
		      ;; --- Auto-completion ---
		       company		       
		       ;; --- Better Editor ---		       	       
		       hungry-delete
		       swiper
		       counsel
		       smartparens
		       ;; --- Major Mode ---
		       js2-mode
		       ;; --- Minor Mode ---
		       nodejs-repl
		       exec-path-from-shell
		       ;; --- popwin ---
		       popwin
		       ;; --- Themes ---
		       monokai-theme
		       emmet-mode
		       js2-refactor
		       web-mode
		       expand-region
		       iedit
		       helm-ag
		       ;;pyim input method
		       pyim
		       pyim-basedict
		       ;; prettier
		       prettier-js
		       ;;typescritpt
		       typescript-mode
		       ;; flycheck
		       flycheck
		       ;; magit
		       magit
		       use-package
		       rjsx-mode
		       tide
		       ;;
		       posframe
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

;; company
(add-hook 'after-init-hook 'global-company-mode) 

;; hungry
(require 'hungry-delete)
(global-hungry-delete-mode t)

;; swiper counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; smartparens
;; (require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)


;; js2-mode
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.js\\'" . js2-mode)
;; 	 ("\\.js\\'" . js2-refactor-mode)
;; 	 ("\\.html\\'" . web-mode)
;; 	 ("\\.vue\\'" . web-mode))
;;        auto-mode-alist))
;; (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; nodejs
(require 'nodejs-repl)
(global-set-key (kbd "<f5>") 'nodejs-repl-send-buffer)
(global-set-key (kbd "C-`") 'nodejs-repl)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; config for web-mode
;; (defun my-web-mode-indent-setup ()
;;   (setq web-mode-markup-indent-offset 4) ; web-mode, html tag in html file
;;   (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
;;   (setq web-mode-code-indent-offset 4)   ; web-mode, js code in html file
;;   )
;; (add-hook 'web-mode-hook 'my-web-mode-indent-setup)

;; theme here
(add-to-list 'my/packages 'monokai-theme)
(load-theme 'monokai 1)


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


;; pyim config
;; (require 'pyim)
;; (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;; (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;; (setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; prettier-js config
;; (require 'prettier-js)
;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)

;; (setq prettier-js-args '(
;; 			 "--trailing-comma" "all"
;; 			 "--tab-width" "4"
;; 			 "--single-quote" "true"
;; 			 "--print-width" "120"
;; 			 "--jsx-bracket-same-line" "true"
;; ))


(provide 'init-packages)




