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
		       ;; solarized-theme
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
(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)

;; js2-mode
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; nodejs
(require 'nodejs-repl)
(global-set-key (kbd "<f5>") 'nodejs-repl-send-buffer)
(global-set-key (kbd "C-`") 'nodejs-repl)

;; popwin
(require 'popwin)
(popwin-mode 1)



;; theme here
(add-to-list 'my/packages 'monokai-theme)
(load-theme 'monokai 1)

(provide 'init-packages)




