 (when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))


;; cl - Common Lisp Extension
(require 'cl)


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



;;personal config file
(tool-bar-mode -1)

(scroll-bar-mode -1)

(global-linum-mode 1)

(global-hl-line-mode t)

(delete-selection-mode 1)

(setq make-backup-files nil)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

(setq-default cursor-type 'bar)

(require 'org)
(setq org-src-fontify-natively t)
;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/Work/org"))

;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)


(setq inhibit-splash-screen 1)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; recentf-mode

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)
(require 'hungry-delete)
(global-hungry-delete-mode t)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; theme here
(add-to-list 'my/packages 'monokai-theme)

(load-theme 'monokai 1)

(set-face-attribute 'default nil :height 160)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f3>") 'open-init-file)

(require 'nodejs-repl)
(global-set-key (kbd "<f5>") 'nodejs-repl-send-buffer)
(global-set-key (kbd "C-`") 'nodejs-repl)

(add-hook 'after-init-hook 'global-company-mode)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-error ((t (:foreground "red"))))
 '(js2-external-variable ((t (:foreground "dark gray"))))
 '(js2-function-call ((t (:foreground "yellow green")))))
