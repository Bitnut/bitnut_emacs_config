 (package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")


;; Package Management
;; ------------------------------------------------------
(require 'init-packages)

;;personal config file using system settings
(tool-bar-mode -1)

(scroll-bar-mode -1)

(global-linum-mode 1)

(global-hl-line-mode t)

(global-auto-revert-mode)

(delete-selection-mode 1)

(setq make-backup-files nil)

(setq-default cursor-type 'bar)

(require 'org)
(setq org-src-fontify-natively t)
;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/Work/org"))

;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)


(setq inhibit-splash-screen 1)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; recentf-mode 配置
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; 字体大小
(set-face-attribute 'default nil :height 160)

;; 配置根文件快捷键设置
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f3>") 'open-init-file)

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
