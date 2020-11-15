(require 'init-const)

;; remove toolbar & menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

(scroll-bar-mode -1)

(global-linum-mode 1)

(global-hl-line-mode t)

(setq inhibit-splash-screen 1)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 字体大小
(cond
 (sys/win32p (set-face-attribute 'default nil :height 120))
 (sys/linuxp (set-face-attribute 'default nil :height 160)))


(provide 'init-ui)
