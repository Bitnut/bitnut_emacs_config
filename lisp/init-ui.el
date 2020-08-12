(tool-bar-mode -1)

(scroll-bar-mode -1)

(global-linum-mode 1)

(global-hl-line-mode t)

(setq inhibit-splash-screen 1)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 字体大小
(set-face-attribute 'default nil :height 120)

(provide 'init-ui)
