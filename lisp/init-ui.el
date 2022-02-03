(require 'init-const)


(global-linum-mode -1)

(global-hl-line-mode t)

(setq inhibit-splash-screen 1)

(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth) ;; this makes the frame go fullscreen
  (tool-bar-mode -1)  ;; remove toolbar & menu bar
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;; Fullscreen
(add-hook 'after-init-hook #'my-fullscreen)


;; 字体大小
(cond
 (sys/linuxp (set-face-attribute 'default nil :height 160))
 (sys/win32p (set-face-attribute 'default nil :height 140))
 (sys/WSL (set-face-attribute 'default nil :height 130)))


(provide 'init-ui)
