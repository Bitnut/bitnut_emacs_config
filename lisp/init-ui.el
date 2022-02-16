(require 'init-const)
(require 'init-func)

(global-linum-mode -1)

(global-hl-line-mode t)

(setq inhibit-splash-screen 1)

(defun my-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth) ;; this makes the frame go fullscreen
  (tool-bar-mode -1)  ;; remove toolbar & menu bar
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;; Fullscreen
(add-hook 'after-init-hook #'my-fullscreen)

;; Child frame
(when (childframe-workable-p)
  (use-package posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (with-eval-after-load 'persp-mode
      (add-hook 'persp-load-buffer-functions
                (lambda (&rest _)
                  (posframe-delete-all))))
    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              (/ (plist-get info :parent-frame-height)
                 2))))))


;; 字体大小
(cond
 (sys/linuxp (set-face-attribute 'default nil :height 140))
 (sys/win32p (set-face-attribute 'default nil :height 140))
 (sys/WSL (set-face-attribute 'default nil :height 130)))


(provide 'init-ui)
;;; init-ui.el ends here
