;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :bind (:map vterm-mode-map
           ([f9] . (lambda ()
                     (interactive)
                     (and (fboundp 'shell-pop)
                          (shell-pop nil))))))

;; eshell
;; (global-set-key (kbd "C-`") 'vterm)

;; Shell Pop
(use-package shell-pop
  :bind ([f9] . shell-pop)
  :init (setq shell-pop-window-size 30
              shell-pop-shell-type
	      shell-pop-universal-key C-t
              (cond ((fboundp 'vterm) '("vterm" "*vterm*" #'vterm))
                    (t '("terminal" "*terminal*"
                         (lambda () (term shell-pop-term-shell)))))))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
