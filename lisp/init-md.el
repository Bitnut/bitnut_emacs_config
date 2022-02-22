(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))
  ;; Preview via `grip'
  ;; Install: pip install grip
  (use-package grip-mode
    :bind (:map markdown-mode-command-map
           ("g" . grip-mode))
    :init
    (setq grip-update-after-change nil)
    (when-let ((credential (auth-source-user-and-password "api.github.com")))
      (setq grip-github-user (car credential)
            grip-github-password (cadr credential)))))
(provide 'init-md)

;; init-md end this
