(use-package projectile
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :config
  (progn
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'default
        projectile-sort-order 'recentf
        projectile-mode-line-prefix "")
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules")))

(provide 'init-projectile)
