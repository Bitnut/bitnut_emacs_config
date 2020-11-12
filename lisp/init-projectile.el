;; Manage and navigate projects
;; (use-package projectile
;;   :diminish
;;   :bind (:map projectile-mode-map
;;          ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
;;          ("C-c p" . projectile-command-map))
;;   :hook (after-init . projectile-mode)
;;   :init
;;   (setq projectile-mode-line-prefix ""
;;         projectile-sort-order 'recentf
;;         projectile-use-git-grep t)
;;   :config
;;   ;; (projectile-mode +1)
;;   ;; (projectile-update-mode-line)         ; Update mode-line at the first time
;;   )

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
