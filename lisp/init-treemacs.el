(require 'init-const)

(when emacs/>=26p
  ;; A tree layout file explorer
  (use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f7]        . treemacs)
           ("M-0"       . treemacs-select-window)
           ("C-x 1"     . treemacs-delete-other-windows)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t b"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :config
    (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
          treemacs-missing-project-action  'remove
          treemacs-sorting                 'alphabetic-asc
          treemacs-follow-after-init       t
          treemacs-width                   35
          treemacs-wide-toggle-width       70
          )
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (use-package treemacs-projectile
      :ensure t
      :after projectile
      :bind (:map projectile-command-map
             ("h" . treemacs-projectile)))

    (use-package treemacs-magit
      :ensure t
      :after magit
      :commands treemacs-magit--schedule-update
      :hook ((magit-post-commit
              git-commit-post-finish
              magit-post-stage
              magit-post-unstage)
             . treemacs-magit--schedule-update))

    (use-package treemacs-persp
      :ensure t
      :after persp-mode
      :demand t
      :functions treemacs-set-scope-type
      :config (treemacs-set-scope-type 'Perspectives))))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
