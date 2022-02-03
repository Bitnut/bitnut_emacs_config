;; code

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))
  (use-package all-the-icons-dired
    :diminish
    :if (icons-displayable-p)
    :hook (dired-mode . all-the-icons-dired-mode)
    :init (setq all-the-icons-dired-monochrome nil))

  )

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(provide 'init-dired)
