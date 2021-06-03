(use-package elfeed
  :custom ((elfeed-use-curl t)
           (elfeed-db-directory "~/Documents/elfeed/db/")
           (elfeed-curl-extra-arguments '("-x" "socks5://172.21.192.1:10808")))
  :bind (:map elfeed-show-mode-map
              ("h" . evil-backward-char)
              ("8" . my/elfeed-toggle-star)
              ("9" . my/elfeed-show-images)
              :map elfeed-search-mode-map
              ("8" . my/elfeed-search-star)
              ("*" . my/elfeed-search-unstar))
  :init
  (setq elfeed-set-max-connections 30)
  (setq elfeed-set-timeout 30)
  (defun my/elfeed-set-line-space ()
    (setq-local line-spacing 0.3))
  :hook (elfeed-search-mode . my/elfeed-set-line-space)
  :config
  (setq elfeed-search-filter "@6-months-ago +unread #50"
        shr-inhibit-images t
        elfeed-feeds '())
  (when-let ((ff-cmd (executable-find "firefox")))
    (setq
     browse-url-browser-function 'browse-url-generic
     browse-url-generic-program ff-cmd))
  (defun my/elfeed-show-images ()
    (interactive)
    (let ((shr-inhibit-images nil))
      (elfeed-show-refresh)))
  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun my/elfeed-open-db-and-load ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun my/elfeed-close-db-and-save ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    ;; (quit-window)
    )

  (defun my/elfeed-toggle-star ()
    (interactive)
    (when elfeed-show-entry
      (let ((tag (intern "starred")))
        (if (elfeed-tagged-p tag elfeed-show-entry)
            (elfeed-untag elfeed-show-entry tag)
          (elfeed-tag elfeed-show-entry tag)))
      (elfeed-show-refresh)))

  (defun my/elfeed-search-star ()
    (interactive)
    (let ((tag (intern "starred"))
          (entries (elfeed-search-selected)))
      (cl-loop for entry in entries do (elfeed-tag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun my/elfeed-search-unstar ()
    "Remove starred tag from all selected entries."
    (interactive)
    (let ((tag (intern "starred"))
          (entries (elfeed-search-selected)))
      (cl-loop for entry in entries do (elfeed-untag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  )

(use-package elfeed-dashboard
  :config
  (setq elfeed-dashboard-file "~/Documents/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package elfeed-org
  :custom ((rmh-elfeed-org-files (list "~/Documents/elfeeds.org")))
  :config
  (defun my/reload-org-feeds ()
    (interactive)
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id))
  (advice-add 'elfeed-dashboard :before #'my/reload-org-feeds)
  (elfeed-org))

(provide 'init-elfeed)
