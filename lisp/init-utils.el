(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t))

(provide 'init-utils)
