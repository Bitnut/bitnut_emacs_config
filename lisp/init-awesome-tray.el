;;; Require

(use-package awesome-tray
  :ensure nil
  :hook after-init
  :config
  (add-to-list 'awesome-tray-active-modules
             '"buffer-name"))

(provide 'init-awesome-tray)
