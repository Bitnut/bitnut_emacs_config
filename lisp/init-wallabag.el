(use-package wallabag
  :defer t
  :load-path "~/.emacs.d/elisp/wallabag/"
  :init
  (autoload 'wallabag "wallabag")
  :config
  (setq wallabag-host "https://xx.xx.xx") ;; wallabag server host name
  (setq wallabag-username "xx") ;; username
  (setq wallabag-password "xx") ;; password
  (setq wallabag-clientid "xx") ;; created with API clients management
  (setq wallabag-secret "xx") ;; created with API clients management
  ;; (setq wallabag-db-file "~/OneDrive/Org/wallabag.sqlite") ;; optional, default is saved to ~/.emacs.d/.cache/wallabag.sqlite
  ;; (run-with-timer 0 3540 'wallabag-request-token) ;; optional, auto refresh token, token should refresh every hour
  )
