(require 'rime)

(setq rime-emacs-module-header-root "/opt/emacs/include/")

;;; Code:
(setq rime-user-data-dir "~/.config/fcitx/rime")

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(provide 'init-rime)
