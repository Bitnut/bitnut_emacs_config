(setq make-backup-files nil)

(setq-default cursor-type 'bar)

(global-auto-revert-mode)

(delete-selection-mode 1)


;; recentf-mode 配置
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 25)


(provide 'init-better-default)
