(require 'tide)

(dolist (hook (list
               'js2-mode-hook
               'rjsx-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook (lambda ()
                   ;; 初始化 tide
		   (tide-setup)
		   ;; 当 tsserver 服务没有启动时自动重新启动
		   (unless (tide-current-server)
		     (tide-restart-server))
                   )))

(provide 'init-tide)
