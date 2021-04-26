;; (use-package dap-mode
;;   :ensure
;;   :bind
;;   (("<f5>" . dap-debug))
;;   :hook
;;   ((typescript-mode . dap-mode)
;;    (typescript-mode . dap-ui-mode))
;;   :config
;;   (use-package dap-node))

;; ;;;###autoload
;; (defun +dap-debug-a (&rest _)
;;   (progn (dap-ui-locals)
;;          (dap-ui-sessions)
;;          (dap-ui-repl)))

;; (advice-add #'dap-debug :after #'+dap-debug-a)
