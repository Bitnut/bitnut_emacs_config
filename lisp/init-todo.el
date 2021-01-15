(setq hl-todo-keyword-faces
      '(("TODO"   . "#00FF00")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("DONE"   . "#1E90FF")))
(use-package hl-todo
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook
  (after-init . global-hl-todo-mode)
  (global-hl-todo . set-todo-faces)

  )

(provide 'init-todo)
