(use-package pretty-hydra
  :bind ("<f2>" . toggles-hydra/body)
  :init
  (pretty-hydra-define toggles-hydra (:title "Awesome Hydra!" :color amaranth :quit-key "q")
    ("Basic"
     (("n" linum-mode "line number" :toggle t)
      ("w" whitespace-mode "whitespace" :toggle t)
      ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
      ("L" page-break-lines-mode "page break lines" :toggle t))
     "Highlight"
     (("s" symbol-overlay-mode "symbol" :toggle t)
      ("l" hl-line-mode "line" :toggle t)
      ("x" highlight-sexp-mode "sexp" :toggle t)
      ("t" hl-todo-mode "todo" :toggle t))
     "Coding"
     (("p" smartparens-mode "smartparens" :toggle t)
      ("P" smartparens-strict-mode "smartparens strict" :toggle t)
      ("S" show-smartparens-mode "show smartparens" :toggle t)
      ("f" flycheck-mode "flycheck" :toggle t))
     "Emacs"
     (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
  )

(use-package hydra
  :bind (("<f4>" . 'hydra-zoom/body)
         ("M-m" . 'hydra-move-text/body))
  :init
  (defhydra hydra-zoom (:idle 1.0)
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out"))
  (defhydra hydra-move-text (:idle 1.0)
    "Move text"
    ("j" move-text-up "up")
    ("k" move-text-down "down"))
  )

(provide 'init-hydra)
