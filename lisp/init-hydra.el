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
         ;; ("C-n" . 'hydra-move/body)
         ("M-m" . 'hydra-move-text/body))
  :init
  (defhydra hydra-zoom (:idle 2.0)
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  (defhydra hydra-move-text (:idle 2.0)
    "Move text"
    ("u" move-text-up "up")
    ("d" move-text-down "down"))
  (defhydra hydra-move (:idle 6.0 :body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom))
  (defhydra hydra-goto-line (goto-map ""
                                      :pre (linum-mode 1)
                                      :post (linum-mode -1)
                                      :idle 2.0)
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))
  )

(provide 'init-hydra)
