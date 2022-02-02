;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
;; expand-region. = to expand, - to contract, 0 to reset
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after ivy
    :bind (:map flyspell-mode-map
           ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
    :init (setq flyspell-correct-interface #'flyspell-correct-ivy)))

;; hungry
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Flexible text folding
;; (use-package origami
;;   :pretty-hydra
;;   ((:title (pretty-hydra-title "Origami" 'octicon "fold" :height 1.1 :v-adjust -0.05)
;;     :color amaranth :quit-key "q")
;;    ("Node"
;;     ((";" origami-recursively-toggle-node "toggle recursively")
;;      ("a" origami-toggle-all-nodes "toggle all")
;;      ("t" origami-toggle-node "toggle current")
;;      ("o" origami-open-node "open current")
;;      ("c" origami-close-node "close current")
;;      ("s" origami-show-only-node "only show current"))
;;     "Actions"
;;     (("u" origami-undo "undo")
;;      ("d" origami-redo "redo")
;;      ("r" origami-reset "reset")
;;      ("n" origami-next-fold "next fold")
;;      ("p" origami-previous-fold "previous fold"))))
;;   :bind (:map origami-mode-map
;;          ("C-~" . origami-hydra/body))
;;   :hook (prog-mode . origami-mode)
;;   :init (setq origami-show-fold-header t)
;;   :config (face-spec-reset-face 'origami-fold-header-face))



(provide 'init-edit)
