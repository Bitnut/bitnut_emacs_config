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

(use-package hungry-delete
  :defer nil
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")

    ;; Override the default definitions of `hungry-delete-skip-ws-forward' and
    ;; `hungry-delete-skip-ws-backward'; do not delete back-slashes at EOL.
    (defun hungry-delete-skip-ws-forward ()
      "Skip over any whitespace following point.
This function skips over horizontal and vertical whitespace."
      (skip-chars-forward hungry-delete-chars-to-skip)
      (while (get-text-property (point) 'read-only)
        (backward-char)))

    (defun hungry-delete-skip-ws-backward ()
      "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace."
      (skip-chars-backward hungry-delete-chars-to-skip)
      (while (get-text-property (point) 'read-only)
        (forward-char)))

    (defun modi/turn-off-hungry-delete-mode ()
      "Turn off hungry delete mode."
      (hungry-delete-mode -1))

    ;; Enable `hungry-delete-mode' everywhere ..
    (global-hungry-delete-mode)

    ;; Except ..
    ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
    ;; i.e. when editing file names in the *Dired* buffer.
    (add-hook 'wdired-mode-hook #'modi/turn-off-hungry-delete-mode)
    ;; and in minibuffer
    (add-hook 'minibuffer-setup-hook #'modi/turn-off-hungry-delete-mode)))

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
