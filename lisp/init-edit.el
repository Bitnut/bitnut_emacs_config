;; Edit multiple regions in the same way simultaneously

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


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
;; (use-package flyspell
;;   :ensure nil
;;   :diminish
;;   :if (executable-find "aspell")
;;   :hook (((text-mode outline-mode) . flyspell-mode)
;;          (prog-mode . flyspell-prog-mode)
;;          (flyspell-mode . (lambda ()
;;                             (dolist (key '("C-;" "C-," "C-."))
;;                               (unbind-key key flyspell-mode-map)))))
;;   :init (setq flyspell-issue-message-flag nil
;;               ispell-program-name "aspell"
;;               ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
;;   :config
;;   ;; Correcting words with flyspell via Ivy
;;   (use-package flyspell-correct-ivy
;;     :after ivy
;;     :bind (:map flyspell-mode-map
;;            ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
;;     :init (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config
  (progn
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

    ;; Except ..
    ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
    ;; i.e. when editing file names in the *Dired* buffer.
    (add-hook 'wdired-mode-hook #'modi/turn-off-hungry-delete-mode)
    ;; and in minibuffer
    (add-hook 'minibuffer-setup-hook #'modi/turn-off-hungry-delete-mode)))

;; A modern Packages Menu
(use-package paradox
  :hook (after-init . paradox-enable)
  :init (setq paradox-execute-asynchronously t
              paradox-github-token t
              paradox-display-star-count nil)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                "Display `page-break-lines' in \"*Paradox Report*\"."
                (let ((buf (get-buffer "*Paradox Report*"))
                      (inhibit-read-only t))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (page-break-lines-mode 1)))))
              t)))

;; smartparens
(use-package smartparens
  :hook (after-init . smartparens-global-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t))

;; popwin
(use-package popwin
  :hook (after-init . popwin-mode)
  :config
  (setq popwin:popup-window-position 'right
        popwin:popup-window-width 60))

(use-package all-the-icons)

;; keyfreq
(use-package keyfreq
  :hook (after-init . keyfreq-mode)
  :config
  (keyfreq-autosave-mode 1))

;; (require 'keyfreq)
;; (keyfreq-mode 1)

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Flexible text folding
(use-package origami
  :pretty-hydra
  ((:title (pretty-hydra-title "Origami" 'octicon "fold" :height 1.1 :v-adjust -0.05)
    :color amaranth :quit-key "q")
   ("Node"
    ((";" origami-recursively-toggle-node "toggle recursively")
     ("a" origami-toggle-all-nodes "toggle all")
     ("t" origami-toggle-node "toggle current")
     ("o" origami-open-node "open current")
     ("c" origami-close-node "close current")
     ("s" origami-show-only-node "only show current"))
    "Actions"
    (("u" origami-undo "undo")
     ("d" origami-redo "redo")
     ("r" origami-reset "reset")
     ("n" origami-next-fold "next fold")
     ("p" origami-previous-fold "previous fold"))))
  :bind (:map origami-mode-map
         ("C-~" . origami-hydra/body))
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))



(provide 'init-edit)
