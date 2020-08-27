;; code here

(use-package lsp-mode
     :defines (lsp-clients-python-library-directories
               lsp-rust-server)
     :commands (lsp-enable-which-key-integration
                lsp-format-buffer
                lsp-organize-imports)
     :diminish
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                             (lsp-deferred))))
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          ;; Format and organize imports
                          (unless (apply #'derived-mode-p centaur-lsp-format-on-save-ignore-modes)
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
     :init
     ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
     (setq read-process-output-max (* 1024 1024)) ;; 1MB

     (setq lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-signature-auto-activate nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil

           lsp-enable-file-watchers nil
           lsp-enable-file-watchers nil
           lsp-enable-folding nil
           lsp-enable-semantic-highlighting nil
           lsp-enable-symbol-highlighting nil
           lsp-enable-text-document-color nil

           lsp-enable-indentation nil
           lsp-enable-on-type-formatting nil)

     ;; For `lsp-clients'
     (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     (when (executable-find "rust-analyzer")
       (setq lsp-rust-server 'rust-analyzer))
     :config
     (with-no-warnings
       (defun my-lsp--init-if-visible (func &rest args)
         "Not enabling lsp in `git-timemachine-mode'."
         (unless (bound-and-true-p git-timemachine-mode)
           (apply func args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)))


(use-package lsp-ui
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :pretty-hydra
     ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket")
       :color amaranth :quit-key "q")
      ("Doc"
       (("d e" (progn
                 (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                 (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
         "enable" :toggle lsp-ui-doc-mode)
        ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
         "signature" :toggle lsp-ui-doc-include-signature)
        ("d t" (setq lsp-ui-doc-position 'top)
         "top" :toggle (eq lsp-ui-doc-position 'top))
        ("d b" (setq lsp-ui-doc-position 'bottom)
         "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
        ("d p" (setq lsp-ui-doc-position 'at-point)
         "at point" :toggle (eq lsp-ui-doc-position 'at-point))
        ("d f" (setq lsp-ui-doc-alignment 'frame)
         "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
        ("d w" (setq lsp-ui-doc-alignment 'window)
         "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
       "Sideline"
       (("s e" (progn
                 (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
                 (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
         "enable" :toggle lsp-ui-sideline-mode)
        ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
         "hover" :toggle lsp-ui-sideline-show-hover)
        ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
         "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
        ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
         "symbol" :toggle lsp-ui-sideline-show-symbol)
        ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
         "code actions" :toggle lsp-ui-sideline-show-code-actions)
        ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
         "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
       "Action"
       (("h" backward-char "←")
        ("j" next-line "↓")
        ("k" previous-line "↑")
        ("l" forward-char "→")
        ("C-a" mwim-beginning-of-code-or-line nil)
        ("C-e" mwim-end-of-code-or-line nil)
        ("C-b" backward-char nil)
        ("C-n" next-line nil)
        ("C-p" previous-line nil)
        ("C-f" forward-char nil)
        ("M-b" backward-word nil)
        ("M-f" forward-word nil)
        ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
     :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body)
            ("M-RET" . lsp-ui-sideline-apply-code-actions))
     :hook (lsp-mode . lsp-ui-mode)
     :init (setq lsp-ui-sideline-show-diagnostics nil
                 lsp-ui-sideline-ignore-duplicate t
                 lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
                 lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                       ,(face-foreground 'font-lock-string-face)
                                       ,(face-foreground 'font-lock-constant-face)
                                       ,(face-foreground 'font-lock-variable-name-face)))
     :config
     ;; `C-g'to close doc
     (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

     ;; Reset `lsp-ui-doc-background' after loading theme
     (add-hook 'after-load-theme-hook
               (lambda ()
                 (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
                 (set-face-background 'lsp-ui-doc-background
                                      (face-background 'tooltip)))))

(provide 'init-lsp)
