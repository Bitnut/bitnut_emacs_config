;; code here

(use-package lsp-mode
  :commands (lsp-enable-which-key-integration)
  :diminish
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (js2-mode . lsp)
         (js-mode . lsp)
         (json-mode . lsp)
         (php-mode . lsp)
         (typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         ((c-mode c++-mode objc-mode cuda-mode) .
          (lambda () (require 'ccls) (lsp))))
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)
         :map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 3)
  (setq ccls-executable "~/languageServer/ccls/Release/ccls")
  (setq lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-log-io nil
        lsp-enable-semantic-highlighting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  :config
  (with-no-warnings
    (defun my-lsp--init-if-visible (func &rest args)
      "Not enabling lsp in `git-timemachine-mode'."
      (unless (bound-and-true-p git-timemachine-mode)
        (apply func args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)))


;; (use-package lsp-ui
;;   :custom-face
;;   (lsp-ui-sideline-code-action ((t (:inherit warning))))
;;   :bind (("C-c u" . lsp-ui-imenu)
;;          :map lsp-ui-mode-map
;;          ("M-<f6>" . lsp-ui-hydra/body)
;;          ("M-RET" . lsp-ui-sideline-apply-code-actions))
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :init (setq
;;          lsp-eldoc-enable-hover nil
;;          lsp-ui-sideline-enable nil
;;          lsp-modeline-diagnostics-enable t
;;          lsp-ui-doc-mode nil
;;          lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
;;          lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
;;                                ,(face-foreground 'font-lock-string-face)
;;                                ,(face-foreground 'font-lock-constant-face)
;;                                ,(face-foreground 'font-lock-variable-name-face)))
;;   :config
;;   ;; `C-g'to close doc
;;   (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

;;   ;; Reset `lsp-ui-doc-background' after loading theme
;;   (add-hook 'after-load-theme-hook
;;             (lambda ()
;;               (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
;;               (set-face-background 'lsp-ui-doc-background
;;                                    (face-background 'tooltip)))))

(when emacs/>=26p
  (use-package dap-mode
    :bind (:map lsp-mode-map
                ("<f5>" . dap-debug)
                ("M-<f5>" . dap-hydra))
    :init
    :config
    (require 'dap-node)
    :hook (
           (after-init . dap-auto-configure-mode)
           ;; (dap-stopped . (lambda (_args) (dap-hydra)))
           ;; (dap-terminated . (lambda (_args) (dap-hydra/nil)))
           )
    ))

(provide 'init-lsp)
