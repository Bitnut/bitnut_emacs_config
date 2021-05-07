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
            (typescript-mode-hook . lsp)
            (lsp-mode . lsp-enable-which-key-integration))
     :bind (("M-n" . flycheck-next-error)
            ("M-p" . flycheck-previous-error)
            :map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point))
     :init
     ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
     (setq read-process-output-max (* 1024 1024)) ;; 1MB
     (setq gc-cons-threshold 100000000)
     (setq lsp-idle-delay 0.5)

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
     :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body)
            ("M-RET" . lsp-ui-sideline-apply-code-actions))
     :hook (lsp-mode . lsp-ui-mode)
     :init (setq ;;lsp-ui-sideline-show-diagnostics nil
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

;; (with-eval-after-load 'lsp-mode
;;   ;; enable log only for debug
;;   (setq lsp-log-io nil)
;;   ;; handle yasnippet by myself
;;   (setq lsp-enable-snippet nil)
;;   ;; auto restart lsp
;;   (setq lsp-restart 'auto-restart)
;;   ;; don't watch 3rd party javascript libraries
;;   (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\|awk\\)$" lsp-file-watch-ignored)
;;   ;; don't ping LSP language server too frequently
;;   (defvar lsp-on-touch-time 0)
;;   (defun my-lsp-on-change-hack (orig-fun &rest args)
;;     ;; do NOT run `lsp-on-change' too frequently
;;     (when (> (- (float-time (current-time))
;;                 lsp-on-touch-time) 120) ;; 2 mins
;;       (setq lsp-on-touch-time (float-time (current-time)))
;;       (apply orig-fun args)))
;;   (advice-add 'lsp-on-change :around #'my-lsp-on-change-hack))

(provide 'init-lsp)
