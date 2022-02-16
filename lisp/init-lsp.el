;; code here
(require 'init-func)

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
         (web-mode . lsp)
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
  (setq lsp-idle-delay 3)
  (setq ccls-executable "~/languageServer/ccls/Release/ccls")
  (setq lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        ;; lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-log-io nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  :config
  (add-to-list 'lsp-language-id-configuration '(web-mode . "html"))
  (with-no-warnings
    (defun my-lsp--init-if-visible (func &rest args)
      "Not enabling lsp in `git-timemachine-mode'."
      (unless (bound-and-true-p git-timemachine-mode)
        (apply func args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible))
  (use-package lsp-ui
       :custom-face
       (lsp-ui-sideline-code-action ((t (:inherit warning))))
       :pretty-hydra
       ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
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
          ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
           "header" :toggle lsp-ui-doc-header)
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
              ("s-<return>" . lsp-ui-sideline-apply-code-actions)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
       :hook (lsp-mode . lsp-ui-mode)
       :init (setq lsp-ui-sideline-show-diagnostics nil
                   lsp-ui-sideline-ignore-duplicate t
                   lsp-ui-doc-delay 0.1
                   lsp-ui-doc-show-with-cursor t
                   lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
                   lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                         ,(face-foreground 'font-lock-string-face)
                                         ,(face-foreground 'font-lock-constant-face)
                                         ,(face-foreground 'font-lock-variable-name-face)))
       :config
       (with-no-warnings
         ;; Display peek in child frame if possible
         ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
         (defvar lsp-ui-peek--buffer nil)
         (defun lsp-ui-peek--peek-display (fn src1 src2)
           (if (childframe-workable-p)
               (-let* ((win-width (frame-width))
                       (lsp-ui-peek-list-width (/ (frame-width) 2))
                       (string (-some--> (-zip-fill "" src1 src2)
                                 (--map (lsp-ui-peek--adjust win-width it) it)
                                 (-map-indexed 'lsp-ui-peek--make-line it)
                                 (-concat it (lsp-ui-peek--make-footer)))))
                 (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
                 (posframe-show lsp-ui-peek--buffer
                                :string (mapconcat 'identity string "")
                                :min-width (frame-width)
                                :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                                :internal-border-width 1
                                :poshandler #'posframe-poshandler-frame-center))
             (funcall fn src1 src2)))
         (defun lsp-ui-peek--peek-destroy (fn)
           (if (childframe-workable-p)
               (progn
                 (when (bufferp lsp-ui-peek--buffer)
                   (posframe-hide lsp-ui-peek--buffer))
                 (setq lsp-ui-peek--last-xref nil))
             (funcall fn)))
         (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
         (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

         ;; Handle docs
         (defun my-lsp-ui-doc--handle-hr-lines nil
           (let (bolp next before after)
             (goto-char 1)
             (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
               (when (get-text-property next 'markdown-hr)
                 (goto-char next)
                 (setq bolp (bolp)
                       before (char-before))
                 (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                 (setq after (char-after (1+ (point))))
                 (insert
                  (concat
                   (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                   (propertize "\n" 'face '(:height 0.5))
                   (propertize " "
                               ;; :align-to is added with lsp-ui-doc--fix-hr-props
                               'display '(space :height (1))
                               'lsp-ui-doc--replace-hr t
                               'face `(:background ,(face-foreground 'font-lock-comment-face)))
                   ;; :align-to is added here too
                   (propertize " " 'display '(space :height (1)))
                   (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
         (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines))

       ;; Reset `lsp-ui-doc-background' after loading theme
       (add-hook 'after-load-theme-hook
                 (lambda ()
                   (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t))
                   (set-face-background 'lsp-ui-doc-background (face-background 'tooltip nil t)))))
  )

(when emacs/>=26p
  (use-package dap-mode
    :bind (:map lsp-mode-map
                ("<f5>" . dap-debug)
                ("M-<f5>" . dap-hydra))
    :config
    (require 'dap-node)
    :hook (
           (after-init . dap-auto-configure-mode)
           ;; (dap-stopped . (lambda (_args) (dap-hydra)))
           ;; (dap-terminated . (lambda (_args) (dap-hydra/nil)))
           )
    ))

(provide 'init-lsp)
