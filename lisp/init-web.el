(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.vue\\'" "\\.php\\'" "\\.tpl\\'")
  :config
  (setq web-mode-markup-indent-offset  4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'")))
  (setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("php"  . "\\.tpl\\.php\\'"))
      )
  (use-package company-web
    :ensure t)
  (add-hook 'web-mode-hook (lambda()
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup))
                                   )))
  )

;;
;; html
;;
(defun my/web-html-setup()
  "Setup for web-mode html files."
  (message "web-mode use html related setup")
  (add-to-list (make-local-variable 'company-backends)
               '(company-web-html company-files company-css company-capf company-dabbrev))
  ;; (add-hook 'before-save-hook #'sgml-pretty-print)

  )


;;
;; web-mode for vue
;;
(defun my/web-vue-setup()
  "Setup for js related."
  (message "web-mode use vue related setup")
  ;; (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-to-list (make-local-variable 'company-backends)
               '(company-tide company-web-html company-css company-files))
  )

;; vue config for eglot
;; (define-derived-mode bitnut-vue-mode web-mode "customVue"
;;   "A major mode derived from web-mode, for editing .vue files with LSP support.")
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . bitnut-vue-mode))
;; (add-hook 'bitnut-vue-mode-hook #'eglot-ensure)
;; (add-to-list 'eglot-server-programs '(bitnut-vue-mode "vls"))



;;
;; eslint use local
;;
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint)
      (setq flycheck-javascript-standard-executable "")
      )))

(defun my/use-global-eslint ()
  "Use global eslint"
  (setq flycheck-javascript-eslint-executable   "/usr/lib/node_modules/eslint/bin/eslint.js")
  (setq flycheck-eslintrc "/home/picher/.emacs.d/js/eslintrc.js")
  (setq exec-path (append exec-path '("/usr/lib/node_modules/eslint")))
  )

(defun my/use-validate-eslint ()
  "Use validate eslint"
  (setq flycheck-javascript-eslint-executable   "/mnt/e/projects/reolink-public/node_modules/eslint/bin/eslint.js")
  (setq flycheck-eslintrc "/mnt/e/projects/reolink-public/.eslintrc.js"))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 css                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (add-hook 'css-mode-hook (lambda()
                             (add-to-list (make-local-variable 'company-backends)
					  '(company-css company-files company-yasnippet company-capf))))
  (setq css-indent-offset 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                emmet                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda()
                              (setq emmet-indent-after-insert t)))

  )

(use-package mode-local
  :ensure t
  :config
  (setq-mode-local rjsx-mode emmet-expand-jsx-className? t)
  (setq-mode-local web-mode emmet-expand-jsx-className? t)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                  json               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package json-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                  js                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package js2-mode
  :ensure t
  :init
  (setq js2-basic-offset 4)
  (setq js-indent-level 4)
  (setq js2-global-externs '("module" "require" "assert" "setInterval" "console" "__dirname__") )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              typescript             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-tide-mode ()
  "Setup tide mode for other mode."
  (interactive)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              php                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package php-mode
;;   :ensure t
;;   :mode ("\\.php\\'")
;;   )

;; (add-hook 'js2-mode-hook 'eglot-ensure)
;; (add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)

;; flymake-keybindings
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)



(use-package go-mode
  :ensure t
  :mode ("\\.go\\'")
  )

(provide 'init-web)
;;; web.el ends here
