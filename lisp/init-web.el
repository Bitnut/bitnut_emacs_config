(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.vue\\'" "\\.php\\'")
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
  ;; (flycheck-add-mode 'html-tidy 'web-mode)
  ;; (flycheck-select-checker 'html-tidy)
  (add-to-list (make-local-variable 'company-backends)
               '(company-web-html company-files company-css company-capf company-dabbrev))
  (add-hook 'before-save-hook #'sgml-pretty-print)

  )


;;
;; web-mode for vue
;;
(defun my/web-vue-setup()
  "Setup for js related."
  (message "web-mode use vue related setup")
  (setup-tide-mode)
  (prettier-js-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-select-checker 'javascript-eslint)
  (my/use-eslint-from-node-modules)
  (add-to-list (make-local-variable 'company-backends)
               '(comany-tide company-web-html company-css company-files))
  )


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

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 rjsx                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package rjsx-mode
;;   :ensure t
;;   :mode ("\\.js\\'")
;;   :config
;;   (setq js2-basic-offset 2)
;;   (add-hook 'rjsx-mode-hook (lambda()
;;                               (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;;                               (my/use-eslint-from-node-modules)
;;                               (flycheck-select-checker 'javascript-eslint)
;;                               ))
;;   (setq js2-basic-offset 2)
;;   )

;; (use-package react-snippets
;;   :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 css                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package css-mode
;;   :ensure t
;;   :mode "\\.css\\'"
;;   :config
;;   (add-hook 'css-mode-hook (lambda()
;;                              (add-to-list (make-local-variable 'company-backends)
;;                                           '(company-css company-files company-yasnippet company-capf))))
;;   (setq css-indent-offset 2)
;;   (setq flycheck-stylelintrc "~/.stylelintrc")
;;   )


;; (use-package scss-mode
;;   :ensure t
;;   :mode "\\scss\\'"
;;   )


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
                                        ;                  js                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package js2-mode
  :ensure t
  :mode (("\\.json\\'" . javascript-mode))
  :init
  (setq indent-tabs-mode nil)
  (setq js2-basic-offset 4)
  (setq js-indent-level 4)
  (setq js2-global-externs '("module" "require" "assert" "setInterval" "console" "__dirname__") )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              typescript             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Javascript, Typescript and Flow support for lsp-mode
;;
;; Install:
;;
;; npm install -g typescript
;; npm install -g typescript-language-server
;;
;; Fixed error "[tsserver] /bin/sh: /usr/local/Cellar/node/10.5.0_1/bin/npm: No such file or directory" :
;; 
;; sudo ln -s /usr/local/bin/npm /usr/local/Cellar/node/10.5.0_1/bin/npm
;;
;; (add-hook 'js-mode-hook #'lsp-typescript-enable)
;; (add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support
;; (add-hook 'js3-mode-hook #'lsp-typescript-enable) ;; for js3-mode support
;; (add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support

;; (defun lsp-company-transformer (candidates)
;;   (let ((completion-ignore-case t))
;;     (all-completions (company-grab-symbol) candidates)))

;; (defun lsp-js-hook nil
;;   (make-local-variable 'company-transformers)
;;   (push 'lsp-company-transformer company-transformers))

;; (add-hook 'js-mode-hook 'lsp-js-hook)

(defun setup-tide-mode ()
  "Setup tide mode for other mode."
  (interactive)
  (message "setup tide mode")
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'rjsx-mode-hook #'setup-tide-mode)



(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  ;;(before-save . tide-format-before-save))
  :config
  (setq tide-completion-enable-autoimport-suggestions t)
  )

(use-package prettier-js
  :ensure t
  :hook (;;(js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '(
                           "--bracket-spacing" "false"
			   "--trailing-comma" "all"
			   "--single-quote" "true"
			   "--print-width" "120"
			   "--tab-width" "4"
			   "--jsx-bracket-same-line" "true"
                           ))
  )


(provide 'init-web)
;;; web.el ends here
