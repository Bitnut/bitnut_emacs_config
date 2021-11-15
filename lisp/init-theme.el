;; theme here

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  )


(defvar bitnut-theme-status "init")

(defun bitnut-theme-load-light ()
  "load customize light theme"
  ;; chose theme used in daytime
  (modus-themes-load-operandi)
  ;; (load-theme 'spacemacs-light t)
  (setq bitnut-theme-status "light")
  ;; (awesome-tray-mode 1)
  ;; (awesome-tab-mode t)
  )

(defun bitnut-theme-load-dark ()
  "load customize night theme"
  ;; chose theme used in night
  (modus-themes-load-vivendi)
  ;; (load-theme 'spacemacs-dark t)
  ;; (setq bitnut-theme-status "dark")
  ;; (awesome-tray-mode 1)
  ;; (awesome-tab-mode t)
  )

(bitnut-theme-load-dark)

(defun bitnut-theme-is-day ()
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (and (> current-hour 7)
         (< current-hour 18))))

(defun bitnut-theme-load ()
  "check if its during the day, then load theme"
  (if (bitnut-theme-is-day)
      (when (or (string-equal bitnut-theme-status "init")
		(string-equal bitnut-theme-status "dark"))
        (bitnut-theme-load-light))
    (when (or (string-equal bitnut-theme-status "init")
              (string-equal bitnut-theme-status "light"))
      (bitnut-theme-load-dark))))

(defun bitnut-auto-load-theme ()
  (run-with-timer 0 (* 30 60) 'bitnut-theme-load))

(defun bitnut-renew-hl-todo ()
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#00FF00")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("DONE"   . "#1E90FF")))
  (command-execute  'global-hl-todo-mode)
  )

(bitnut-auto-load-theme)

(add-hook 'after-load-theme-hook
          (lambda ()
            (awesome-tray-mode 1)
            ;; (sort-tab-mode 1)
            ;; (global-set-key (kbd "<C-iso-lefttab>") 'sort-tab-select-prev-tab)
            ;; (global-set-key (kbd "<C-tab>") 'sort-tab-select-next-tab)
            ;; (global-set-key (kbd "s-1") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-2") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-3") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-4") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-5") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-6") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-7") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-8") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-9") sort-tab-select-visible-tab)
            ;; (global-set-key (kbd "s-0") sort-tab-select-visible-tab)
            ))

;; FIXME disrupt hl-todo face config
;; (add-hook 'after-init-hook 'bitnut-auto-load-theme)


(provide 'init-theme)
