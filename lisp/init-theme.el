;; theme here

(use-package modus-themes
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
  (modus-themes-load-operandi)
  (setq bitnut-theme-status "light")
  )

(defun bitnut-theme-load-dark ()
  "load customize night theme"
  (modus-themes-load-vivendi)
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
      (bitnut-theme-load-dark)))
  (awesome-tray-mode 1)
  (bitnut-renew-hl-todo)
  )

(defun bitnut-auto-load-theme ()
  (run-with-timer 0 (* 30 60) 'bitnut-theme-load))

(defun bitnut-renew-hl-todo ()
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#00FF00")
          ("todo"   . "#00FF00")
          ("FIXME"  . "#FF0000")
          ("fixme"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("note" . "#FF4500")
          ("GOTCHA" . "#FF4500")
          ("DONE"   . "#1E90FF")
          ("done"   . "#1E90FF")))
  (global-hl-todo-mode 1)
  )
(add-hook 'emacs-startup-hook 'bitnut-auto-load-theme)

(provide 'init-theme)
