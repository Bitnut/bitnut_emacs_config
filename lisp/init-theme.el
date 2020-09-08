

;; theme here
(require 'awesome-tray)
(require 'awesome-tab)



(defvar bitnut-theme-status "init")

(defun bitnut-theme-load-light ()
  ;; chose theme used in daytime
  (load-theme 'spacemacs-light t)
  (setq bitnut-theme-status "light")
  ;; (awesome-tray-mode 1)
  ;; (awesome-tab-mode t)
  )

(defun bitnut-theme-load-dark ()
  ;; chose theme used in night
  (load-theme 'spacemacs-dark t)
  (setq bitnut-theme-status "dark")
  ;; (awesome-tray-mode 1)
  ;; (awesome-tab-mode t)
  )

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

(bitnut-auto-load-theme)



(provide 'init-theme)
