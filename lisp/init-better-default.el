(setq make-backup-files nil)

;; cursor
(setq-default cursor-type 'bar)

(setq ring-bell-function 'ignore)

(global-auto-revert-mode)

(delete-selection-mode 1)


;; recentf-mode 配置
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 25)




(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

;; (defun remove-dos-eol ()
;;   "Replace DOS eolns CR LF with Unix eolns CR"
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (search-forward "\r" nil t) (replace-match "")))

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)


;; region/ background / foreground color config
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;; move through camel words
(global-subword-mode 1)

;; encode setting
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


(setq-default default-tab-width 4)
(setq-default indent-tabs-mode nil)


;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)

;; (when (display-graphic-p)
;;   (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;         mouse-wheel-progressive-speed nil))
;; (setq scroll-step 1
;;       scroll-margin 0
;;       scroll-conservatively 100000)

(setq-default smerge-command-prefix "\C-cv")

(provide 'init-better-default)
