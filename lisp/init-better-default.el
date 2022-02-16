(setq make-backup-files nil)

;; cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

(setq ring-bell-function 'ignore)

(global-auto-revert-mode)

(delete-selection-mode 1)

(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

(defun remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

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

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(electric-pair-mode 1)

(setq-default smerge-command-prefix "\C-cv")

(setq dired-dwim-target t)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(global-set-key (kbd "C-c c c") 'compile)

(provide 'init-better-default)
