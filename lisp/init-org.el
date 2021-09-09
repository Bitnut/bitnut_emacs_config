;; 设置默认 Org Agenda 文件目录

(require 'init-const)

(use-package org
  :ensure nil
  :commands (org-dynamic-block-define)
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
        my/org-directory my/org-directory
        org-capture-templates
        `(("i" "Idea" entry (file ,(concat my/org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat my/org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat my/org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                                ,(concat my/org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("b" "Book" entry (,(if emacs/>=26p 'file+olp+datetree 'file+datetree)
                             ,(concat my/org-directory "/book.org"))
	       "* Topic: %^{Description}  %^g %? Added: %U"))
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-agenda-files '("~/org")
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (and (display-graphic-p) (char-displayable-p ?⏷)) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)
  ;; Prettify UI
  (when emacs/>=26p
    (use-package org-superstar
      :if (and (display-graphic-p) (char-displayable-p ?⚫))
      :hook (org-mode . org-superstar-mode)
      :init (setq org-superstar-headline-bullets-list '("⚫" "⚫" "⚫" "⚫"))))
  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-mode-map
                ("C-c C-x m" . org-pomodoro))
    :init
    (with-eval-after-load 'org-agenda
      (bind-keys :map org-agenda-mode-map
                 ("K" . org-pomodoro)
                 ("C-c C-x m" . org-pomodoro)))))

;; indent of org
(setq org-startup-indented t)

;; r represents remember
(global-set-key (kbd "C-c r") 'org-capture)
(provide 'init-org)
