;; 设置默认 Org Agenda 文件目录

(with-eval-after-load 'org
  (setq org-agenda-files '("~/Work/org"))
  (setq org-src-fontify-natively t)
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "工作安排")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)))
  )


;; r represents remember
(global-set-key (kbd "C-c r") 'org-capture)
(provide 'init-org)
