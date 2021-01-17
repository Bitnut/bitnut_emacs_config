;; 设置默认 Org Agenda 文件目录

(with-eval-after-load 'org
  (setq org-agenda-files '("~/org"))
  (setq org-src-fontify-natively t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "工作安排")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
          ("b" "Todo" entry (file+headline "~/org/gtd.org" "博客任务")
	 "* TODO [#A] %?\n  %i\n"
	 :empty-lines 1))
        ))

;; indent of org
(setq org-startup-indented t)

;; r represents remember
(global-set-key (kbd "C-c r") 'org-capture)
(provide 'init-org)
