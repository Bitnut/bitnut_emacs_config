;; swiper counsel
(ivy-mode 1)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c s") 'ivy-switch-view)


(use-package ivy-yasnippet
  :bind ("C-c C-y" . ivy-yasnippet))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
    :hook (ivy-mode . all-the-icons-ivy-rich-mode)
    :config
    (plist-put all-the-icons-ivy-rich-display-transformers-list
               'centaur-load-theme;; example of a command that works on current word or text selection
               (defun down-case-word-or-region ()
                 "Lower case the current word or text selection."
                 (interactive)
                 (let (pos1 pos2 meat)
                   (if (and transient-mark-mode mark-active)
                       (setq pos1 (region-beginning)
                             pos2 (region-end))
                     (setq pos1 (car (bounds-of-thing-at-point 'symbol))
                           pos2 (cdr (bounds-of-thing-at-point 'symbol))))

                                        ; now, pos1 and pos2 are the starting and ending positions
                                        ; of the current word, or current text selection if exists

                   ;; put your code here.
                   
                   ;; Some example of things you might want to do
                   (downcase-region pos1 pos2) ; example of a func that takes region as args
                   (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.
                   (delete-region pos1 pos2) ; get rid of it
                   (insert "newText") ; insert your new text

                   )
                 )
               '(:columns
                 ((all-the-icons-ivy-rich-theme-icon)
                  (ivy-rich-candidate))
                 :delimiter "\t"))
    (all-the-icons-ivy-rich-reload))
;; (when (icons-displayable-p)
;;   )


;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook ((projectile-mode . ivy-rich-mode) ; MUST after `counsel-projectile'
         (ivy-rich-mode . ivy-rich-project-root-cache-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(provide 'init-ivy)
