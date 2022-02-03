;; swiper counsel

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (
         ("C-s" . 'swiper-isearch)
         ("C-r"   . swiper-isearch-backward)

         ("M-x" . 'counsel-M-x)
         ("M-s i" . 'counsel-imenu)
         ("C-c c n" . 'counsel-fzf)
         ("C-c C-r" . 'ivy-resume)
         ("C-c c s" . 'counsel-rg)
         ("C-c c g" . 'counsel-git)
         ("C-c c j" . 'counsel-git-grep)
         ("C-x C-f" . 'counsel-find-file)

         ("C-c v p" . 'ivy-push-view)
         ("C-c v o" . 'ivy-pop-view)
         ("C-c v ." . 'ivy-switch-view))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers)
  (setq ivy-height 12
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-fixed-height-minibuffer t)
  )

(use-package ivy-yasnippet
  :bind ("C-c C-y" . ivy-yasnippet))

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
