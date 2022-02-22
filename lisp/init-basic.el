(require 'init-const)
(require 'init-my-funcs)

;; Personal information
(setq user-full-name "Bitnut"
      user-mail-address "940095072@qq.com")

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

;; Encoding
;; UTF-8 as the default coding system
(my-let-it-utf8)

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 200)
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (push "~/org/*" recentf-exclude)
  (push "~/.emacs.d/bookmarks" recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(provide 'init-basic)
