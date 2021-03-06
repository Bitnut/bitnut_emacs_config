;; (require 'pyim)
;; (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;; (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;; (setq default-input-method "pyim")

(defun pyim-probe-meow-normal ()
  "用于解决 meow 和 pyim 的冲突问题"
  (and (boundp 'meow-normal-mode) meow-normal-mode))

(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")
  ;; 默认使用双拼
  (setq pyim-default-scheme 'pyim-shuangpin)
  (setq pyim-assistant-scheme 'pyim-quanpin)
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-meow-normal))

  ;; 开启拼音搜索功能
  ;; (pyim-isearch-mode 1)

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  :bind
  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ;; ("C-;" . pyim-delete-word-from-personal-buffer)
   ))

(global-set-key (kbd "C-\\") 'toggle-input-method)


(defun pyim-go-quanpin ()
  "change to quanpin method"
  (interactive)
  (setq pyim-default-scheme 'pyim-quanpin))

(defun pyim-go-shuangpin ()
  "change to quanpin method"
  (interactive)
  (setq pyim-default-scheme 'pyim-shuangpin))


(provide 'init-pyim)
