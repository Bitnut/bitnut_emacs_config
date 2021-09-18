(setq
 ;; no frame resize when setting font, menu bar, tool bar, tab bar, internal borders, fringes or scroll bars
 frame-inhibit-implied-resize t
 ;; use simpliest major mode
 initial-major-mode 'fundamental-mode
 ;; Disable package.el in favor of straight.el
 package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
