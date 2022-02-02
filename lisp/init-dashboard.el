;;; init-dashboard.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes dashboard
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))


(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)

(setq dash/banner (concat "images/" (car (shuffle '("sharkCantBreath.png"
                                                       "huaji.png"
                                                       "gua.png"
                                                       "watermelon.png")))))


;; DashboardPac
(use-package dashboard
  :demand
  :diminish (dashboard-mode page-break-lines-mode)
  :bind
  (("C-c d" . open-dashboard)
   :map dashboard-mode-map
   (("n" . dashboard-next-line)
    ("p" . dashboard-previous-line)
    ("N" . dashboard-next-section)
    ("F" . dashboard-previous-section)))
  :custom
  (dashboard-banner-logo-title "Welcome to Emacs!")
  (dashboard-startup-banner (expand-file-name dash/banner user-emacs-directory))
  (dashboard-items '((recents  . 7)
                     (bookmarks . 7)
                     (agenda . 5)
                     (projects . 5)))
  (initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   (if (featurep 'all-the-icons)
       `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust -0.05)
           "Bitnut" "Browse my Homepage"
           (lambda (&rest _) (browse-url "https://github.com/Bitnut/.emacs.d")))
          (,(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1)
           "Configuration" "" (lambda (&rest _) (edit-configs)))
          ))
     `((("" "Bitnut" "Browse my Homepage"
         (lambda (&rest _) (browse-url "https://github.com/Bitnut/.emacs.d")))
        ("" "Configuration" "" (lambda (&rest _) (edit-configs)))
        ))))
  :config
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (dashboard-setup-startup-hook)
  ;; Open Dashboard function
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (delete-other-windows)))
;; -DashboardPac

;; tang poem start
(defvar poem-file "~/.emacs.d/.poem.json")
(defvar poem-cache nil)

(defun poem-update ()
  "Download poem from `jinrishici.com`"
  (let ((url-request-extra-headers
	 '(("X-User-Token" . "1mKLjzllYduP0s/rFGTVQDxGwl47+x4J"))))
    (ignore-errors
      (url-retrieve
       "https://v2.jinrishici.com/sentence"
       (lambda (status)
	 (write-region url-http-end-of-headers (point-max) poem-file)))))
  (setq poem-cache nil))

(defun poem-get (prop)
  "Get poem from cache file, PROP can be 'content, 'origin"
  (ignore-errors
    (if poem-cache
        (alist-get prop poem-cache)
      (with-temp-buffer
        (insert-file-contents poem-file)
        (let ((data (alist-get 'data (json-read))))
          (setq poem-cache data)
          (alist-get prop data))))))

(defun poem-get-formatted ()
  (let* ((poem (poem-get 'origin))
         (lines (alist-get 'content poem))
         (content (mapconcat #'identity lines "\n")))
    (format "%s\n%s · %s\n%s"
            (alist-get 'title poem)
            (alist-get 'dynasty poem)
            (alist-get 'author poem)
            content)))


(advice-add #'dashboard-refresh-buffer :after #'poem-update)

(defun dashboard-poem (list-size)
   (insert (poem-get-formatted)))
(add-to-list 'dashboard-item-generators '(poem . dashboard-poem))
(add-to-list 'dashboard-items '(poem) t)
;; tang poem end


;; PBLPac
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
;; -PBLPac

(provide 'init-dashboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
