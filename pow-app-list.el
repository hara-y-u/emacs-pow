;;; pow-app-list.el --- pow (http://pow.cx/) app list view component

;; Copyright (c) 2013  Free Software Foundation, Inc.

;; Author: yukihiro hara <yukihr@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;;; Code:

(require 'pow-core)
(require 'tabulated-list)
(eval-when-compile (require 'cl))



;;
;; List View
;;

;; app list mode

(defvar pow-app-list-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Pow App List")))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'pow-app-list-open-app)
    (define-key map "f" 'pow-app-list-find-app-path)
    (define-key map "u" 'pow-app-list-mark-unmark)
    (define-key map "U" 'pow-app-list-mark-unmark-all)
    (define-key map "d" 'pow-app-list-mark-delete)
    (define-key map "r" 'pow-app-list-refresh)
    (define-key map "x" 'pow-app-list-execute)
    (define-key map [menu-bar pow-app-list-menu] (cons "Pow App List" menu-map))
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
                  :help "Quit listing apps"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
                  :help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
                  :help "Previous Line"))
    (define-key menu-map [s2] '("--"))
    (define-key menu-map [mf]
      '(menu-item "Find App Path" pow-app-list-find-app-path
                  :help "Open app path with `find-file'"))
    (define-key menu-map [s3] '("--"))
    (define-key menu-map [mu]
      '(menu-item "Unmark" pow-app-list-mark-unmark
                  :help "Clear any marks on a app and move to the next line"))
    (define-key menu-map [mU]
      '(menu-item "Unmark All" pow-app-list-mark-unmark-all
                  :help "Clear all marks on apps"))
    (define-key menu-map [md]
      '(menu-item "Mark for Deletion" pow-app-list-mark-delete
                  :help "Mark a app for deletion and move to the next line"))
    (define-key menu-map [s4] '("--"))
    (define-key menu-map [mr]
      '(menu-item "Refresh App List" revert-buffer
                  :help "Refresh the list of apps"))
    (define-key menu-map [s5] '("--"))
    (define-key menu-map [mx]
      '(menu-item "Execute Actions" package-menu-execute
                  :help "Perform all the marked actions"))
    map)
  "Local keymap for `pow-app-list-mode' buffers.")

(define-derived-mode pow-app-list-mode tabulated-list-mode "Pow App List"
  "Major mode for browsing a list of pow apps.
Letters do not insert themselves; instead, they are commands.
\\<pow-app-list-mode-map>
\\{pow-app-list-mode-map}"
  (setq tabulated-list-format [("Name" 18 nil)
                               ("Path" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'pow-app-list-refresh nil t)
  (tabulated-list-init-header))

(defmacro pow-app-list-with-proper-buffer (&rest body)
  (declare (indent 0))
  `(if (not (derived-mode-p 'pow-app-list-mode))
       (error "The current buffer is not in Pow App List mode")
     (progn ,@body)))

(defun pow-app-list-open-app (&optional button)
  (interactive)
  (pow-app-list-with-proper-buffer
   (let ((app (if button (button-get button 'app)
                (tabulated-list-get-id))))
     (pow-app-open app))))

(defun pow-app-list-find-app-path (&optional button)
  (interactive)
  (pow-app-list-with-proper-buffer
   (let ((app (if button (button-get button 'app)
                (tabulated-list-get-id))))
     (find-file (pow-app-path app)))))

(defun pow-app-list-mark-unmark (&optional _num)
  (interactive "p")
  (pow-app-list-with-proper-buffer
   (tabulated-list-put-tag " " 'next-line)))

(defun pow-app-list-mark-unmark-all ()
  (interactive)
  (pow-app-list-with-proper-buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (pow-app-list-mark-unmark)))))

(defun pow-app-list-mark-delete (&optional _num)
  (interactive "p")
  (pow-app-list-with-proper-buffer
   (tabulated-list-put-tag "D" 'next-line)))

(defun pow-app-list-refresh ()
  (interactive)
  (pow-app-list-with-proper-buffer
   (pow-list-view-reload pow-app-list--view)
   (pow-list-view-refresh pow-app-list--view)))

(defun pow-app-list-execute (&optional noquery)
  (interactive)
  (pow-app-list-with-proper-buffer
   (let (apps-to-delete cmd app)
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         (setq cmd (char-after))
         (unless (eq cmd ?\s)
           (setq app (tabulated-list-get-id))
           (cond ((eq cmd ?D)
                  (push app apps-to-delete))))
         (forward-line)))
     ;; delete
     (when (and apps-to-delete
                (or noquery
                    (yes-or-no-p "Delete marked apps?")))
       (mapc 'pow-app-delete apps-to-delete)))
   (pow-app-list-refresh)))


;; struct

(defstruct (pow-list-view
            (:constructor nil)
            (:constructor pow-list-view--inner-make))
  apps buffer)

(defun make-pow-list-view (&rest options)
  (let* ((list-view (apply 'pow-list-view--inner-make options))
         (buffer (pow-list-view-create-buffer list-view)))
    list-view))

(defun pow-list-view-create-buffer (list-view)
  (let ((buffer (get-buffer-create "*Pow Apps*")))
    (setf (pow-list-view-buffer list-view) buffer)
    (with-current-buffer buffer
      (defvar-local pow-app-list--view list-view)
      (pow-app-list-mode))
    buffer))

(defun pow-list-view-refresh (list-view)
  (let ((apps (pow-list-view-apps list-view))
        (buffer (pow-list-view-buffer list-view)))
    (with-current-buffer buffer
      (setq tabulated-list-entries
            (mapcar
             #'(lambda (app)
                 (list app
                       (vector (list (pow-app-name app)
                                     'face 'link
                                     'follow-link t
                                     'app app
                                     'action 'pow-app-list-open-app
                                     )
                               (propertize (pow-app-path app)
                                           'font-lock-face 'default))))
             apps))
      (tabulated-list-init-header)
      (tabulated-list-print 'remember-pos))))

(defun pow-list-view-reload (list-view)
  (let ((apps (pow-app-load-all)))
    (setf (pow-list-view-apps list-view) apps)))

(defun pow-list-view-show (list-view)
  (pop-to-buffer (pow-list-view-buffer list-view)))

;; user function

;;;###autoload
(defun pow-list-apps ()
  (interactive)
  (let* ((list-view (make-pow-list-view)))
    (pow-list-view-reload list-view)
    (pow-list-view-refresh list-view)
    (pow-list-view-show list-view)))
(defalias 'list-pow-apps 'pow-list-apps)

(provide 'pow-app-list)
;;; pow-app-list.el ends here
