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

(defun pow-app-list-open-app (&optional button)
  (interactive)
  (let ((app (if button (button-get button 'app)
               (tabulated-list-get-id))))
    (pow-app-open app)))

;; struct

(defstruct (pow-list-view
            (:constructor nil)
            (:constructor pow-list-view--inner-make))
  apps buffer)

(defun make-pow-list-view (&rest options)
  (let* ((list-view (apply 'pow-list-view--inner-make options))
         (buffer (pow-list-view-create-buffer list-view)))
    (setf (pow-list-view-buffer list-view) buffer)
    list-view))

(defun pow-list-view-create-buffer (list-view)
  (let ((buffer (get-buffer-create "*Pow Apps*")))
    (with-current-buffer buffer
      (set (make-local-variable 'pow-app-list--view) list-view)
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
