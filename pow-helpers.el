;;; pow-helpers.el --- pow (http://pow.cx/) manager helper functions

;; Copyright (c) 2014 yukihiro hara

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

(require 'cl-lib)


(defun pow-rack-project-root-p (dir)
  "Check if the `dir' is rack project."
  (file-exists-p (expand-file-name "config.ru" dir)))

(defun pow-rack-project-root-for-dir (dir)
  "Return the root directory of the rack project within which DIR is found."
  (setq dir (expand-file-name dir))
  (cond ((or (not (file-directory-p dir))
          ;;; regexp to match windows roots, tramp roots, or regular posix roots
          ;;; copied from rinari.el
          (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" dir))
         nil)
        ((pow-rack-project-root-p dir) dir)
        (t (pow-rack-project-root-for-dir (replace-regexp-in-string "[^/]+/?$" "" dir)))))

(defun pow-same-file-p (&rest paths)
  "Check if all passed paths are pointing to same path."
  (cl-reduce #'(lambda (l r)
              (when (and
                     (stringp l) (stringp r)
                     (equal (file-truename l) (file-truename r)))
                (file-truename r)))
          (mapcar #'(lambda (path)
                      (replace-regexp-in-string "/$" "" path)) paths)
          :start 0))

(defun pow-message (format-string &rest args)
  "Message utility. format-string and rest args are passed to `format',
and then pass the output to `message'."
  (message (apply 'format (concat "Pow: " format-string) args)))

(defun pow-error (format-string &rest args)
  "Error utility. format-string and rest args are passed to `error'."
  (apply 'error (concat "Pow: " format-string) args))

(defun pow-user-error (format-string &rest args)
  "User error utility. format-string and rest args are passed to `user-error'."
  (apply 'user-error (concat "Pow: " format-string) args))

(defun pow-filename-last-directory (path)
  "Return the last entry of path."
  (string-match "\\/\\([^\\/]+\\)\\/?$" path)
  (match-string 1 path))

(defun pow-pair (lst)
  "Make list to paired elements list."
  (cond
   ((null lst) nil)
   ((eq (length lst) 1) (list lst))
   (t (cons (list
             (car lst) (cadr lst))
            (pow-pair (cddr lst))))))


(provide 'pow-helpers)
;;; pow-helpers.el ends here
