;;; pow-helpers.el --- pow (http://pow.cx/) manager helper macros/functions

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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'pow-variables)


;;
;; Macros
;;

(defmacro pow--with-name-or-app (name-or-app app &rest body)
  "An macro receives `name-or-app' arg and call `body' certainly with `app'."
  (declare (indent 2))
  `(let (,app name)
     (if (pow-app-p ,name-or-app)
         (setq ,app ,name-or-app name (pow-app-name ,name-or-app))
       (setq ,app (pow-app-load-by-name ,name-or-app)
             name ,name-or-app))
     (if (null ,app)
         (pow-error "App \"%s\" not found" name)
       (progn ,@body))))

(eval-and-compile
  (defvar pow-interactive-strategies
    `((:app-name (completing-read
                  "App name: "
                  (mapcar #'pow-app-name (pow-app-load-all))))
      (:new-app-name (read-string "New app name:"))
      (:app-log-kind (let ((def (symbol-name (car pow-app-log-files))))
                       (intern
                        (completing-read
                         (format "Log for(%s): " def)
                         (mapcar #'(lambda (elm)
                                     (symbol-name (car elm)))
                                 (pow-pair pow-app-log-files))
                         nil nil nil nil def)))))
    "Strategies for reading string by `interactive'."))

(defmacro pow-interactive (&rest strategies)
  "Interactive with `strategies'."
  `(interactive
    (list ,@(cl-reduce
             #'(lambda (mem val)
                 (if (member (car val) strategies)
                     (append mem (list (cadr val)))
                   mem))
             pow-interactive-strategies
             :initial-value nil))))

(defmacro pow-with-rack-project-root (root-path &rest body)
  "A macro verifies current-directory is in rack project,
and call `body' with project\'s `root-path'."
  (declare (indent 1))
  `(let ((,root-path (pow-rack-project-root-for-dir default-directory)))
     (if ,root-path
         (progn ,@body)
       (pow-user-error "Not in rack project"))))

(defmacro pow-with-message-error (message-on-success &rest body)
  "A macro translates errors to error message for interactive fns."
  (declare (indent 1))
  `(condition-case err
       (progn
         ,@body
         (unless (null ,message-on-success)
           (pow-message ,message-on-success)))
     (error (pow-error (cdr err)))))

(defmacro pow-with-current-apps (apps &rest body)
  "Execute `body' with `apps' of current project."
  (declare (indent 1))
  `(let ((,apps (pow-app-load-for-dir default-directory)))
     (if (not (null ,apps))
         (progn ,@body)
       (pow-error "App for current project was not found"))))

(defmacro pow-with-current-app (app &rest body)
  "Execute `body' with an `app' of current project.
If there is many app registered for the current project,
shows prompt to choose one app from the apps."
  (declare (indent 1))
  `(pow-with-current-apps apps
     (if (eq (length apps) 1)
         (let ((,app (car apps)))
           (progn ,@body))
       (let* ((names (mapcar #'(lambda (-app) (pow-app-name -app)) apps))
              (name
               (completing-read (format "App Name(%s):" (car names))
                                names nil t nil nil (car names)))
              (,app
               (car (cl-remove-if
                     #'(lambda (--app)
                         (not
                          (equal (pow-app-name --app) name)))
                     apps))))
         (if (not (null ,app))
             (progn ,@body)
           (pow-error "App \"%s\" is not found" name))))))

(defmacro pow-with-current-one-app (app &rest body)
  "Execute `body' with one of apps of current project.
App to be chosen is indefinite."
  (declare (indent 1))
  `(pow-with-current-apps apps
     (let ((,app (car apps)))
       (progn ,@body))))


;;
;; Functions
;;

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

(defun pow-filename (path)
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

(defun pow-local-ip-address-string ()
  "Returns local ip address in format like \"192.168.0.1\".
Device specified by `pow-default-network-device' will used."
  (format-network-address
   (assoc-default pow-default-network-device
                  (network-interface-list))
   'omit-port))


(provide 'pow-helpers)
;;; pow-helpers.el ends here
