;;; pow-interactive.el --- pow (http://pow.cx/) manager interactive functions

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
(require 'pow-variables)
(require 'pow-helpers)
(require 'pow-app)


;;
;; user function
;;

;; TODO: familiar error message
;;;###autoload
(defun pow-register-app (&optional name path)
  "Register pow app for `name' and `path'."
  (interactive "sApp name: \nDPath to rack project: ")
  (let ((app (make-pow-app :path path :name name)))
    (pow-app-save app)))

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
                       (completing-read
                        (format "Log for(%s): " def)
                        (mapcar #'(lambda (elm)
                                    (symbol-name (car elm)))
                                (pow-pair pow-app-log-files))
                        nil nil nil nil def))))
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

;;;###autoload
(defun pow-unregister-app (&optional name-or-app)
  "Unregister app specified by `name-or-app'."
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (pow-app-delete app)))

;;;###autoload
(defun pow-open-app (&optional name-or-app)
  "Open app specified by `name-or-app'."
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (pow-app-open app)))

;;;###autoload
(defun pow-copy-url-for-remote (&optional name-or-app)
  "Copy remote accessible url to clipboard."
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (with-temp-buffer
      (insert (pow-app-url-for-remote app))
      (clipboard-kill-region (point-min) (point-max)))))

;;;###autoload
(defun pow-restart-app (&optional name-or-app)
  "Restart app specified by `name-or-app'."
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (pow-app-restart app)))

;;;###autoload
(defun pow-rename-app (&optional name-or-app new-app-name)
  "Rename app specified by `name-or-app' to `new-app-name'."
  (pow-interactive :app-name :new-app-name)
  (pow--with-name-or-app name-or-app app
    (pow-app-rename app new-app-name)))

;;;###autoload
(defun pow-find-log (&optional name-or-app)
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (find-file (pow-app-log-path app))))

;;;###autoload
(defun pow-find-app-log (&optional name-or-app app-log-kind)
  (pow-interactive :app-name :app-log-kind)
  (pow--with-name-or-app name-or-app app
    (find-file
     (pow-app-app-log-path app app-log-kind))))

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

;;;###autoload
(defun pow-register-current-app (&optional name)
  "Register current project as an pow app."
  (interactive)
  (pow-with-rack-project-root path
    (let (app
          (appname (pow-filename-last-directory path)))
      (when (null name)
          (setq name (read-string (format "App Name(%s):" appname)
                                  nil nil appname)))
      (pow-with-message-error
          (format "Registered app \"%s\"" name)
        (pow-register-app name path)))))

;;;###autoload
(defun pow-register-current-app-as-default ()
  "Registers current project as an pow app named \"default\"."
  (interactive)
  (pow-register-current-app "default"))

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

;;;###autoload
(defun pow-unregister-current-app ()
  "Unregister a pow app for current project."
  (interactive)
  (pow-with-current-app app
    (pow-with-message-error
        (format "Unregistered app \"%s\"" (pow-app-name app))
      (pow-unregister-app app))))

;;;###autoload
(defun pow-open-current-app ()
  "Open a pow app for current project."
  (interactive)
  (pow-with-current-app app (pow-open-app app)))

;;;###autoload
(defun pow-copy-current-app-url-for-remote ()
  "Copy remote accessible url of current app to clipboard."
  (interactive)
  (pow-with-current-app app
    (pow-copy-url-for-remote app)))

;;;###autoload
(defun pow-restart-current-app ()
  "Restart a pow app for current project."
  (interactive)
  (pow-with-current-one-app app
    (pow-with-message-error
        (format "App \"%s\" will restart on next request" (pow-app-name app))
      (pow-restart-app app))))

;;;###autoload
(defun pow-find-current-log ()
  (interactive)
  (pow-with-current-one-app app
    (find-file (pow-app-log-path app))))

;;;###autoload
(defun pow-find-current-app-log (&optional app-log-kind)
  (pow-interactive :app-log-kind)
  (pow-with-current-one-app app
    (find-file
     (pow-app-app-log-path app (intern app-log-kind)))))

;;;###autoload
(defun pow-rename-current-app (&optional new-app-name)
  "Rename a pow app for current project."
  (pow-interactive :new-app-name)
  (pow-with-current-app app (pow-rename-app app new-app-name)))

(provide 'pow-interactive)
;;; pow-interactive.el ends here
