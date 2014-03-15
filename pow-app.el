;;; pow-app.el --- pow (http://pow.cx/) manager app model

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


;; errors

(put 'pow-app-already-exists 'error-conditions
     '(pow-app-already-exists pow-app-error error))
(put 'pow-app-already-exists 'error-message
     "App already exists")

(put 'pow-app-couldnt-delete 'error-conditions
     '(pow-app-couldnt-delete pow-app-error error))
(put 'pow-app-couldnt-delete 'error-message
     "Couldn't delete app")

(put 'pow-app-couldnt-restart 'error-conditions
     '(pow-app-couldnt-restart pow-app-error error))
(put 'pow-app-couldnt-restart 'error-message
     "Couldn't restart app")

(put 'pow-app-invalid-path 'error-conditions
     '(pow-app-invalid-path pow-app-error error))
(put 'pow-app-invalid-path 'error-message
     "Path is not rack project")

(put 'pow-app-invalid-name 'error-conditions
     '(pow-app-invalid-path pow-app-error error))
(put 'pow-app-invalid-path 'error-message
     "App name is invalid")


;; struct

(cl-defstruct (pow-app
            (:constructor nil)
            (:constructor pow-app--inner-make))
  "A structure abstracting pow app symlinks."
  (symlink-directory pow-symlink-directory)
  (path "")
  (name "")
  )

(defun make-pow-app (&rest options)
  "Constructor for `pow-app'.

options:
`symlink-directory': symlink-directory of pow apps. (defalut: `pow-symlink-directory')
`path': path to pow app project.
`name': name of the pow app."
  (if (plist-get options :path)
      (setq options (plist-put options :path
                               (expand-file-name
                                (plist-get options :path)))))
  (let ((pow-app
         (apply 'pow-app--inner-make options)))
    (if (not (plist-get options :name))
        (pow-app-set-name-with-path pow-app))
    pow-app))

(defun pow-app-set-name-with-path (app)
  "Set app name to last entry of the `path'."
  (let* ((path (pow-app-path app))
         (name (pow-filename path)))
    (when (not (null name))
      (setf (pow-app-name app) name))))

(defun pow-app-set-name-default (app)
  "Set app name to \"default\"."
  (setf (pow-app-name app) "default"))

(defun pow-app-url (app)
  "Getter for url of the pow app."
  (format pow-url-format (pow-app-name app)))

(defun pow-app-url-for-remote (app)
  "Returns app url for remote hosts."
  (format pow-url-for-remote-format
          (pow-app-name app)
          (pow-local-ip-address-string)))

(defun pow-app-open (app)
  "Open pow app url with function referenced by `pow-browse-url-function'."
  (let ((url (pow-app-url app)))
    (funcall pow-browse-url-function url)))

(defun pow-app-symlink-path (app)
  "Gertter for symlink's whole path of the pow app."
  (expand-file-name
   (pow-app-name app)
   (pow-app-symlink-directory app)))

(defun pow-app-validate (app)
  "Validate app"
  (unless (pow-rack-project-root-p (pow-app-path app))
    (signal 'pow-app-invalid-path
            (format "Path \"%s\" is not rack project" (pow-app-path app))))
  (unless (string-match "^\\([_-]\\|\\.\\|\\w\\)+$" (pow-app-name app))
    (signal 'pow-app-invalid-name
            (format "Name \"%s\" is invalid for pow app" (pow-app-name app)))))

(defun pow-app-save (app)
  "Save pow app as symlink in `symlink-directory'."
  (pow-app-validate app)
  (make-directory (pow-app-symlink-directory app) 'parents)
  (condition-case err
      (make-symbolic-link (pow-app-path app)
                          (pow-app-symlink-path app))
    (file-already-exists
     (signal 'pow-app-already-exists
             (format "App \"%s\" already exists" (pow-app-name app))))))

(defun pow-app-delete (app)
  "Delete pow app symlink in `symlink-directory'."
  (condition-case err
      (delete-file (pow-app-symlink-path app))
    (file-error
     (signal 'pow-app-couldnt-delete
             (format "Couldn't delete app \"%s\". Check if directory \"%s\" is writable"
                     (pow-app-name app)
                     pow-symlink-directory)))))

(defun pow-app-rename (app new-name)
  "Rename pow app symlink."
  (let ((old-app (copy-pow-app app)))
    (setf (pow-app-name app) new-name)
    (pow-app-save app)
    (pow-app-delete old-app)))

(defun pow-app-restart (app)
  "Flag pow app to restart on next request."
  (let* ((txt "restart.txt")
         (dir (expand-file-name "tmp" (pow-app-path app)))
         (path (expand-file-name txt dir)))
    (condition-case err
        (progn
          (make-directory dir 'parents)
          (write-region "" nil path nil 'quiet))
      (file-error
       (signal 'pow-app-couldnt-restart
               (format "Couldn't restart app \"%s\". Check if file \"%s\" is writable"
                       (pow-app-name app)
                       path))))))

(defun pow-app-log-path (app)
  "Path of pow log for the `app'."
  (expand-file-name (format "%s.log" (pow-app-name app))
                    pow-log-directory))

(defun pow-app-app-log-path (app app-log-kind)
  "Path of app log for the `app'"
  (expand-file-name
   (plist-get pow-app-log-files app-log-kind)
   (pow-app-path app)))

(defun pow-app-load-all ()
  "Load all pow apps in `symlink-directory'."
  (let* ((dir pow-symlink-directory)
         (symlinks (directory-files dir)))
    (cl-remove-if
     #'null
     (mapcar
      #'(lambda (symlink)
          (let ((symlink-path (expand-file-name symlink dir)))
            (when (and (not (equal "." symlink))
                       (not (equal ".." symlink))
                       (file-symlink-p symlink-path))
              (make-pow-app :path (file-chase-links symlink-path)
                            :name symlink)))
                    ) symlinks))))

(defun pow-app-load-for-project (project-dir)
  "Load pow app for `project-dir'."
  (cl-remove-if #'(lambda (app)
                 (not (pow-same-file-p
                       (pow-app-path app) project-dir)))
             (pow-app-load-all)))

(defun pow-app-load-for-dir (dir)
  "Load pow app for `dir'.
This function will find root project directory of `dir'
and pass it to `pow-app-load-for-project'."
  (let ((project-path (pow-rack-project-root-for-dir dir)))
    (when project-path
      (pow-app-load-for-project project-path))))

(defun pow-app-load-by-name (name)
  "Load pow app for `name'."
  (car
   (cl-remove-if #'(lambda (app)
                  (not (equal name (pow-app-name app))))
              (pow-app-load-all))))

(provide 'pow-app)
;;; pow-app.el ends here
