;;; pow-variables.el --- pow (http://pow.cx/) manager variables

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


;;
;; customs
;;

(defgroup pow nil
  "pow (http://pow.cx/) apps manager"
  :group 'development
  :prefix "pow-")

(defcustom pow-symlink-directory "~/.pow"
  "Directory for symlinks of pow apps."
  :type 'string
  :group 'pow)

(defcustom pow-url-format "http://%s.dev/"
  "Url format of rack application registered on Pow. This is passed to format function with one argument: app-name."
  :type 'string
  :group 'pow)

(defcustom pow-browse-url-function 'browse-url
  "Function to browse app, takes an app-url."
  :type 'function
  :group 'pow)

(defcustom pow-log-directory "~/Library/Logs/Pow/apps"
  "Directory of pow apps logs."
  :type 'directory
  :group 'pow)

(defcustom pow-default-network-device "en0"
  "Defalut network device to use within apps development."
  :type 'string
  :group 'pow)

(defcustom pow-url-for-remote-format "http://%s.%s.xip.io/"
  "Url format for remote host of rack application registered on Pow. This is passed to format function with two arguments: app name and ip address."
  :type 'string
  :group 'pow)


;;
;; buffer customizable variables
;;

(defvar pow-app-log-files
  (list
   'development "log/development.log"
   'test        "log/test.log"
   'production  "log/production.log")
  "plist for log files of app.
Create same-name buffer-local variable to customize this.")
(make-variable-buffer-local 'pow-app-log-files)


(provide 'pow-variables)
;;; pow-variables.el ends here
