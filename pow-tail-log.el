;;; pow-log.el --- pow (http://pow.cx/) tail log view component

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

;;; TODO:
;;; limit line number

;;; Code:

(require 'pow-helpers)
(require 'pow-app)
(require 'cl-lib)

(defun pow-tail-log-colorize-buffer ()
  "copied from emacs-rails"
  (let ((buffer (current-buffer)))
    (make-local-variable 'after-change-functions)
    (add-hook 'after-change-functions
              '(lambda (start end len)
                 (ansi-color-apply-on-region start end)))))

(defun pow-tail-log-buffer-name (log-path)
  (format "*pow tail log:%s*" (pow-filename log-path)))

(defun pow-tail-log-start (log-path)
  (let* ((buffer-name (pow-tail-log-buffer-name log-path))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (get-buffer-process buffer)
      (progn
        (setq buffer (generate-new-buffer buffer-name))
        (with-current-buffer buffer
          (setq auto-window-vscroll t)
          (setq buffer-read-only t)
          (pow-tail-log-colorize-buffer))
        (start-process "pow tail log"
                       buffer
                       "tail"
                       "-n" "20"
                       "-f" (expand-file-name log-path))))))

;;;###autoload
(defun pow-tail-log (&optional log-path)
  (interactive "fLog file:")
  (let ((tail-process (pow-tail-log-start log-path)))
    (pop-to-buffer (process-buffer tail-process))))
