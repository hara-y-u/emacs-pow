;;; pow-tail-log.el --- pow (http://pow.cx/) tail log view component

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
;;  Minor mode for buffer

;;; Code:

(require 'ansi-color)
(require 'pow-helpers)
(require 'pow-app)
(require 'cl-lib)

(defcustom pow-tail-log-max-num-lines 200
  "Max number of lines for tail log buffer."
  :type 'integer
  :group 'pow)

(defcustom pow-tail-log-display-buffer-fn #'pop-to-buffer
  "Function to display tail log buffer."
  :type 'function
  :group 'pow)

(defun pow-tail-log-process-filter (proc string)
  "Filter function for \"pow tail log\" process."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let (num-lines
              (move-p (= (point) (process-mark proc))))
          (setq buffer-read-only nil)
          (buffer-disable-undo)
          (save-excursion
            (goto-char (point-max))
            (insert string)
            (setq num-lines (count-lines (point-min) (point-max)))
            (save-excursion
              (when (< pow-tail-log-max-num-lines num-lines)
                (goto-char (point-min))
                (forward-line (- num-lines pow-tail-log-max-num-lines))
                (delete-region (point-min) (point))))
            (ansi-color-apply-on-region (process-mark proc)
                                        (point-max))
            (set-marker (process-mark proc) (point)))
          (setq buffer-read-only t)
          (buffer-enable-undo)
          (if move-p (goto-char (process-mark proc))))))))

(defun pow-tail-log-buffer-name (log-path &optional app-name)
    (format "*%s:%s - pow tail log*"
            (or app-name "pow app")
            (pow-filename log-path)))

(defun pow--tail-log-start (app log-path)
  (let* ((buffer-name
          (pow-tail-log-buffer-name log-path (pow-app-name app)))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (get-buffer-process buffer)
      (progn
        (setq buffer (generate-new-buffer buffer-name))
        (with-current-buffer buffer
          (setq auto-window-vscroll t)
          (setq buffer-read-only t)
          (setq default-directory (pow-app-path app)))
        (start-process "pow tail log"
                       buffer
                       "tail"
                       "-n" (number-to-string pow-tail-log-max-num-lines)
                       "-f" (expand-file-name log-path))))))

(defun pow-tail-log-start (app log-path)
  "Show buffor for \"pow tail log\" process.
If buffer dosen't exist, create one and start process for it."
  (let ((tail-process (pow--tail-log-start app log-path)))
    (set-process-filter tail-process #'pow-tail-log-process-filter)
    (funcall pow-tail-log-display-buffer-fn (process-buffer tail-process))))

;;;###autoload
(defun pow-tail-log (&optional name-or-app)
  "Tail log for `name-or-app', for monitoring purpose."
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (pow-tail-log-start app (pow-app-log-path app))))

;;;###autoload
(defun pow-tail-app-log (&optional name-or-app app-log-kind)
  "Tail app log for `name-or-app', for monitoring purpose."
  (pow-interactive :app-name :app-log-kind)
  (pow--with-name-or-app name-or-app app
    (pow-tail-log-start app
     (pow-app-app-log-path app app-log-kind))))

;;;###autoload
(defun pow-tail-current-log ()
  "Tail log for current app, for monitoring purpose."
  (interactive)
  (pow-with-current-one-app app
    (pow-tail-log app)))

;;;###autoload
(defun pow-tail-current-app-log (&optional app-log-kind)
  "Tail app log for current app, for monitoring purpose."
  (pow-interactive :app-log-kind)
  (pow-with-current-one-app app
    (pow-tail-app-log app app-log-kind)))

(provide 'pow-tail-log)
;;;pow-tail-log.el ends here
