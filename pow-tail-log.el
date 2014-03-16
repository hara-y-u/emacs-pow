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
;;  Minor mode for buffer

;;; Code:

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
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let (num-lines
              (move-p (= (point) (process-mark proc))))
          (save-excursion
            (setq buffer-read-only nil)
            (buffer-disable-undo)
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
          (if move-p (goto-char (process-mark proc))))))))

(defun pow-tail-log-buffer-name (log-path &optional app-name)
    (format "*%s:%s* - pow tail log"
            (or app-name "pow app")
            (pow-filename log-path)))

(defun pow-tail-log-start (log-path &optional app-name)
  (let* ((buffer-name (pow-tail-log-buffer-name log-path app-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (get-buffer-process buffer)
      (progn
        (setq buffer (generate-new-buffer buffer-name))
        (with-current-buffer buffer
          (setq auto-window-vscroll t)
          (setq buffer-read-only t))
        (start-process "pow tail log"
                       buffer
                       "tail"
                       "-n" "100"
                       "-f" (expand-file-name log-path))))))

(defun pow--tail-log (&optional log-path app-name)
  (interactive "fLog file:")
  (let ((tail-process (pow-tail-log-start log-path app-name)))
    (set-process-filter tail-process #'pow-tail-log-process-filter)
    (funcall pow-tail-log-display-buffer-fn (process-buffer tail-process))))

;;;###autoload
(defun pow-tail-log (&optional name-or-app)
  (pow-interactive :app-name)
  (pow--with-name-or-app name-or-app app
    (pow--tail-log
     (pow-app-log-path app) (pow-app-name app))))

;;;###autoload
(defun pow-tail-app-log (&optional name-or-app app-log-kind)
  (pow-interactive :app-name :app-log-kind)
  (pow--with-name-or-app name-or-app app
    (pow--tail-log
     (pow-app-app-log-path app app-log-kind)
     (pow-app-name app))))

(provide 'pow-tail-log)
;;;pow-tail-log.el ends here
