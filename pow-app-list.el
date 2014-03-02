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
        (menu-map (make-sparse-keymap "Pow")))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "C-m") 'pow-app-list-open-app)
    (define-key map (kbd "m")   'pow-app-list-rename-app)
    (define-key map (kbd "f")   'pow-app-list-find-app-path)
    (define-key map (kbd "u")   'pow-app-list-mark-unmark)
    (define-key map (kbd "U")   'pow-app-list-mark-unmark-all)
    (define-key map (kbd "d")   'pow-app-list-mark-delete)
    (define-key map (kbd "r")   'pow-app-list-refresh)
    (define-key map (kbd "x")   'pow-app-list-execute)
    map)
  "Local keymap for `pow-app-list-mode' buffers.")

(easy-menu-define pow-app-list-menu pow-app-list-mode-map
  "Menu for `pow-app-list-mode'"
  '("Pow Apps"
    ["Execute actions" pow-app-list-execute]
    "--"
    ["Refresh app list" revert-buffer]
    "--"
    ["Unmark all" pow-app-list-mark-unmark-all]
    ["Unmark" pow-app-list-mark-unmark]
    ["Mark for deletion" pow-app-list-mark-delete]
    "--"
    ["Open app" pow-app-list-open-app]
    ["`find-file' app" pow-app-list-find-app-path]
    ["Rename app" pow-app-list-rename-app]
    "--"
    ["Quit" quit-window]))

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

(defvar pow-app-list--view nil
  "Reference to view object on pow-app-list-mode buffers.")


;; app-list-mode functions

(defmacro pow-app-list-ad-buffer-check (fn)
  "Make sure the `fn' is called in `pow-app-list-mode' buffer."
  (declare (indent 1))
  `(progn
    (defadvice ,fn (before pow-app-list-check-buffer (&rest arg))
     "Check if in `pow-app-list-mode' buffer"
     (unless (derived-mode-p 'pow-app-list-mode)
       (user-error "The current buffer is not in Pow App List mode")))
    (ad-activate ',fn)))

(defun pow-app-list-refresh ()
  "Refresh `pow-app-list-mode' buffer."
  (interactive)
  (funcall 'pow-app-list-view-reload pow-app-list--view)
  (funcall 'pow-app-list-view-refresh pow-app-list--view))
(pow-app-list-ad-buffer-check pow-app-list-refresh)

(defun pow-app-list-open-app (&optional button)
  "Open current line app."
  (interactive)
  (let ((app (if button (button-get button 'app)
               (tabulated-list-get-id))))
    (pow-app-open app)))
(pow-app-list-ad-buffer-check pow-app-list-open-app)

(defun pow-app-list-rename-app (&optional new-app-name button)
  "Rename current line app."
  (pow-interactive :new-app-name)
  (let ((app (if button (button-get button 'app)
               (tabulated-list-get-id))))
    (pow-app-rename app new-app-name))
  (pow-app-list-refresh))
(pow-app-list-ad-buffer-check pow-app-list-rename-app)

(defun pow-app-list-find-app-path (&optional button)
  "`find-file' project directory of current line app."
  (interactive)
  (let ((app (if button (button-get button 'app)
               (tabulated-list-get-id))))
    (find-file (pow-app-path app))))
(pow-app-list-ad-buffer-check pow-app-list-find-app-path)

(defun pow-app-list-mark-unmark (&optional _num)
  "Unmark current line app."
  (interactive "p")
  (tabulated-list-put-tag " " 'next-line))
(pow-app-list-ad-buffer-check pow-app-list-mark-unmark)

(defun pow-app-list-mark-unmark-all ()
  "Unmark all apps."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (pow-app-list-mark-unmark))))
(pow-app-list-ad-buffer-check pow-app-list-mark-unmark-all)

(defun pow-app-list-mark-delete (&optional _num)
  "Mark delete current app."
  (interactive "p")
  (tabulated-list-put-tag "D" 'next-line))
(pow-app-list-ad-buffer-check pow-app-list-mark-delete)

(defun pow-app-list-execute (&optional noquery)
  "Execute all marked actions."
  (interactive)
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
  (pow-app-list-refresh))
(pow-app-list-ad-buffer-check pow-app-list-execute)



;; View
(defstruct (pow-app-list-view
            (:constructor nil)
            (:constructor pow-app-list-view--inner-make))
  "View for convenience of manipulate `pow-app-list-mode'."
  apps buffer)

(defun make-pow-app-list-view (&rest options)
  "Constructor of `pow-app-list-view'."
  (let* ((list-view (apply 'pow-app-list-view--inner-make options))
         (buffer (pow-app-list-view-create-buffer list-view)))
    list-view))

(defun pow-app-list-view-create-buffer (list-view)
  "Create buffer for `pow-app-list-view'."
  (let ((buffer (get-buffer-create "*Pow Apps*")))
    (setf (pow-app-list-view-buffer list-view) buffer)
    (with-current-buffer buffer
      (pow-app-list-mode)
      (set (make-local-variable 'pow-app-list--view)
           list-view))
    buffer))

(defun pow-app-list-view-refresh (list-view)
  "Refresh buffer for `pow-app-list-view'."
  (let ((apps (pow-app-list-view-apps list-view))
        (buffer (pow-app-list-view-buffer list-view)))
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

(defun pow-app-list-view-reload (list-view)
  "Reload all apps for `pow-app-list-view'."
  (let ((apps (pow-app-load-all)))
    (setf (pow-app-list-view-apps list-view) apps)))

(defun pow-app-list-view-show (list-view)
  "Show `pow-app-list-view'."
  (pop-to-buffer (pow-app-list-view-buffer list-view)))



;; user function

;;;###autoload
(defun pow-list-apps ()
  "List all registered pow apps."
  (interactive)
  (let* ((list-view (make-pow-app-list-view)))
    (pow-app-list-view-reload list-view)
    (pow-app-list-view-refresh list-view)
    (pow-app-list-view-show list-view)))
(defalias 'list-pow-apps 'pow-list-apps)



(provide 'pow-app-list)
;;; pow-app-list.el ends here
