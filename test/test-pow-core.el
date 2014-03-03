;;; test-pow-app.el --- unit test for pow-app

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

(require 'ert)
(require 'pow-core)

(defvar testdir "emacs-pow-test-project-20140303")
(defvar testpath (expand-file-name testdir "~/"))
(make-directory testpath 'parent)
(write-region "" nil (expand-file-name "config.ru" testpath) nil 'quiet)

;; pow-app
(ert-deftest make-pow-app ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (should
     (equal (pow-app-name app) "hoge"))
    (should
     (equal (pow-app-path app) "/home/huga/proj/hoge"))
    (should (equal (pow-app-symlink-directory app) pow-symlink-directory))))

(ert-deftest pow-app-set-name-default ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (pow-app-set-name-default app)
    (should (equal (pow-app-name app) "default"))))

(ert-deftest pow-app-url ()
  (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
    (should (equal (pow-app-url app) "http://hoge.dev/"))))

(ert-deftest pow-app-symlink-path ()
  (let ((app (make-pow-app :path testpath
                           :name "test-app")))
    (should (pow-same-file-p
             "~/.pow/test-app"
             (pow-app-symlink-path app)))))

(ert-deftest pow-app-validate-with-no-args ()
  (should-error
   (let ((app (make-pow-app)))
     (pow-app-validate app))))

(ert-deftest pow-app-validate-with-invalid-path ()
  (should-error
   (let ((app (make-pow-app :path "/hoge/huga")))
     (pow-app-validate app))))

(ert-deftest pow-app-validate-with-invalid-name ()
  (should-error
   (let ((app (make-pow-app :path " foo bar ")))
     (pow-app-validate app))))

(ert-deftest pow-app-validate-with-valid-path ()
  (let ((app (make-pow-app :path testpath)))
    (pow-app-validate app)))

(ert-deftest pow-app-save ()
  (let* ((app (make-pow-app :path testpath))
         (symlink-path (expand-file-name
                        (pow-app-name app)
                        (pow-app-symlink-directory app))))
    (pow-app-save app)
    (should (file-symlink-p symlink-path))
    (delete-file symlink-path)))

(ert-deftest pow-app-delete ()
  (let ((app (make-pow-app :path testpath)))
    (pow-app-save app)
    (should (file-symlink-p (pow-app-symlink-path app)))
    (pow-app-delete app)
    (should (not (file-exists-p (pow-app-symlink-path app))))))

(ert-deftest pow-app-rename ()
  (let ((app (make-pow-app :path testpath)))
    (pow-app-save app)
    (pow-app-rename app "emacs-pow-new-name")
    (should (file-symlink-p
             (expand-file-name "emacs-pow-new-name"
                               pow-symlink-directory)))
    (pow-app-delete app)))

(ert-deftest pow-app-restart ()
  (let* ((app (make-pow-app :path testpath))
        (txt (expand-file-name "tmp/restart.txt"
                               (pow-app-path app))))
    (pow-app-restart app)
    (should (file-exists-p txt))))


(ert-run-tests-batch)

(delete-directory testpath 'rec)
(makunbound 'testpath)
