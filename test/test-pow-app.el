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

(defvar testdir "~/test-project")
(make-directory testdir 'parent)
(write-region "" nil (expand-file-name "config.ru" testdir) nil 'quiet)

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

;; (ert-deftest pow-app-open ()
;;   (let ((app (make-pow-app :path "/home/huga/proj/hoge")))
;;     (pow-app-open app)
;;     (should (y-or-n-p "App url opened correctly?"))))

(ert-deftest pow-app-symlink-path ()
  (let ((app (make-pow-app :path testdir
                           :name "test-app")))
    (should (pow-same-file-p
             "~/.pow/test-app"
             (pow-app-symlink-path app)))))

(ert-deftest pow-app-save ()
  (let* ((app (make-pow-app :path testdir))
         (symlink-path (expand-file-name
                        (pow-app-name app)
                        (pow-app-symlink-directory app))))
    (pow-app-save app)
    (should (file-symlink-p symlink-path))
    (delete-file symlink-path)))

(ert-run-tests-batch)

(delete-directory testdir 'rec)
(makunbound 'testdir)
