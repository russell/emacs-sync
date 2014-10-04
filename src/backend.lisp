;; EMACS-Sync a service to store S-Exp
;; Copyright (C) 2014 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:emacs-sync)


(defparameter *dbs* (make-hash-table :test 'equal))

(defun pathname-to-database (namespace username)
  (let ((directory (pathname-as-directory (merge-pathnames username *store-path*))))
    (values
     (merge-pathnames namespace directory)
     directory)))


(defun load-store (namespace &key (username (session-value 'username)) (database *db*))
  (let ((path (pathname-to-database namespace username)))
    (when (file-exists-p path)
        (with-open-file (stream path
                                :external-format :utf-8)
          (loop :for row :in (read stream)
                :for key = (car (getf row :data))
                :do (setf (gethash key database) row)))))
  database)


(defun save-store (namespace &key (username (session-value 'username)) (database *db*))
  (multiple-value-bind (pathname directory)
      (pathname-to-database namespace username)
    (ensure-directories-exist directory)
    (with-open-file (stream pathname
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format :utf-8)
      (write
       (loop :for var :being :the hash-values :in database
             :collect var)
       :stream stream))))

(defun database-name (namespace username)
  (concatenate 'string username "/" namespace))

(defun namespaced-database (namespace &key (username (session-value 'username)))
  (if-let (database (gethash namespace *dbs*))
    database
    (progn
      (setf (gethash (database-name namespace username) *dbs*)
            (make-hash-table :test 'equal))
      (load-store namespace :database (gethash (database-name namespace username) *dbs*)
                            :username username))))


(defun database-max-revision (&key (database *db*))
  "Return the current revision number"
  (let ((max-revision 0))
   (loop :for var :being :the hash-values :in database
         :do (let ((db-revision (getf var :revision)))
               (when (> db-revision max-revision)
                 (setf max-revision db-revision))))
    max-revision))


(defun record-data (record)
  (getf record :data))


(defun fetch-records (revision system &key (database *db*))
  "Return the current revision number"
  (let (records min (max 0))
   (flet ((record-revision (record)
            (getf record :revision))
          (push-record (record)
            (prog1
                (push record records)
              (let ((revision (getf record :revision)))
                (when (> revision max)
                  (setf max revision))
                (when (or (null min) (< revision min))
                  (setf min revision))))))
     (maphash (lambda (k v)
                (declare (ignore k))
                (when (> (getf v :revision) revision)
                  (cond
                    ((and system (member system (getf v :system) :test #'equal))
                     (push-record v))
                    ;; if system is defined but doesn't match, skip
                    (system nil)
                    (t (push-record v)))))
              database)
     (values (mapcar #'record-data
                     (sort records #'< :key #'record-revision))
             min max))))
