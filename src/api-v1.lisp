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


(defparameter *db* nil)


(defun symbol-to-header (symbol)
  (concatenate 'string "x-sexp-" (symbol-name symbol)))


(defun entry-headers (entry)
  "Convert internal entry values to headers."
  (loop :for (key value) :on entry :by #'cddr
        :unless (eql key :data)
          :do (setf (header-out (symbol-to-header key))
                    (write-to-string value :escape t :case :downcase :pretty nil))))


(defun parse-post-data (&optional (data (raw-post-data)))
  (read-from-string (flexi-streams:octets-to-string data :external-format :utf-8)))


(defun write-response (sexp)
  (setf (hunchentoot:content-type*) "text/plain")
  (write-to-string sexp :escape t :case :downcase))


(defun authenticated ()
  (unless *session*
    (setf (return-code*) +http-forbidden+)
    (write-response '(:message "Authenticated sessions only."))))


(hunchentoot:define-easy-handler (v1-auth) ()
  (case (request-method*)
    (:post
     (let* ((users (namespaced-database "users" :username "_internal"))
            (sexp (parse-post-data))
            (username (getf sexp :username))
            (key (getf sexp :key)))
       (if (equal (getf (cadr (record-data (gethash username users))) :key) key)
           (progn
             (start-session)
             (setf (session-value 'username) username)
             (write-response '(:message "Authenticated.")))
           (progn
             (setf (return-code*) +http-forbidden+)
             (write-response '(:message "Invalid username and password."))))))))


(hunchentoot:define-easy-handler (v1-view-all) (revision system)
  (unless (authenticated)
   (register-groups-bind (namespace)
       ("^/api/v1/hash/([^/]*)/" (request-uri*))
     (let ((*db* (namespaced-database namespace))
           (from-system (header-in* "x-system-name")))
       (case (request-method*)
         (:get (write-response
                (let ((revision (or (and revision (parse-integer revision :junk-allowed t)) 0))
                      (system (when (> (length system) 0) system)))
                  (multiple-value-bind (records min max)
                      (fetch-records revision system)
                    (setf (header-out "x-sexp-revision-min") min)
                    (setf (header-out "x-sexp-revision-max") max)
                    records))))
         (:post
          (let ((sexp (parse-post-data))
                (revision (database-max-revision)))
            (write-response
             (if (= (parse-integer (header-in* "x-sexp-revision")) revision)
                 (prog1
                     (loop :for row :in sexp
                           :for key = (car row)
                           :for origional = (or (gethash key *db*) (list :data nil :revision 0 :system nil))
                           :do (progn
                                 ;; only when the data has changed
                                 (when (not (equal row (getf origional :data)))
                                   (setf (getf origional :revision) (incf revision))
                                   ;; store the systems as a list
                                   (when (not (member from-system (getf origional :system) :test 'equal))
                                     (setf (getf origional :system) (cons from-system (getf origional :system))))
                                   (setf (getf origional :data) row)
                                   (setf (gethash key *db*) origional)))
                           :collect `(:system ,(getf origional :system)
                                      :revision ,(getf origional :revision)
                                      :data ,key))
                   (save-store namespace :database *db*))
                 (progn
                   (setf (return-code*) +http-conflict+)
                   '(:message "Out of sync.  Fetch the latest records first.")))))))))))


(hunchentoot:define-easy-handler (v1-view-attribute) ()
  (unless (authenticated)
   (register-groups-bind (namespace attribute)
       ("^/api/v1/hash/([^/]*)/@([^/]*)" (request-uri*))
     (let ((*db* (namespaced-database namespace))
           (attribute (unless (eq (length attribute) 0) (string-upcase attribute))))
       (case (request-method*)
         (:get (write-response
                (loop :for key :being :the hash-keys :in *db*
                      :collect (concatenate 'list `(:key ,key)
                                            (loop :for (key value) :on (gethash key *db*)
                                                  :by #'cddr
                                                  :unless (eql key :data)
                                                    :when (and attribute (equal attribute (symbol-name key)))
                                                      :collect `(,key ,value)
                                                  :unless (eql key :data)
                                                    :when (null attribute)
                                                      :collect `(,key ,value)))))))))))


(hunchentoot:define-easy-handler (v1-view-item) ()
  (unless (authenticated)
   (register-groups-bind (namespace group)
       ("^/api/v1/hash/([^/]*)/([^/]*)/" (request-uri*))
     (let ((*db* (namespaced-database namespace)))
       (case (request-method*)
         (:get
          (let ((entry (gethash group *db*)))
            (entry-headers entry)
            (write-response (record-data entry))))
         (:post
          (prog1
              (setf (gethash group *db*)
                    (read (raw-post-data :want-stream t)))
            (save-store namespace :database *db*))))))))
