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

(defparameter *httpd* nil
  "Bound to hunchentoot instance after startup.")

(defparameter *swank-enabled* nil
  "If set to true, a swank server is started on port *SWANK-PORT*")

(defparameter *swank-port* 4005
  "If *SWANK-ENABLED* is true, a swank server is started on this port.")

(defparameter *webserver-port* 8888
  "The hunchentoot server listens on this port.")

(defparameter *store-path* nil)

(defparameter *username* nil)


(setq *dispatch-table*
 (list
  (create-regex-dispatcher "^/api/v1/auth/$" 'v1-auth)
  (create-regex-dispatcher "^/api/v1/hash/[^/]*/$" 'v1-view-all)
  (create-regex-dispatcher "^/api/v1/hash/[^/]*/@[^/]*$" 'v1-view-attribute)
  (create-regex-dispatcher "^/api/v1/hash/[^/]*/[^/]*/" 'v1-view-item)))


(defvar *user-configurable-parameters*
  '((:swank-enabled *swank-enabled*)
    (:swank-port *swank-port*)
    (:webserver-port *webserver-port*)
    (:shutdown-port *shutdown-port*)
    (:store-path *store-path*)))


(defun initialize-parameters (&key path)
    (loop
       :for (keyword variable)
       :in *user-configurable-parameters*
       :do (when-let (it (get-user-configuration-parameter keyword path))
             (setf (symbol-value variable) it))))


(defun get-config-option (option component &optional path)
  (let ((filespec (or path
                      (make-pathname :defaults (asdf:component-pathname
                                                (asdf:component-system
                                                 (asdf:find-system component)))
                                     :name "config"
                                     :type "sexp")))
        (orig-filespec (make-pathname :defaults (asdf:component-pathname
                                                 (asdf:component-system component))
                                      :name "config"
                                      :type "sexp.in")))
    (unless (probe-file filespec)
      (with-simple-restart (accept-default "Create default settings for config.sexp and proceed.")
        (error "Missing configuration file: config.sexp.  Please copy config.sexp.in to config.sexp and customize for your local environment."))
      (with-open-file (src orig-filespec :direction :input)
        (with-open-file (dest filespec :direction :output)
          (write (read src) :stream dest))))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))


(defun get-user-configuration-parameter (name &optional path)
  "This function pulls a value from the key-value pairs stored in
   my-config.sexp so data stores can have their own pairs for appropriate
   customization after loading."
  (get-config-option name (asdf:find-system :emacs-sync) path))


(defun wait-for-shutdown-connection (&optional (port *shutdown-port*))
  "Opens a server socket at the given port (default *SHUTDOWN-PORT*,
waits for a connection indefinitely."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    ;; Listen on the given port for a TCP connection
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen socket 1)
    ;; When it comes, close the sockets and continue
    (let ((client-socket (sb-bsd-sockets:socket-accept socket)))
      (sb-bsd-sockets:socket-close client-socket)
      (sb-bsd-sockets:socket-close socket))))


(defun startup (&key config-path debug no-config)
  ;; Start our web server.
  (unless no-config (initialize-parameters :path config-path))
  (if debug
      (setf *catch-errors-p* nil)
      (setf *catch-errors-p* t))
  (ensure-directories-exist *store-path*)
  (setf *swank-server* (when *swank-enabled*
                         (swank:create-server :port *swank-port*
                                              :style :spawn :dont-close t)))
  (setf *httpd* (start (make-instance 'easy-acceptor :port *webserver-port*))))


(defun shutdown ()
  "disconnect hunchentoot and the database"
  (format t ";; Stopping Hunchentoot ... ")
  (stop *httpd*)
  (format t "done.~%"))

(defun main ()
  (startup :config-path "/etc/emacs-sync/config.sexp")
  (format t ";; Starting shutdown service at port: ~s.~%" *shutdown-port*)
  (wait-for-shutdown-connection)
  (format t ";; Shutdown connection detected at port ~S. Attempting to stop ... "
          *shutdown-port*)
  (shutdown)
  (format t "done.~%"))
