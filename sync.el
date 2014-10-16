;;; sync.el --- another attempt at a synchronization facility for
;;; emacs

;; Author: Russell Sim <russell.sim@gmail.com>
;; Maintainer: Russell Sim <russell.sim@gmail.com>
;; Created: 3 Sept 2014
;; Version: 0.0.1
;; Url: http://github.com/russell/emacs-sync
;; Keywords: synchronization
;; Package-requires: ((request "0.2.0"))

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the sync-gnus.el package.  It's based/inspired by the
;; gnus-sync package included in gnus.

;;; Code:

(eval-when-compile (require 'cl))

(require 'request)

(defgroup sync nil
  "The Emacs synchronization facility."
  :group 'gnus)

(defcustom sync-server nil
  "The synchronization backend."
  :group 'sync
  :type 'string)

(defcustom sync-username ""
  "The username used to authenticate to the server."
  :group 'sync
  :type 'string)

(defcustom sync-api-key ""
  "The key that is used to authenticate to the server."
  :group 'sync
  :type 'string)

(defcustom sync-name (system-name)
  "The name for this machine."
  :group 'sync
  :type 'string)

(defcustom sync-verbose 6
  :group 'sync
  :type 'integer)

(defvar sync-api-url "/api/v1/")


(defun sync-url (namespace)
  "Create a URL using the `NAMESPACE'."
  (concat sync-server sync-api-url "hash/" namespace "/"))


(defun sync-message (level &rest args)
  "If LEVEL is lower than `sync-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages, 3 - non-serious error messages, 5 - messages for things
that take a long time, 7 - not very important messages on stuff, 9 - messages
inside loops."
  (if (<= level gnus-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))


(defun sync-parse ()
  "Parse the result of a sync request."
  (when (> (point-max) (point))
    (destructuring-bind (data . len)
        (condition-case nil
            (read-from-string
             (buffer-substring-no-properties
              (point) (point-max)))
          (error
           (sync-message 1 "sync-parse: Could not read the SEXP response!")
           nil))
      (assert (equal
               (string-to-number (request-response-header
                                  response "content-length"))
               len) t "Content length doesn't match parsed data length.")
      data)))


(defun* sync-call (url
                   &key
                   (type "GET")
                   (params nil)
                   (data nil)
                   (files nil)
                   (headers nil)
                   (success nil)
                   (error nil)
                   (complete nil)
                   (revision nil)
                   (timeout request-timeout))
  (let (parsed-data
        (headers (cons `("x-system-name" . ,sync-name) headers)))
    (when data
      (setq headers (cons `("content-type" . "text/plain") headers)))
    (request
     url
     :type type
     :data data
     :headers headers
     :sync t
     :parser 'sync-parse
     :success success
     :error error
     :status-code
     '((409 . (lambda (&rest _)
                (message "You are out of sync (%s) with the server (%s)."
                         revision
                         (request-response-header
                          response "x-sexp-revision"))))))
    parsed-data))


(defun sync-authenticate ()
  ""
  (let (resp)
    (sync-call
     (concat sync-server sync-gnus-api-url "auth/")
     :type "POST"
     :data (prin1-to-string
            `(:username ,sync-gnus-username :key ,sync-gnus-api-key))
     :success
     (function*
      (lambda (&key data &allow-other-keys)
        (setq resp data))))
    resp))


(provide 'sync)

;;; sync.el ends here
