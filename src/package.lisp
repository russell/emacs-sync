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


(defpackage #:emacs-sync
  (:use #:cl)
  (:import-from #:hunchentoot
                #:start
                #:stop
                #:easy-acceptor
                #:request-uri*
                #:+http-conflict+
                #:+http-forbidden+
                #:return-code*
                #:header-in*
                #:*catch-errors-p*
                #:raw-post-data
                #:remote-addr*
                #:authorization
                #:script-name*
                #:query-string
                #:server-protocol*
                #:content-length*
                #:referer
                #:acceptor-log-access
                #:acceptor-log-message
                #:query-string*
                #:user-agent
                #:session-value
                #:start-session
                #:header-out
                #:*session*
                #:*dispatch-table*
                #:create-regex-dispatcher
                #:request-method*)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:cl-fad
                #:pathname-as-directory
                #:file-exists-p)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:alexandria
                #:when-let
                #:if-let))
