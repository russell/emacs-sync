;;; sync-gnus.el --- another attempt at a synchronization facility for
;;; Gnus

;; Author: Russell Sim <russell.sim@gmail.com>
;; Maintainer: Russell Sim <russell.sim@gmail.com>
;; Created: 3 Sept 2014
;; Version: 0.0.1
;; Url: http://github.com/russell/emacs-sync
;; Keywords: news, synchronization, nntp, nnrss
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

;; Put this in your startup file (~/.gnus.el for instance)

;;; Code:

(eval-when-compile (require 'cl))

(require 'request)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-util)

(defvar gnus-topic-alist) ;; gnus-group.el
(eval-when-compile
  (autoload 'gnus-group-topic "gnus-topic")
  (autoload 'gnus-topic-create-topic "gnus-topic" nil t)
  (autoload 'gnus-topic-enter-dribble "gnus-topic"))

(defgroup sync-gnus nil
  "The Gnus synchronization facility."
  :version "24.1"
  :group 'gnus)

(defcustom sync-gnus-newsrc-groups '("nntp" "nnrss")
  "List of groups to be synchronized in the gnus-newsrc-alist.
The group names are matched, they don't have to be fully
qualified.  Typically you would choose all of these.  That's the
default because there is no active sync backend by default, so
this setting is harmless until the user chooses a sync backend."
  :group 'sync-gnus
  :type '(repeat regexp))

(defcustom sync-gnus-newsrc-offsets '(2 3)
  "List of per-group data to be synchronized."
  :group 'sync-gnus
  :version "24.4"
  :type '(set (const :tag "Read ranges" 2)
	      (const :tag "Marks" 3)))

(defcustom sync-gnus-global-vars nil
  "List of global variables to be synchronized.
You may want to sync `gnus-newsrc-last-checked-date' but pretty
much any symbol is fair game.  You could additionally sync
`gnus-newsrc-alist', `gnus-server-alist', `gnus-topic-topology',
and `gnus-topic-alist'.  Also see `gnus-variable-list'."
  :group 'sync-gnus
  :type '(repeat (choice (variable :tag "A known variable")
                         (symbol :tag "Any symbol"))))

(defcustom sync-gnus-backend nil
  "The synchronization backend."
  :group 'sync-gnus
  :type '(radio (const :format "None" nil)
                (list :tag "Sync server"
                      (const :format "Sync Server API" sync)
                      (string :tag "URL of sync server for API access"))))

(defcustom sync-name (system-name)
  "The name for this machine."
  :group 'sync-gnus
  :version "24.3"
  :type 'string)

(defcustom sync-gnus-install-topics 'ask
  "Should sync install the recorded topics?"
  :group 'sync-gnus
  :version "24.3"
  :type '(choice (const :tag "Never Install" nil)
                 (const :tag "Always Install" t)
                 (const :tag "Ask Me Once" ask)))

(defcustom sync-gnus-remote-namespace "news"
  "The name for this machine."
  :group 'sync-gnus
  :version "24.3"
  :type 'string)

(defcustom sync-gnus-username "russell"
  ""
  :group 'sync-gnus
  :version "24.3"
  :type 'string)

(defcustom sync-gnus-api-key "eeh4uT-i9odah3osu=i|sh7uK3eMie"
  "The name for this machine."
  :group 'sync-gnus
  :version "24.3"
  :type 'string)

(defvar sync-gnus-state-hash (make-hash-table :test 'equal)
  "Sync status, keyed by group name")

(defvar sync-gnus-dirty-groups nil
  "Any locally modified groups should be added to this list.")

(defvar sync-gnus-state-revision 0
  "The revision of the remote server.")

(defvar sync-gnus-api-url "/api/v1/")

(defun sync-gnus-set-state (prop key val)
  "Update the PROPerty of document KEY at URL to VAL.
Updates `sync-gnus-state-hash'."
    (puthash (format "%s.%s" key prop) val sync-gnus-state-hash))

(defun sync-gnus-get-state (prop key)
  "Get the PROPerty of KEY from `sync-gnus-state-hash'."
    (gethash (format "%s.%s" key prop) sync-gnus-state-hash))

(defun sync-gnus-url ()
  (concat (nth 1 sync-gnus-backend)
          sync-gnus-api-url "hash/"
          sync-gnus-remote-namespace "/"))

(defun sync-gnus-parse (data)
  "Parse the result of a sync request."
  (condition-case nil
      (car (read-from-string data))
    (error
     (gnus-message 1 "sync-gnus-sexp-parse: Could not read the SEXP response!")
     nil)))


(defun* sync-gnus-call (url
                        &key
                        (type "GET")
                        (params nil)
                        (data nil)
                        (files nil)
                        (headers nil)
                        (success nil)
                        (error nil)
                        (complete nil)
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
     :parser
     (lambda ()
       (when (> (point-max) (point))
         (destructuring-bind (data . len)
             (read-from-string
              (buffer-substring-no-properties
               (point) (point-max)))
           (assert (equal
                    (string-to-number (request-response-header response "content-length"))
                    len) t "Content length doesn't match parsed data length.")
           data)))
     :success success
     :error error
     :status-code
     '((409 . (lambda (&rest _)
                (message "You are out of sync with the server.")))))
    parsed-data))


(defun sync-gnus-authenticate ()
  ""
  (let (resp)
    (sync-gnus-call
     (concat (nth 1 sync-gnus-backend)
             sync-gnus-api-url "auth/")
     :type "POST"
     :data (prin1-to-string `(:username ,sync-gnus-username :key ,sync-gnus-api-key))
     :success
     (function*
      (lambda (&key data &allow-other-keys)
        (setq resp data))))
    resp))

(defun sync-gnus-deep-print (data)
  ""
  (let* ((print-quoted t)
         (print-readably t)
         (print-escape-multibyte nil)
         (print-escape-nonascii t)
         (print-length nil)
         (print-level nil)
         (print-circle nil)
         (print-escape-newlines t))
    (format "%S" data)))


(defun sync-gnus-mark-group-dirty (&optional name)
  ""
  (let ((name (or name gnus-newsgroup-name)))
    (assert name)
    (when (and (gnus-grep-in-list name sync-gnus-newsrc-groups)
               (not (member name sync-gnus-newsrc-groups)))
      (push name sync-gnus-dirty-groups))))


(defun sync-gnus-newsrc-loader-builder (&optional only-modified)
  ""
  (let* ((entries (cdr gnus-newsrc-alist))
         entry name ret)
    (while entries
      (setq entry (pop entries)
            name (car entry))
      (when (gnus-grep-in-list name sync-gnus-newsrc-groups)
        (let ((topic (gnus-group-topic name)))
          (if only-modified
              (when (member name sync-gnus-dirty-groups)
                (gnus-message 9 "%s: add %s, it's modified"
                              "sync-gnus-newsrc-loader-builder" name)
                (push `(,name
                        ((info ,(nth 2 entry))
                         (level ,(nth 1 entry))
                         (topic ,topic)
                         (topic-offset ,(sync-gnus-topic-group-position name topic))
                         (marks ,(nth 3 entry))
                         (adaptive ,(gnus-score-load-score-alist
                                     (gnus-score-file-name name gnus-adaptive-file-suffix)))
                         (scores ,(gnus-score-load-score-alist
                                   (gnus-score-file-name name gnus-score-file-suffix)))
                         (server ,(nth 4 entry)))) ret))
            (push `(,name
                    ((info ,(nth 2 entry))
                     (level ,(nth 1 entry))
                     (topic ,topic)
                     (topic-offset ,(sync-gnus-topic-group-position name topic))
                     (marks ,(nth 3 entry))
                     (adaptive ,(gnus-score-load-score-alist
                                 (gnus-score-file-name name gnus-adaptive-file-suffix)))
                     (scores ,(gnus-score-load-score-alist
                               (gnus-score-file-name name gnus-score-file-suffix)))
                     (server ,(nth 4 entry)))) ret)))))
    ret))

(defun sync-gnus-position (search list &optional test)
  "Find the position of SEARCH in LIST using TEST, defaulting to `eq'."
  (let ((pos 0)
        (test (or test 'eq)))
    (while (and list (not (funcall test (car list) search)))
      (pop list)
      (incf pos))
    (if (funcall test (car list) search) pos nil)))

(defun sync-gnus-topic-group-position (group topic-name)
  (sync-gnus-position
   group (cdr (assoc topic-name gnus-topic-alist)) 'equal))

(defun sync-gnus-fix-topic-group-position (group topic-name position)
  (unless (equal position (sync-gnus-topic-group-position group topic-name))
    (let* ((loc "sync-gnus-fix-topic-group-position")
           (groups (delete group (cdr (assoc topic-name gnus-topic-alist))))
           (position (min position (1- (length groups))))
           (old (nth position groups)))
      (when (and old (not (equal old group)))
        (setf (nth position groups) group)
        (setcdr (assoc topic-name gnus-topic-alist)
                (append groups (list old)))
        (gnus-message 9 "%s: %s moved to %d, swap with %s"
                      loc group position old)))))


(defun sync-gnus-groups-builder (success error &optional revision)
  "Fetch the groups changed since REVISION.  REVISION defaults to
0.  URL is the location of the sync server."
  (sync-gnus-call
   (sync-gnus-url)
   :params `(("revision" . ,(number-to-string (or revision 0))))
   :success success
   :error error))

(defun sync-gnus-subscribe-group (name)
  "Subscribe to group NAME.  Returns NAME on success, nil otherwise."
  (gnus-subscribe-newsgroup name))

(defun sync-gnus-read-group-entry (name entry &rest passed-props)
  "Read ENTRY information for NAME.  Returns NAME if successful.
Skips entries whose sources don't contain
`sync-gnus-name'.  When the alist PASSED-PROPS has a
`subscribe-all' element that evaluates to true, we attempt to
subscribe to unknown groups.  The user is also allowed to delete
unwanted groups."
  (let* ((loc "sync-gnus-read-group-entry")
         (subscribe-all (cdr (assq 'subscribe-all passed-props)))
         (known (assoc name gnus-newsrc-alist))
         cell)
    (unless known
      (if (and subscribe-all
               (y-or-n-p (format "Subscribe to group %s?" name)))
          (setq known (sync-gnus-subscribe-group name))
        ;; else...
        (when (y-or-n-p (format "Delete group %s from server?" name))
          (if (equal name (sync-gnus-delete-group name))
              (gnus-message 1 "%s: removed group %s from server %s"
                            loc name (sync-gnus-url))
            (gnus-error 1 "%s: could not remove group %s from server %s"
                        loc name (sync-gnus-url))))))

    ;; if the source matches AND we have this group
    (if known
        (progn
          (gnus-message 10 "%s: reading Sync entry %s" loc name)
          (sync-gnus-set-state 'entry name entry)
          name)
      ;; else...
      (unless known
        (gnus-message 5 "%s: ignoring entry %s, it wasn't subscribed.  %s"
                      loc name "Call `sync-gnus-read' with C-u to force it."))
      nil)))


(defun sync-gnus-install-group-entry (name)
  (let* ((master (assoc name gnus-newsrc-alist))
         (old-topic-name (gnus-group-topic name))
         (old-topic (assoc old-topic-name gnus-topic-alist))
         (group-entry (sync-gnus-get-state 'entry name))
         (target-topic-name (cadr (assoc 'topic group-entry)))
         (target-topic-offset (cadr (assoc 'topic-offset group-entry)))
         (target-topic (assoc target-topic-name gnus-topic-alist))
         (loc "sync-gnus-install-group-entry"))
    (if master
        (progn
          (when (eq 'ask sync-gnus-install-topics)
            (setq sync-gnus-install-topics
                  (y-or-n-p "Install topics from remote?")))
          (when (and (eq t sync-gnus-install-topics)
                     target-topic-name)
            (if (equal old-topic-name target-topic-name)
                (gnus-message 12 "%s: %s is already in topic %s"
                              loc name target-topic-name)
              ;; see `gnus-topic-move-group'
              (when (and old-topic target-topic)
                (setcdr old-topic (gnus-delete-first name (cdr old-topic)))
                (gnus-message 5 "%s: removing %s from topic %s"
                              loc name old-topic-name))
              (unless target-topic
                (when (y-or-n-p (format "Create missing topic %s?"
                                        target-topic-name))
                  (gnus-topic-create-topic target-topic-name nil)
                  (setq target-topic (assoc target-topic-name
                                            gnus-topic-alist))))
              (if target-topic
                  (prog1
                      (nconc target-topic (list name))
                    (gnus-message 5 "%s: adding %s to topic %s"
                                  loc name (car target-topic))
                    (gnus-topic-enter-dribble))
                (gnus-error 2 "%s: remote group %s can't go in missing topic %s"
                            loc name target-topic-name)))
            (when (and target-topic-offset target-topic)
              (sync-gnus-fix-topic-group-position
               name target-topic-name target-topic-offset)))
          ;; install the subscription level
          (when (cadr (assoc 'level group-entry))
            (setf (nth 1 master) (cadr (assoc 'level group-entry))))
          ;; install the read and other marks
          (setf (nth 2 master) (cadr (assoc 'info group-entry)))
          (setf (nth 3 master) (cadr (assoc 'marks group-entry)))
          ;; install the score file
          (-when-let (scores (cadr (assoc 'scores group-entry)))
            (with-temp-buffer
              (insert (prin1-to-string scores))
              (gnus-write-buffer (gnus-score-file-name name gnus-score-file-suffix))))
          ;; install the adaptive file
          (-when-let (adaptive (cadr (assoc 'adaptive group-entry)))
            (with-temp-buffer
              (insert (prin1-to-string adaptive))
              (gnus-write-buffer (gnus-score-file-name name gnus-adaptive-file-suffix))))
          nil)
      (gnus-error 1 "%s: invalid remote group %s" loc name)
      'invalid-name)))


(defun sync-gnus-delete-group (name)
  "Returns NAME if successful deleting it from URL, an error otherwise."
  (interactive "sEnter group name: ")
  (sync-gnus-call
   (concat (sync-gnus-url) (url-hexify-string name) "/")
   :type "DELETE"))

(defun sync-gnus-get-group (name)
  "Returns NAME if successful deleting it from URL, an error otherwise."
  (interactive "sEnter group name: ")
  (let (response-data)
    (sync-gnus-call
     (concat (sync-gnus-url) name "/")
     :type "GET"
     :success
     (function*
      (lambda (&key data &allow-other-keys)
        (message "%s" data)
        (setq response-data data))))
    response-data))


(defun sync-gnus-save (&optional force)
  "Save the Gnus sync data to the backend.
With a prefix, FORCE is set and all groups will be saved."
  (interactive "P")
  (assert (and (listp sync-gnus-backend)
               (eq (nth 0 sync-gnus-backend) 'sexp-sync)
               (stringp (nth 1 sync-gnus-backend))))

  (let* ((ftime (float-time))
         (entries
          (sync-gnus-newsrc-loader-builder (not force))))
    ;; when there are no entries, there's nothing to save
    (if entries
        (sync-gnus-call
         (sync-gnus-url)
         :type "POST"
         :headers `(("x-sexp-revision" . ,(number-to-string sync-gnus-state-revision)))
         :data (prin1-to-string entries)
         :success
         (function*
          (lambda (&key data response &allow-other-keys)
            ;; Update the current state revision and remove any
            ;; dirty groups that were saved.
            (setq max-revision (request-response-header response "x-sexp-revision-max"))
            (setq sync-gnus-dirty-groups
                  (remove data sync-gnus-dirty-groups)))))
      (gnus-message
       2 "sync-gnus-save: nothing to save to the SEXP backend")
      nil)))

(defun sync-gnus-read (&optional subscribe-all refresh-all)
  "Load the Gnus sync data from the backend.
With a prefix, SUBSCRIBE-ALL is set and unknown groups will be subscribed."
  (interactive "P")
  (assert (and (listp sync-gnus-backend)
               (eq (nth 0 sync-gnus-backend) 'sexp-sync)
               (stringp (nth 1 sync-gnus-backend))))

  (when sync-gnus-backend
    (gnus-message 7 "sync-gnus-read: loading from backend %s" sync-gnus-backend)
    (let ((errored nil)
          groups name max-revision)

      ;; Gather list of groups
      (sync-gnus-groups-builder
       (function*
        (lambda (&key data response &allow-other-keys)
          (setq max-revision (string-to-number
                              (request-response-header response "x-sexp-revision-max")))
          (setq groups data)))
       (function*
        (lambda (&key data &allow-other-keys)
          (error "Failed to sync.")))
       (unless refresh-all
         sync-gnus-state-revision))

      ;; Read in groups
      (mapc (lambda (entry)
              (setq name (car entry))

              (unless errored
                (setq errored
                      (when (equal name
                                   (sync-gnus-read-group-entry
                                    name
                                    (cadr entry)
                                    `(subscribe-all ,subscribe-all)))
                        (sync-gnus-install-group-entry name)))))
            groups)
      (unless errored
        (setq sync-gnus-state-revision max-revision)))

    (gnus-message 9 "sync-gnus-read: remaking the newsrc hashtable")
    (gnus-make-hashtable-from-newsrc-alist)))

;;;###autoload
(defun sync-gnus-initialize ()
"Initialize the Gnus sync facility."
  (interactive)
  (gnus-message 5 "Initializing the sync facility")
  (sync-gnus-install-hooks))

;;;###autoload
(defun sync-gnus-install-hooks ()
  "Install the sync hooks."
  (interactive)
  ;; (add-hook 'gnus-get-new-news-hook 'sync-gnus-read)
  ;; (add-hook 'gnus-read-newsrc-el-hook 'sync-gnus-read)
  (add-hook 'gnus-group-catchup-group-hook 'sync-gnus-mark-group-dirty)
  (add-hook 'gnus-exit-group-hook 'sync-gnus-mark-group-dirty)
  (add-hook 'gnus-save-newsrc-hook 'sync-gnus-save))

(defun sync-gnus-unload-hook ()
  "Uninstall the sync hooks."
  (interactive)
  (remove-hook 'gnus-save-newsrc-hook 'sync-gnus-save))

(add-hook 'sync-gnus-unload-hook 'sync-gnus-unload-hook)

(when sync-gnus-backend (sync-gnus-initialize))

(provide 'sync-gnus)

;;; sync-gnus.el ends here
