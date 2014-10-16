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

;;; Code:

(eval-when-compile (require 'cl))

(require 'request)
(require 'sync)

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

(defcustom sync-gnus-install-topics 'ask
  "Should sync install the recorded topics?"
  :group 'sync-gnus
  :type '(choice (const :tag "Never Install" nil)
                 (const :tag "Always Install" t)
                 (const :tag "Ask Me Once" ask)))

(defcustom sync-gnus-remote-namespace "news"
  "The name for this machine."
  :group 'sync-gnus
  :type 'string)

(defvar sync-gnus-state-hash (make-hash-table :test 'equal)
  "Sync status, keyed by group name")

(defvar sync-gnus-dirty-groups nil
  "Any locally modified groups should be added to this list.")

(defvar sync-gnus-state-revision 0
  "The revision of the remote server.")


(defun sync-gnus-set-state (prop key val)
  "Update the PROPerty of document KEY at URL to VAL.
Updates `sync-gnus-state-hash'."
    (puthash (format "%s.%s" key prop) val sync-gnus-state-hash))


(defun sync-gnus-get-state (prop key)
  "Get the PROPerty of KEY from `sync-gnus-state-hash'."
    (gethash (format "%s.%s" key prop) sync-gnus-state-hash))


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
                (sync-message 9 "%s: add %s, it's modified"
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
        (sync-message 9 "%s: %s moved to %d, swap with %s"
                      loc group position old)))))


(defun sync-gnus-groups-builder (success error &optional revision)
  "Fetch the groups changed since REVISION.  REVISION defaults to
0.  URL is the location of the sync server."
  (sync-call
   (sync-url sync-gnus-remote-namespace)
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
              (sync-message 1 "%s: removed group %s from server %s"
                            loc name (sync-url sync-gnus-remote-namespace))
            (sync-error 1 "%s: could not remove group %s from server %s"
                        loc name (sync-url sync-gnus-remote-namespace))))))

    ;; if the source matches AND we have this group
    (if known
        (progn
          (sync-message 10 "%s: reading Sync entry %s" loc name)
          (sync-gnus-set-state 'entry name entry)
          name)
      ;; else...
      (unless known
        (sync-message 5 "%s: ignoring entry %s, it wasn't subscribed.  %s"
                      loc name "Call `sync-gnus-read' with C-u to force it."))
      nil)))


(defun sync-gnus-install-group-entry (name)
  ""
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
                (sync-message 12 "%s: %s is already in topic %s"
                              loc name target-topic-name)
              ;; see `gnus-topic-move-group'
              (when (and old-topic target-topic)
                (setcdr old-topic (gnus-delete-first name (cdr old-topic)))
                (sync-message 5 "%s: removing %s from topic %s"
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
                    (sync-message 5 "%s: adding %s to topic %s"
                                  loc name (car target-topic))
                    (gnus-topic-enter-dribble))
                (sync-error 2 "%s: remote group %s can't go in missing topic %s"
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
              (gnus-write-buffer
               (gnus-score-file-name name gnus-score-file-suffix))))
          ;; install the adaptive file
          (-when-let (adaptive (cadr (assoc 'adaptive group-entry)))
            (with-temp-buffer
              (insert (prin1-to-string adaptive))
              (gnus-write-buffer
               (gnus-score-file-name name gnus-adaptive-file-suffix))))
          nil)
      (gnus-error 1 "%s: invalid remote group %s" loc name)
      'invalid-name)))


(defun sync-gnus-delete-group (name)
  "Returns NAME if successful deleting it from URL, an error otherwise."
  (interactive "sEnter group name: ")
  (sync-call
   (concat (sync-url sync-gnus-remote-namespace) (url-hexify-string name) "/")
   :type "DELETE"))


(defun sync-gnus-get-group (name)
  "Returns NAME if successful deleting it from URL, an error otherwise."
  (interactive "sEnter group name: ")
  (let (response-data)
    (sync-call
     (concat (sync-url sync-gnus-remote-namespace) name "/")
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
  (let* ((ftime (float-time))
         (entries
          (sync-gnus-newsrc-loader-builder (not force))))
    ;; when there are no entries, there's nothing to save
    (if entries
        (progn
          (sync-message 5 "sync-gnus-save: Saving entries.")
          (sync-call
           (sync-url sync-gnus-remote-namespace)
           :type "POST"
           :headers `(("x-sexp-revision" .
                       ,(number-to-string sync-gnus-state-revision)))
           :data (prin1-to-string entries)
           :revision sync-gnus-state-revision
           :success
           (function*
            (lambda (&key data response &allow-other-keys)
              ;; Update the current state revision and remove any
              ;; dirty groups that were saved.
              (setq max-revision
                    (request-response-header response "x-sexp-revision"))
              (sync-message 5 "sync-gnus-save: Server revision at %s."
                            max-revision)

              ;; Remove synced groups from the dirty groups list
              (let ((groups (mapcar (lambda (e) (getf e :data)) data)))
                (sync-message 9 "sync-gnus-save: Synced groups %s" groups)
                (setq sync-gnus-dirty-groups
                      (remove-if (lambda (e) (member e groups))
                                 sync-gnus-dirty-groups)))))))
      (sync-message 2 "sync-gnus-save: Nothing to sync.")
      nil)))


(defun sync-gnus-read (&optional subscribe-all refresh-all)
  "Load the Gnus sync data from the backend.
With a prefix, SUBSCRIBE-ALL is set and unknown groups will be subscribed."
  (interactive "P")
  (when sync-gnus-backend
    (sync-message 7 "sync-gnus-read: loading from backend %s" sync-server)
    (let ((errored nil)
          groups name max-revision)

      ;; Gather list of groups
      (sync-gnus-groups-builder
       (function*
        (lambda (&key data response &allow-other-keys)
          (setq max-revision
                (string-to-number
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
        (sync-message 7 "sync-gnus-read: server revision at %s" max-revision)
        (when max-revision
          (setq sync-gnus-state-revision max-revision))))

    (sync-message 9 "sync-gnus-read: remaking the newsrc hashtable")
    (gnus-make-hashtable-from-newsrc-alist)))


;;;###autoload
(defun sync-gnus-initialize ()
"Initialize the Gnus sync facility."
  (interactive)
  (sync-message 5 "Initializing the sync facility")
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


(defun sync-gnus-unload-hooks ()
  "Uninstall the sync hooks."
  (interactive)
  (remove-hook 'gnus-group-catchup-group-hook 'sync-gnus-mark-group-dirty)
  (remove-hook 'gnus-exit-group-hook 'sync-gnus-mark-group-dirty)
  (remove-hook 'gnus-save-newsrc-hook 'sync-gnus-save))


(provide 'sync-gnus)

;;; sync-gnus.el ends here
