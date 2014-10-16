;;;; emacs-sync.asd

(asdf:defsystem #:emacs-sync
  :serial t
  :description "Describe emacs-sync here"
  :author "Russell Sim <russell.sim@gmail.com>"
  :license "GPL"
  :depends-on (:hunchentoot :cl-ppcre :cl-fad :alexandria :swank :log4cl :sb-daemon)
  :components ((:static-file "emacs-sync.asd")
               (:module "src"
                :components ((:file "package")
                             (:file "sexp-sync")
                             (:file "backend")
                             (:file "api-v1")))))
