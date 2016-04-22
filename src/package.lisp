;;;; package.lisp

(defpackage selenium
  (:use :cl)
  (:export :make-session
           :delete-session
           :with-session)
  (:import-from :alexandria
                :with-gensyms))
