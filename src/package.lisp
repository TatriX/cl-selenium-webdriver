;;;; package.lisp

(defpackage selenium
  (:use :cl)
  (:export :make-session
           :delete-session
           :with-session

           :url
           :back

           :find-element
           :element-clear
           :element-click
           :element-send-keys
           :element-id
           :element-text

           :no-such-element-error)
  (:import-from :alexandria
                :with-gensyms))
