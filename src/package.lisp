;;;; package.lisp

(defpackage selenium
  (:use :cl)
  (:export :make-session
           :delete-session
           :with-session

           :key

           :url
           :back
           :refresh

           :find-element
           :active-element
           :element-clear
           :element-click
           :element-send-keys
           :element-id
           :element-text
           :element-tagname
           :element-attribute

           :make-cookie
           :cookie

           :log-types
           :logs

           :execute-script

           :no-such-element-error)
  (:import-from :alexandria
                :with-gensyms
                :assoc-value))

(defpackage selenium-utils
  (:use :cl :selenium)
  (:export :*timeout*
           :wait-for
           :get-cookie
           :elem
           :attr
           :text
           :send-key
           :send-keys
           :click)
  (:import-from :alexandria
                :assoc-value
                :rcurry))
