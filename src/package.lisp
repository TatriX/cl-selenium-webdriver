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
           :find-elements
           :active-element
           :element-clear
           :element-click
           :element-send-keys
           :element-id
           :element-text
           :element-tagname
           :element-attribute

           :mouse-move-to
           :mouse-click

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
           :id
           :text
           :send-key
           :send-keys
           :click)
  (:import-from :alexandria
                :assoc-value
                :rcurry))
