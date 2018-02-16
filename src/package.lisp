;;;; package.lisp

(defpackage selenium
  (:use :cl)
  (:export :make-session
           :delete-session
           :with-session

           :start-interactive-session
           :stop-interactive-session

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

           :switch-to-frame
           :close-current-window

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
           :*default-element-func*
           :find-elem
           :wait-for
           :get-cookie
           :elem
           :attr
           :id
           :classname
           :classlist
           :text
           :send-key
           :send-keys
           :click)
  (:import-from :alexandria
                :assoc-value
                :rcurry))
