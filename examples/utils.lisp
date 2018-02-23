(defpackage my-test
  (:use :cl :cl-selenium)
  (:import-from :cl-selenium-utils
   :send-keys
                :click
   :wait-for))

(in-package :my-test)

(with-session ()
  (setf (url) "http://google.com")
  (send-keys "cl-selenium-webdriver")
  (click "[name=btnG]")
  (wait-for "#resultStats"))
