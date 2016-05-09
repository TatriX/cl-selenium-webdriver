(in-package :cl-user)

(defpackage selenium-utils-test
  (:use :cl :selenium :selenium-utils :prove))

(in-package :selenium-utils-test)

(defparameter *base-url* "https://www.google.ru?hl=en")

(defmacro with-base-session (&body body)
  `(with-session ()
     (setf (url) *base-url*)
     ,@body))

(plan nil)

(subtest "wait-for"
  (with-base-session
    (let ((result-selector "#resultStats"))
      (is-error (find-element result-selector) 'no-such-element-error)
      (element-send-keys (find-element "[name=q]") "cl-selenium-webdriver")
      (element-click (find-element "[name=btnG]"))
      (is-error (find-element result-selector) 'no-such-element-error)
      (ok (wait-for result-selector)))))

(subtest "cookie-get"
  (with-base-session
    (setf (cookie) (make-cookie "cl-selenium-webdriver" "common lisp"))
    (is (get-cookie (cookie) "cl-selenium-webdriver") "common lisp")))

(subtest "elem"
  (with-base-session
    (is (element-id (elem)) (element-id (active-element)))
    (is (element-id (elem "[name=btnG]")) (element-id (find-element "[name=btnG]")))))

(subtest "attr"
  (with-base-session
    (is (attr "name") "q")))

(subtest "text"
  (with-base-session
    (is (text ".logo-subtext") "Russia")))

(subtest "send-key"
  (with-base-session
    (send-key :tab)
    (is (attr "name") "btnK")))

(subtest "send-keys"
  (with-base-session
    (send-keys "cl-selenium-webdriver")
    (is (attr "value") "cl-selenium-webdriver")))

(subtest "click"
  (with-base-session
    (send-keys "cl-selenium-webdriver")
    (click "[name=btnG]")
    (ok (wait-for "#resultStats"))))

(finalize)
