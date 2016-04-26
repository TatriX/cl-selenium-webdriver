(in-package :cl-user)

(defpackage selenium-utils-test
  (:use :cl :selenium :selenium-utils :prove))

(in-package :selenium-utils-test)

(plan nil)

(subtest "wait-for"
  (with-session ()
    (setf (url) "http://google.com")
    (let ((result-selector "#resultStats"))
      (is-error (find-element result-selector) 'no-such-element-error)
      (element-send-keys (find-element "[name=q]") "cl-selenium-webdriver")
      (element-click (find-element "[name=btnG]"))
      (is-error (find-element result-selector) 'no-such-element-error)
      (ok (wait-for result-selector)))))

(subtest "cookie-get"
  (with-session ()
    (setf (url) "http://google.com")
    (setf (cookie) (make-cookie "cl-selenium-webdriver" "common lisp"))
    (is (get-cookie (cookie) "cl-selenium-webdriver") "common lisp")))

(finalize)
