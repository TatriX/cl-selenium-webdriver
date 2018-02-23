(in-package :cl-user)

(defpackage cl-selenium-utils-test
  (:use :cl :cl-selenium :cl-selenium-utils :prove))

(in-package :cl-selenium-utils-test)

(defparameter *base-url* "https://www.google.com?hl=en")
(defparameter *headless* '((:chrome-options . ((:args . #("--headless"))))))
(setf *timeout* 5)

(defmacro with-base-session (&body body)
  `(with-session (:additional-capabilities *headless*)
     (setf (url) *base-url*)
     ,@body))

(plan nil)

(subtest "find-elem"
  (with-base-session
    (is-type (find-elem "[name=q]") 'cl-selenium::element)
    (is (find-elem (gensym)) nil)))

(subtest "wait-for"
  (with-base-session
    (let ((result-selector "#resultStats"))
      (is-error (find-element result-selector) 'no-such-element-error)
      (element-send-keys (find-element "[name=q]") "cl-selenium-webdriver")
      (sleep 0.5)
      (element-click (find-element "[name=btnK]"))
      (ok (wait-for result-selector)))))

(subtest "cookie-get"
  (with-base-session
    (setf (cookie) (make-cookie "cl-selenium-webdriver" "common lisp"))
    (is (get-cookie (cookie) "cl-selenium-webdriver") "common lisp")))

(subtest "elem"
  (with-base-session
    (is (element-id (elem)) (element-id (active-element)))
    (is (element-id (elem "[name=btnK]")) (element-id (find-element "[name=btnK]")))))

(subtest "attr"
  (with-base-session
    (is (attr "name") "q")
    (is (attr "id" "[name=q]") "lst-ib")))

(subtest "id"
  (with-base-session
    (is (id) "lst-ib")))

(subtest "classname"
  (with-base-session
    (is (classname) "gsfi lst-d-f")))

(subtest "classlist"
  (with-base-session
    (is (classlist) '("gsfi" "lst-d-f"))))

(subtest "text"
  (with-base-session
    (is (text ".gb_P") "Gmail")))

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
    (sleep 0.5)
    (click "[name=btnK]")
    (ok (wait-for "#resultStats"))))

(finalize)
