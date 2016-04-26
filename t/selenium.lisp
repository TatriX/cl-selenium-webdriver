(in-package :cl-user)

(defpackage selenium-test
  (:use :cl :selenium :selenium-utils :prove))

(in-package :selenium-test)

(plan nil)

(is-print (princ (selenium::make-uri "/session") nil) "#<quri.uri.http:uri-http http://127.0.0.1:4444/wd/hub/session>")

(defparameter *base-url* "https://www.google.com?hl=en")

(defmacro with-base-session (&body body)
  `(with-session ()
     (setf (url) *base-url*)
     ,@body))

(subtest "session"
  (let (session)
    (ok (setf session (make-session)))
    (ok (delete-session session))))

(subtest "url"
  (with-session ()
    (setf (url) *base-url*)
    (like (url) "https://www.google.*")))

;; TODO: useless
(subtest "back"
  (with-base-session
    (like (url) "https://www.google.*")
    (ok (back))))

(subtest "find-element"
  (with-base-session
    (ok (find-element "#lst-ib"))
    (is-type (find-element "[name=q]") 'selenium::element)
    (ok (element-id (find-element "[name=btnK]")))))

(subtest "find-element-no-such-element"
  (with-base-session
    (is-error (find-element (gensym)) 'selenium:no-such-element-error)))

(subtest "element-clear"
  (with-base-session
    (ok (element-clear (find-element "[name=q]")))))

(subtest "element-send-keys"
  (with-base-session
    (ok (element-send-keys (find-element "[name=q]") "Common Lisp"))))

(subtest "element-click"
  (with-base-session
    (ok (element-click (find-element "[name=btnI]")))))

(subtest "element-text"
  (with-base-session
    (is (element-text (find-element "Gmail" :by :partial-link-text)) "Gmail")))

(subtest "element-attribute"
  (with-base-session
    (let ((input (find-element "[name=q]")))
      (element-send-keys input "cl-selenium-webdriver")
      (is (element-attribute input "value") "cl-selenium-webdriver"))))

(subtest "active-element"
  (with-base-session
    (ok (element-id (active-element)))
    (is (element-attribute (active-element) "name") "q")))

(subtest "cookie"
  (with-base-session
    (ok (setf (cookie) (make-cookie "foo" "bar")))
    (is (get-cookie (cookie) "foo") "bar")))

(subtest "refresh"
  (with-base-session
    (element-send-keys (find-element "[name=q]") "cl-selenium-webdriver")
    (refresh)
    (is (element-text (find-element "[name=q]")) "")))

(subtest "special-keys"
  (with-base-session
    (let ((input (find-element "[name=q]")))
      (element-send-keys input "webdriver")
      (element-send-keys input (key :home))
      (element-send-keys input "cl-selenium-")
      (is (element-attribute input "value") "cl-selenium-webdriver"))))

(finalize)
