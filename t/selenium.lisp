(in-package :selenium)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(prove))
  (use-package :prove))

;; (in-package :cl-user)
;; (defpackage :selenium-test
;;   (:use :cl :selenium :prove))
;; (in-package selenium-test)





(plan nil)

(is-print (princ (make-uri "/session") nil) "#<quri.uri.http:uri-http http://127.0.0.1:4444/wd/hub/session>")

(defparameter *base-url* "https://www.google.com?hl=en")

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
  (with-session ()
    (setf (url) *base-url*)
    (like (url) "https://www.google.*")
    (ok (back))))

(subtest "find-element"
  (with-session ()
    (setf (url) *base-url*)
    (ok (find-element "#lst-ib"))
    (is-type (find-element "[name=q]") 'element)
    (ok (element-id (find-element "[name=btnK]")))))

(subtest "element-clear"
  (with-session ()
    (setf (url) *base-url*)
    (ok (element-clear (find-element "[name=q]")))))

(subtest "element-send-keys"
  (with-session ()
    (setf (url) *base-url*)
    (ok (element-send-keys (find-element "[name=q]") "Common Lisp"))))

(subtest "element-click"
  (with-session ()
    (setf (url) *base-url*)
    (ok (element-click (find-element "[name=btnI]")))))

(subtest "element-text"
  (with-session ()
    (setf (url) *base-url*)
    (is (element-text (find-element "Gmail" :by :partial-link-text)) "Gmail")))

(finalize)
