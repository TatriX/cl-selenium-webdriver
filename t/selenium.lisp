(in-package :cl-user)

(defpackage cl-selenium-test
  (:use :cl :cl-selenium :cl-selenium-utils :prove))

(in-package :cl-selenium-test)

(plan nil)

(subtest "make-uri"
  (let ((uri (cl-selenium::make-uri "/session")))
    (is (format nil "~a://~a:~a/"
                (quri:uri-scheme uri)
                (quri:uri-host uri)
                (quri:uri-port uri))
        cl-selenium::*uri*)
    (is (quri:uri-path uri)
        (format nil "~a/session" cl-selenium::*prefix*))))

(defparameter *base-url* "https://www.google.com?hl=en")
(defparameter *headless* '((:chrome-options . ((:args . #("--headless"))))))
(setf *timeout* 5)

(defmacro with-base-session (&body body)

  `(with-session (:additional-capabilities *headless*)
     (setf (url) *base-url*)
     ,@body))

(subtest "session"
  (let (session)
    (ok (setf session (make-session :additional-capabilities *headless*)))
    (ok (delete-session session))))

(subtest "url"
  (with-session (:additional-capabilities *headless*)
    (setf (url) *base-url*)
    (like (url) "https://www.google.*")))

(subtest "back"
  (with-base-session
    (like (url) "https://www.google.*")
    (ok (back))))

(subtest "find-element"
  (with-base-session
    (ok (find-element "#lst-ib"))
    (is-type (find-element "[name=q]") 'cl-selenium::element)
    (ok (element-id (find-element "[name=btnK]")))))

(subtest "find-elements"
  (with-base-session
    (let ((elements (find-elements "input[type=text]")))
      (is-type elements 'list)
      (dolist (elem elements)
        (is-type elem 'cl-selenium::element)))))

(subtest "print-element"
  (with-base-session
    (let ((got (princ-to-string (find-element "#lst-ib")))
          (start "#<cl-selenium::element {id:")
          (end "} id=lst-ib>"))
      (is (subseq got 0 (length start)) start)
      (is (subseq got (- (length got) (length end))) end))))

(subtest "find-element-no-such-element"
  (with-base-session
    (is-error (find-element (gensym)) 'cl-selenium:no-such-element-error)))

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

(subtest "element-tagname"
  (with-base-session
    (is (element-tagname (find-element "[name=q]")) "input")))

(subtest "element-attribute"
  (with-base-session
    (let ((input (find-element "[name=q]")))
      (element-send-keys input "cl-cl-selenium-webdriver")
      (is (element-attribute input "value") "cl-cl-selenium-webdriver"))))

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

(subtest "logs"
  (with-base-session
    (ok (log-types))
    (ok (logs "client"))))

(subtest "execute-script"
  (with-base-session
    (is (execute-script "return arguments[0] + arguments[1];" '(2 3)) 5)
    (is (execute-script "return 42" nil) 42)))

(subtest "mouse-move-to"
  (with-base-session
    (ok (mouse-move-to 0 0 :element (find-element "[name=btnK]")))
    (ok (mouse-move-to 100 100))))

(subtest "mouse-click"
  (with-base-session
    (element-send-keys (active-element) "cl-selenium-webdriver")
    (sleep 0.5)
    (ok (mouse-move-to 20 0 :element (find-element "[name=btnK]")))
    (ok (mouse-click :left))
    (ok (wait-for "#resultStats"))))

(subtest "take-screenshot"
  (with-base-session
    (with-open-file (stream "/tmp/test.html" :direction :output :if-exists :supersede)
      (format stream "<img src='data:image/png;base64,~a'>" (screenshot)))))

(finalize)
