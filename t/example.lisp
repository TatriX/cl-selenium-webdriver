(in-package :selenium)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(prove))
  (use-package :prove))


(defparameter *code* "
package main
import \"fmt\"

func main() {
    fmt.Print(\"Hello WebDriver!\")
}")

(plan 1)

(with-session (:browser :chrome)
  (setf (url) "http://play.golang.org/?simple=1")
  (let ((elem (find-element "#code" :by :css-selector)))
    (element-clear elem)
    (element-send-keys elem *code*))
  (element-click (find-element "#run")))

  (loop
     with div = (find-element "#output")
     for ouput = (element-text div)
     while (equal ouput "Waiting for remote server...")
     do (sleep 0.1)
     finally (like ouput "Hello WebDriver!.*")))

(finalize)
