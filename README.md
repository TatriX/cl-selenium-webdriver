# CL Selenium WebDriver
CL Selenium WebDriver is a binding library to the Selenium 2.0

## Warning
This software is in development. The APIs will be likely to change.

## Usage
```lisp
;; see t/example.lisp and t/selenium.lisp

(ql:quickload :selenium)
(in-package :selenium)

(defparameter *code* "
package main
import \"fmt\"

func main() {
    fmt.Print(\"Hello WebDriver!\")
}")

(with-session (:browser :chrome)
  (setf (url) "http://play.golang.org/?simple=1")
  (let ((elem (find-element "#code" :by :css-selector)))
    (element-clear elem)
    (element-send-keys elem *code*))
  (let ((btn (find-element "#run")))
    (element-click btn))

  (loop
     with div = (find-element "#output")
     for ouput = (element-text div)
     while (equal ouput "Waiting for remote server...")
     do (sleep 0.1)
     finally (print ouput)))
```
## Installation
```
git clone https://github.com/TatriX/cl-selenium-webdriver ~/quicklisp/local-projects/
(ql:quickload :selenium)
```

## Copyright

Licensed under the MIT License.
