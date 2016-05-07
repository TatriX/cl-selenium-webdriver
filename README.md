# CL Selenium WebDriver
CL Selenium WebDriver is a binding library to the Selenium 2.0

## Warning
This software is in development. The APIs will be likely to change.

## Usage
```lisp
;; see examples/*.lisp and t/*.lisp
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :selenium))

(defpackage go-test
  (:use :cl :selenium))

(in-package :go-test)

(defparameter *code* "
package main
import \"fmt\"

func main() {
    fmt.Print(\"Hello WebDriver!\")
}")

(with-session ()
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

You need a running instance of selenium-server-standalone.

[Download](http://www.seleniumhq.org/download/) it and run:
```
curl -L0 http://goo.gl/IHP6Qw -o selenium-server-standalone.jar
java -jar selenium-server-standalone.jar
```

## Utils

There is `:selenium-utils` package which should reduce boilerplate. For example:
```lisp
(defpackage my-test
  (:use :cl :selenium)
  (:import-from :selenium-utils
                :send-keys
                :click
                :wait-for))

(in-package :my-test)

(with-session ()
  (setf (url) "http://google.com")
  (send-keys "cl-selenium-webdriver")
  (click "[name=btnG]")
  (wait-for "#resultStats"))

```
By default every util function (except `(wait-for)` for obvious reasons) will work on `(active-element)`.
They also accept css selector as a last parameter.

## Copyright

Licensed under the MIT License.
