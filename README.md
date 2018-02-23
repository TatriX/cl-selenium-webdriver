# CL Selenium WebDriver
CL Selenium WebDriver is a binding library to the Selenium 2.0

## Warning
This software is in development. The APIs will be likely to change.

## Usage
```lisp
;; see examples/*.lisp and t/*.lisp
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-selenium))

(defpackage go-test
  (:use :cl :cl-selenium))

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
(ql:quickload :cl-selenium)
```

You need a running instance of selenium-server-standalone.

[Download](http://www.seleniumhq.org/download/) it and run:
```
curl -L0 https://goo.gl/SP94ZB -o selenium-server-standalone.jar
java -jar selenium-server-standalone.jar
```

## Utils

There is a `:cl-selenium-utils` package which should reduce boilerplate. For example:
```lisp
(defpackage my-test
  (:use :cl :cl-selenium)
  (:import-from :cl-selenium-utils
                :send-keys
                :click
                :wait-for
                :classlist))

(in-package :my-test)

(with-session ()
  (setf (url) "http://google.com")
  (send-keys "cl-selenium-webdriver")
  (click "[name=btnK]")
  (wait-for "#resultStats"))

```

### Interactive session
You can just start the session and control it from your repl:
```lisp
(in-package :my-test)

(start-interactive-session)

(setf (url) "http://google.com")
(send-keys "cl-selenium-webdriver")
(send-keys (key :enter))
(classlist "#slim_appbar") ; prints ("ab_tnav_wrp")

(stop-interactive-session)
```

### Utils API conventions
If utility function needs an element to work on it defaults to `(active-element)`.
```lisp
(click) ; click on the current active element.
```
You can also pass a css selector as a last parameter.
```lisp
(print (id "#submit")) ; print id the of matched element

(assert (= (first (classlist "div")) "first-div-ever"))
```

To change default element you can:
```lisp
(setf cl-selenium-utils:*default-element-func* (lambda () (find-element "input[type=submit]"))
```


### Waiting for the reaction
Often you need to wait for some action to be done. For example if you
do a `(click)` on the button to load search results, you need to wait
them to load.
```lisp
(wait-for ".search-result" :timeout 10) ; wait 10 seconds
```
Timeout defaults to 30 seconds. You can globally change it:
```lisp
(setf cl-selenium-utils:*timeout* 3)
```

## Copyright

Licensed under the MIT License.
