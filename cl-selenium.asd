(asdf:defsystem #:cl-selenium
  :description "cl-selenim-webdriver is a binding library to the Selenium 2.0"
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"
  :depends-on (:dexador :quri :cl-json :alexandria :split-sequence)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "errors")
                 (:file "session")
                 (:file "keys")
                 (:file "mouse")
                 (:file "http")
                 (:file "selenium")
                 (:file "utils"))))
  :in-order-to ((test-op (test-op cl-selenium-test))))
