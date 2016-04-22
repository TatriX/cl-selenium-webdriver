(asdf:defsystem #:selenium
  :description "cl-selenim-webdriver is a binding library to the Selenium 2.0"
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"
  :depends-on (:dexador :quri :cl-json :alexandria)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "selenium" :depends-on ("package")))))
  :in-order-to ((test-op (test-op selenium-test))))
