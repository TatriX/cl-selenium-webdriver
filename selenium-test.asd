(defsystem selenium-test
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"
  :depends-on (:selenium
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((module "t"
           :components ((:test-file "selenium")
                        (:test-file "utils"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
