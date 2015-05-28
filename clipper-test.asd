#|
  This file is a part of clipper project.
  Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage clipper-test-asd
  (:use :cl :asdf))
(in-package :clipper-test-asd)

(defsystem clipper-test
  :author "Rudolph-Miller"
  :license "MIT"
  :description "Tests of Clipper."
  :depends-on (:clipper
               :integral
               :prove)
  :components ((:module "t"
                :components
                ((:file "init")
                 (:test-file "clipper")
                 (:test-file "config")
                 (:test-file "format")
                 (:test-file "database")
                 (:test-file "image")
                 (:test-file "local")
                 (:test-file "s3"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
