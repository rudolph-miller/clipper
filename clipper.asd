#|
  This file is a part of clipper project.
  Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
|#

#|
  Author: Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage clipper-asd
  (:use :cl :asdf))
(in-package :clipper-asd)

(defsystem clipper
  :version "0.1"
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:cl-syntax-annot
               :integral
               :drakma
               :quri
               :split-sequence
               :alexandria
               :zs3)
  :components ((:module "src"
                :components
                ((:file "clipper" :depends-on ("database" "s3" "local" "config" "format" "error"))
                 (:file "database" :depends-on ("config" "error"))
                 (:file "s3" :depends-on ("database" "config" "format"))
                 (:file "local" :depends-on ("database" "config" "format"))
                 (:file "config")
                 (:file "format" :depends-on ("config" "database"))
                 (:file "error"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clipper-test))))
