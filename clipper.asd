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
               :zs3
               :cl-fad
               :cl-gd)
  :components ((:module "src"
                :components
                ((:file "clipper" :depends-on ("database" "image" "s3" "local" "config" "format" "error"))
                 (:file "database" :depends-on ("config" "error"))
                 (:file "image" :depends-on ("database"))
                 (:file "s3" :depends-on ("database" "image" "format"))
                 (:file "local" :depends-on ("database" "image" "format"))
                 (:file "config" :depends-on ("error"))
                 (:file "format" :depends-on ("database"))
                 (:file "error"))))
  :description "File attachment library."
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
