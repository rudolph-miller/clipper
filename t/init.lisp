(in-package :cl-user)
(defpackage clipper-test.init
  (:use :cl
        :integral)
  (:import-from :clipper.image
                :read-image-to-vector))
(in-package :clipper-test.init)

(syntax:use-syntax :annot)

@export
(defvar *clipper-image-directory*
  (merge-pathnames #P"t/images" (asdf:system-source-directory :clipper)))

@export
(defvar *clipper-image-test-file-path-name*
  (merge-pathnames #P"t/images/lisplogo_alien_256.png" (asdf:system-source-directory :clipper)))

@export
(defvar *clipper-image-test-file-type* :png)

@export
(defvar *clipper-image-test-file-name* "splogo_alien_256.png")

@export
(defun connect-to-testdb ()
  (connect-toplevel :mysql
                    :database-name "clipper_test"
                    :username "root"))

@export
(defmacro tests-with-http-request (&body body)
  `(let ((%http-request (symbol-function 'dex:get)))
     (setf (symbol-function 'dex:get)
           (lambda (url &key &allow-other-keys)
             (declare (ignore url))
             (with-open-file (input *clipper-image-test-file-path-name*
                                    :direction :input
                                    :element-type '(unsigned-byte 8))
               (read-image-to-vector input))))

     ,@body

     (setf (symbol-function 'dex:get) %http-request)))
