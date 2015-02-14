(in-package :cl-user)
(defpackage clipper-test.init
  (:use :cl
        :integral))
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
