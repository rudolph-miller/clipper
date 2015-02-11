(in-package :cl-user)
(defpackage clipper-test.init
  (:use :cl
        :integral))
(in-package :clipper-test.init)

(syntax:use-syntax :annot)

@export
(defvar *clipper-image-directory*
  (merge-pathnames #P"images" (asdf:system-source-directory :clipper)))

@export
(defun connect-to-testdb ()
  (disconnect-toplevel)
  (connect-toplevel :mysql
                    :database-name "clipper_test"
                    :username "root"))
