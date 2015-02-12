(in-package :cl-user)
(defpackage clipper-test.format
  (:use :cl
        :prove
        :integral
        :clipper-test.init
        :clipper
        :clipper.database
        :clipper.format
        :clipper.config))
(in-package :clipper-test.format)

(plan 4)

(connect-to-testdb)

(defclass picture ()
  ((id :col-type (:integer 11)
       :primary-key t
       :auto-increment t
       :not-null t
       :initarg :id)
   (image-file-name :col-type (:varchar 255)
                    :initarg :image-file-name)
   (image-content-type :col-type (:varchar 255)
                       :initarg :image-content-type)
   (image-file-size :col-type (:integer 11)
                    :initarg :image-file-size)
   (url :type string
        :initarg :url))
  (:metaclass <dao-table-class>)
  (:table-name "pictures"))

(execute-sql "DROP TABLE IF EXISTS pictures")
(execute-sql (table-definition 'picture))

(let ((object (create-dao 'picture :image-file-name "lisp-alien.png"
                                :image-content-type "image/png"
                                :image-file-size 100
                                :url "http://lisp-alien.org/lisp-alien.png")))
  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":ID")
  (is (store-format object)
      (format nil "~a" (clip-id object)))

  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":URL")
  (is (store-format object)
      (format nil "~a" (clip-url object)))

  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":FILE-NAME")
  (is (store-format object)
      (format nil "~a" (clip-image-file-name-without-extension object)))
  
  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":EXTENSION")
  (is (store-format object)
      (format nil "~a" (clip-extension object))))

(finalize)
