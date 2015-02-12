(in-package :cl-user)
(defpackage clipper-test.database
  (:use :cl
        :prove
        :integral
        :clipper-test.init
        :clipper
        :clipper.database))
(in-package :clipper-test.database)

(plan 11)

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

(setup-clipper :store-type :local
               :image-directory *clipper-image-directory*
               :clipper-class (find-class 'picture))

(let ((object (create-dao 'picture :image-file-name "lisp-alien.png"
                                :image-content-type "image/png"
                                :image-file-size 100
                                :url "http://lisp-alien.org/lisp-alien.png")))
  (is (clip-id object) (slot-value object 'id))
  (is (clip-image-file-name object) (slot-value object 'image-file-name))
  (is (clip-image-content-type object) (slot-value object 'image-content-type))
  (is (clip-image-file-size object) (slot-value object 'image-file-size))
  (is (clip-url object) (slot-value object 'url))
  (is (clip-extension object) "png")
  (is (clip-image-file-name-without-extension object) "lisp-alien")

  (setf (clip-image-file-name object) "other-alien.jpg")
  (setf (clip-image-content-type object) "image/jpg")
  (setf (clip-image-file-size object) 200)
  (setf (clip-url object) "http://lisp-alien.org/other-alien.jpg")
  (is (clip-image-file-name object) "other-alien.jpg")
  (is (clip-image-content-type object) "image/jpg")
  (is (clip-image-file-size object) 200)
  (is (clip-url object) "http://lisp-alien.org/other-alien.jpg"))

(finalize)
