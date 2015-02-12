(in-package :cl-user)
(defpackage clipper-test.local
  (:use :cl
        :prove
        :integral
        :clipper-test.init
        :clipper
        :clipper.config
        :clipper.format
        :clipper.database))
(in-package :clipper-test.local)

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

(let ((pic (create-dao 'picture :image-file-name "lisp-alien.png"
                                :image-content-type "image/png"
                                :image-file-size 100
                                :url "http://lisp-alien.org/lisp-alien.png")))

  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":ID/:FILE-NAME.:EXTENSION")
  (is (store-format pic)
      (format nil "~a/~a.~a" (clip-id pic) (clip-image-file-name-without-extension pic) (clip-extension pic)))
  (is (image-url pic)
       (format nil "~a~a/~a.~a"
               (clipper-config-image-directory *clipper-config*)
               (clip-id pic)
               (clip-image-file-name-without-extension pic)
               (clip-extension pic)))

  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":ID/:FILE-NAME.:EXTENSION"
                 :relative (asdf:system-source-directory :clipper))
  (is (image-url pic)
       (format nil "~a~a/~a.~a"
               (enough-namestring (clipper-config-image-directory *clipper-config*) (clipper-config-relative *clipper-config*))
               (clip-id pic)
               (clip-image-file-name-without-extension pic)
               (clip-extension pic)))

  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture)
                 :format ":ID/:FILE-NAME.:EXTENSION"
                 :relative (asdf:system-source-directory :clipper)
                 :prefix "http://localhost:3000/")
  (is (image-url pic)
       (format nil "~a~a~a/~a.~a"
               (clipper-config-prefix *clipper-config*)
               (enough-namestring (clipper-config-image-directory *clipper-config*) (clipper-config-relative *clipper-config*))
               (clip-id pic)
               (clip-image-file-name-without-extension pic)
               (clip-extension pic))))

(finalize)
