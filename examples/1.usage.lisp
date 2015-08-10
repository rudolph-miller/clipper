(in-package :cl-user)
(defpackage clipper.sample
  (:use :cl
        :integral
        :clipper))
(in-package :clipper.sample)

(connect-toplevel :mysql :database-name "clipper_sample" :username "root")

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

(setup-clipper :store-type :s3
               :aws-access-key (asdf::getenv "AWS_ACCESS_KEY")
               :aws-secret-key (asdf::getenv "AWS_SECRET_KEY")
               :s3-endpoint "s3-ap-northeast-1.amazonaws.com"
               :s3-bucket-name "clipper-sample"
               :clipper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "https://s3-ap-northeast-1.amazonaws.com/clipper-sample/1/lisplogo_alien_256.png"

(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :relative #P"/home/cl-user/common-lisp/clipper/"
               :prefix "http://localhost:3000/"
               :clipper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "http://localhsot:3000/images/2/lisplogo_alien_256.png"
