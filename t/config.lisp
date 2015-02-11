(in-package :cl-user)
(defpackage clipper-test.config
  (:use :cl
        :prove
        :integral
        :clipper-test.init
        :clipper
        :clipper.config))
(in-package :clipper-test.config)

(plan 3)

(subtest "store-type :local"
  (is-error (setup-clipper :store-type :local)
            '<clipper-incomplete-local-config>)
  (ok (setup-clipper :store-type :local :image-directory *clipper-image-directory*))
  (is (clipper-config-image-directory (setup-clipper :store-type :local :image-directory #P"sample"))
      #P"sample/"))

(subtest "store-type :s3"
  (is-error (setup-clipper :store-type :s3)
            '<clipper-incomplete-s3-config>)
  (is-error (setup-clipper :store-type :s3
                           :aws-secret-key "sample-secret-key"
                           :s3-endpoint "sample-endpoint"
                           :s3-bucket-name "sample-bucket-name")
            '<clipper-incomplete-s3-config>)
  (is-error (setup-clipper :store-type :s3
                           :aws-access-key "sample-access-key"
                           :s3-endpoint "sample-endpoint"
                           :s3-bucket-name "sample-bucket-name")
            '<clipper-incomplete-s3-config>)
  (is-error (setup-clipper :store-type :s3
                           :aws-access-key "sample-access-key"
                           :aws-secret-key "sample-secret-key"
                           :s3-bucket-name "sample-bucket-name")
            '<clipper-incomplete-s3-config>)
  (is-error (setup-clipper :store-type :s3
                           :aws-access-key "sample-access-key"
                           :aws-secret-key "sample-secret-key"
                           :s3-endpoint "sample-endpoint")
            '<clipper-incomplete-s3-config>)
  (ok (setup-clipper :store-type :s3
                     :aws-access-key "sample-access-key"
                     :aws-secret-key "sample-secret-key"
                     :s3-endpoint "sample-endpoint"
                     :s3-bucket-name "sample-bucket-name")))

(subtest "complete class"
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

  (setup-clipper :store-type :local
                 :image-directory *clipper-image-directory*
                 :clipper-class (find-class 'picture))

  (is (clipper-config-id-slot *clipper-config*) 'id)
  (is (clipper-config-url-slot *clipper-config*) 'url)
  (is (clipper-config-image-file-name-slot *clipper-config*) 'image-file-name)
  (is (clipper-config-image-content-type-slot *clipper-config*) 'image-content-type)
  (is (clipper-config-image-file-size-slot *clipper-config*) 'image-file-size))

(finalize)
