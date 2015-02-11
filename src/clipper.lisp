(in-package :cl-user)
(defpackage clipper
  (:use :cl)
  (:import-from :clipper.error
                :<clipper-invalid-store-type>
                :<clipper-incomplete-config>
                :<clipper-incomplete-local-config>
                :<clipper-incomplete-s3-config>)
  (:import-from :clipper.config
                :setup-clipper)
  (:import-from :clipper.format
                :retrieve-url)
  (:import-from :clipper.database
                :attach-image)
  (:export :<clipper-invalid-store-type>
           :<clipper-incomplete-config>
           :<clipper-incomplete-local-config>
           :<clipper-incomplete-s3-config>
           :setup-clipper
           :attach-image
           :image-url))
(in-package :clipper)

(defun image-url (object)
  (retrieve-url object))
