(in-package :cl-user)
(defpackage clipper
  (:use :cl
        :annot.doc)
  (:import-from :clipper.error
                :<clipper-invalid-store-type>
                :<clipper-incomplete-config>
                :<clipper-incomplete-local-config>
                :<clipper-incomplete-s3-config>)
  (:import-from :clipper.config
                :*clipper-config*
                :setup-clipper
                :clipper-config-store-type)
  (:import-from :clipper.format
                :*format-keys*
                :store-format
                :retrieve-url)
  (:import-from :clipper.image
                :attach-image)
  (:export :<clipper-invalid-store-type>
           :<clipper-incomplete-config>
           :<clipper-incomplete-local-config>
           :<clipper-incomplete-s3-config>
           :setup-clipper
           :*clipper-config*
           :*format-keys*
           :store-format
           :attach-image
           :image-url))
(in-package :clipper)

(syntax:use-syntax :annot)

@doc
"Return URL for object."
(defun image-url (object)
  (retrieve-url object (clipper-config-store-type *clipper-config*)))
