(in-package :cl-user)
(defpackage clipper.s3
  (:use :cl
        :clipper.config
        :clipper.format
        :clipper.database
        :zs3))
(in-package :clipper.s3)

(syntax:use-syntax :annot)

(defclass environment-credentials ()())
(setf *credentials* (make-instance 'environment-credentials))

(defmethod access-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (clipper-config-aws-access-key *clipper-config*))

(defmethod secret-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (clipper-config-aws-secret-key *clipper-config*))

@export
(defun upload (object key)
  (put-object object (clipper-config-s3-bucket-name *clipper-config*) key))

(defmethod store-image (object image (type (eql :s3)))
  (upload image (store-format object)))
