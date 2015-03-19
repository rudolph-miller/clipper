(in-package :cl-user)
(defpackage clipper.s3
  (:use :cl
        :clipper.config
        :clipper.format
        :clipper.database
        :clipper.image
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
  (let ((bucket (clipper-config-s3-bucket-name *clipper-config*)))
    (put-object object bucket key :public t)))

(defmethod store-image (object image (type (eql :s3)))
  (upload image (store-format object)))

(defmethod retrieve-url (object (type (eql :s3)))
  (let ((uri (quri.uri.http:make-uri-https
              :host (clipper-config-s3-endpoint *clipper-config*)
              :path (concatenate 'string (format nil "/~a/" (clipper-config-s3-bucket-name *clipper-config*))
                                 (store-format object)))))
    (quri:render-uri uri)))
