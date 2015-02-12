(in-package :cl-user)
(defpackage clipper.local
  (:use :cl
        :clipper.config
        :clipper.format
        :clipper.database))
(in-package :clipper.local)

(syntax:use-syntax :annot)

(defmethod store-image (object image (type (eql :local)))
  (with-open-file (out (image-pathname object) :direction :output :if-exists :supersede :external-format :utf8)
    (loop for byte in object
          do (write-byte byte out))))

@export
(defun image-pathname (object)
  (merge-pathnames (store-format object) (clipper-config-image-directory *clipper-config*)))


(defmethod retrieve-url (object (type (eql :local)))
  (format nil "~a~a" (clipper-config-image-directory *clipper-config*) (store-format object)))
