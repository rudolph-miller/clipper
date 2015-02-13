(in-package :cl-user)
(defpackage clipper.local
  (:use :cl
        :clipper.config
        :clipper.format
        :clipper.database
        :clipper.image))
(in-package :clipper.local)

(syntax:use-syntax :annot)

(defmethod store-image (object image (type (eql :local)))
  (with-open-file (out (ensure-directories-exist (image-pathname object))
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8)
                       :external-format :utf8)
    (write-image-to-out image out)))

@export
(defun image-pathname (object)
  (merge-pathnames (store-format object) (clipper-config-image-directory *clipper-config*)))


(defmethod retrieve-url (object (type (eql :local)))
  (format nil "~a~a" (clipper-config-prefix *clipper-config*)
          (enough-namestring (image-pathname object) (clipper-config-relative *clipper-config*))))
