(in-package :cl-user)
(defpackage clipper.config
  (:use :cl
        :annot.class)
  (:export :*clipper-config*))
(in-package :clipper.config)

(syntax:use-syntax :annot)

(defvar *clipper-config* nil)

@export
@export-accessors
(defstruct clipper-config
  (store-type :local)
  (aws-access-key)
  (aws-secret-key)
  (s3-bucket-name)
  (s3-endpoint)
  (clipper-class)
  (id-slot)
  (url-slot)
  (image-file-name-slot)
  (image-content-type-slot)
  (image-file-size-slot)
  (format "/:ID/:FILE-NAME.:EXTENSION"))

@export
(defun setup-clipper (&rest initargs &key store-type aws-access-key aws-secret-key s3-endpoint
                                       s3-bucket-name clipper-class id-slot url-slot image-file-name-slot
                                       image-content-type-slot image-file-size-slot format)
  (declare (ignore store-type id-slot url-slot image-file-name-slot image-content-type-slot image-file-size-slot format))
  (flet ((fill-slot (class slot-name key)
           (unless (getf initargs key) (setf initargs (append (list key (find-slot-by-the-name class slot-name))
                                                              initargs)))))
    (when clipper-class
      (fill-slot clipper-class "ID" :id-slot)
      (fill-slot clipper-class "URL" :url-slot)
      (fill-slot clipper-class "IMAGE-FILE-NAME" :image-file-name-slot)
      (fill-slot clipper-class "IMAGE-CONTENT-TYPE" :image-content-type-slot)
      (fill-slot clipper-class "IMAGE-FILE-SIZE" :image-file-size-slot))
    (when (and aws-access-key aws-secret-key s3-endpoint s3-bucket-name)
      (setf initargs (append (list :store-type :s3) initargs)))
  (setf *clipper-config* (apply #'make-clipper-config initargs))))

(defun find-slot-by-the-name (class name)
   (find name (mapcar #'c2mop:slot-definition-name (c2mop:class-direct-slots class))
         :test #'equal
         :key #'symbol-name))
