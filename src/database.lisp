(in-package :cl-user)
(defpackage clipper.database
  (:use :cl
        :clipper.config
        :annot.class
        :integral)
  (:import-from :alexandria
                :lastcar)
  (:import-from :split-sequence
                :split-sequence)
  (:export :*clipper-class*
           :*id-slot*
           :*url-slot* 
           :*image-file-name-slot*
           :*image-content-type-slot* 
           :*image-file-size-slot*
           :*image-updated-at*))
(in-package :clipper.database)

(syntax:use-syntax :annot)

@export
@export-accessors
(defclass clip ()
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
  (:table-name "clips"))

@export
(defun clip-id (clip)
  (slot-value clip (or (clipper-config-id-slot *clipper-config*) 'id)))

@export
(defun clip-url (clip)
  (slot-value clip (or (clipper-config-url-slot *clipper-config*) 'url)))

@export
(defun (setf clip-url) (url clip)
  (setf (slot-value clip (or (clipper-config-url-slot *clipper-config*) 'url)) url))

@export
(defun clip-image-file-name (clip)
  (slot-value clip (or (clipper-config-image-file-name-slot *clipper-config*) 'image-file-name)))

@export
(defun (setf clip-image-file-name) (image-file-name clip)
  (setf (slot-value clip (or (clipper-config-image-file-name-slot *clipper-config*) 'image-file-name))
        image-file-name))

@export
(defun clip-image-content-type (clip)
  (slot-value clip (or (clipper-config-image-content-type-slot *clipper-config*) 'image-content-type)))

@export
(defun (setf clip-image-content-type) (image-content-type clip)
  (setf (slot-value clip (or (clipper-config-image-content-type-slot *clipper-config*) 'image-content-type))
        image-content-type))

@export
(defun clip-image-file-size (clip)
  (slot-value clip (or (clipper-config-image-file-size-slot *clipper-config*) 'image-file-size)))

@export
(defun (setf clip-image-file-size) (image-file-size clip)
  (setf (slot-value clip (or (clipper-config-image-file-size-slot *clipper-config*) 'image-file-size))
        image-file-size))

@export
(defun clip-image-file-name-without-extension (clip)
  (let ((splitted (split-sequence #\. (clip-image-file-name clip))))
    (apply #'concatenate 'string (subseq splitted 0 (1- (length splitted))))))

@export
(defun clip-extension (clip)
  (lastcar (split-sequence #\. (clip-image-file-name clip))))
