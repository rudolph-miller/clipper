(in-package :cl-user)
(defpackage clipper.database
  (:use :cl
        :clipper.config
        :annot.class
        :integral
        :quri)
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

@export
(defun picture-id (picture)
  (slot-value picture (or (clipper-config-id-slot *clipper-config*) 'id)))

@export
(defun picture-url (picture)
  (slot-value picture (or (clipper-config-url-slot *clipper-config*) 'url)))

@export
(defun picture-image-file-name (picture)
  (slot-value picture (or (clipper-config-image-file-name-slot *clipper-config*) 'image-file-name)))

@export
(defun picture-image-content-type (picture)
  (slot-value picture (or (clipper-config-image-content-type-slot *clipper-config*) 'image-content-type)))

@export
(defun picture-image-file-size (picture)
  (slot-value picture (or (clipper-config-image-file-size-slot *clipper-config*) 'image-file-size)))

@export
(defun picture-image-file-name-without-extension (picture)
  (let ((splitted (split-sequence #\. (picture-image-file-name picture))))
    (apply #'concatenate 'string (subseq splitted 0 (1- (length splitted))))))

@export
(defun picture-extension (picture)
  (lastcar (split-sequence #\. (picture-image-file-name picture))))

@export
(defun create-picture (&key url) ;; TODO: by local file
  (create-picture-by-url url))

(defun create-picture-by-url (url)
  (let* ((image-file-name (lastcar (split-sequence #\/ (uri-path (uri url)))))
         (image-content-type (concatenate 'string "image/" (lastcar (split-sequence #\. image-file-name))))
         (image (drakma:http-request url))
         (picture (create-dao (or (clipper-config-clipper-class *clipper-config*) 'picture)
                              :url url
                              :image-file-name image-file-name
                              :image-content-type image-content-type)))
    (when (attach-image picture image)
      picture)))

(defun attach-image (picture image)
  (store-object image (clipper-config-store-type *clipper-config*) picture))

@export
(defgeneric store-object (object type picture))
