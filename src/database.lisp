(in-package :cl-user)
(defpackage clipper.database
  (:use :cl
        :clipper.config
        :clipper.error
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

@export
(defgeneric attach-image (object &optional src)
  (:method (object &optional src)
    (unless src
      (if (clip-url object)
          (setf src (clip-url object))
          (error "No source specified")))
    (etypecase src
      (string
       (let* ((image-file-name (lastcar (split-sequence #\/ (uri-path (uri src)))))
              (image-content-type (concatenate 'string "image/" (lastcar (split-sequence #\. image-file-name))))
              (image (drakma:http-request src)))
         (setf (clip-url object) src)
         (setf (clip-image-file-name object) image-file-name)
         (setf (clip-image-content-type object) image-content-type)
         (store-image object image (clipper-config-store-type *clipper-config*))))
      (simple-vector (error "There is not method of attach-image with raw image.")))))

(defmethod attach-image :around (object &optional src)
  (declare (ignore src))
  (when (call-next-method) object))

@export
(defgeneric store-image (object image type)
  (:method (object image type)
    (declare (ignore image object))
    (error '<clipper-invalid-store-type> :type type)))
