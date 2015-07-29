(in-package :cl-user)
(defpackage clipper.config
  (:use :cl
        :annot.doc
        :clipper.error
        :annot.class)
  (:export :*clipper-config*))
(in-package :clipper.config)

(syntax:use-syntax :annot)

(defvar *s3-require-list* (list :aws-access-key :aws-secret-key :s3-endpoint :s3-bucket-name))
(defvar *local-requrie-list* (list :image-directory))

@doc
"Global variable for config object."
@export
(defvar *clipper-config* nil)

@export
@export-accessors
(defstruct clipper-config
  (store-type)
  (image-directory)
  (relative "")
  (prefix "")
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
  (format "/:ID/:FILE-NAME.:EXTENSION")
  (width)
  (height))

@doc
"Setup clipper. You must call this before use clipper."
@export
(defun setup-clipper (&rest initargs &key store-type image-directory relative prefix aws-access-key aws-secret-key
                                       s3-endpoint s3-bucket-name clipper-class id-slot url-slot image-file-name-slot
                                       image-content-type-slot image-file-size-slot format width height)
  (declare (ignore relative prefix aws-access-key aws-secret-key s3-endpoint s3-bucket-name id-slot url-slot
                   image-file-name-slot image-content-type-slot image-file-size-slot format width height))
  (flet ((raise-error-if-incomplete (require-slots error)
           (let ((slots (loop with result = nil
                              for slot in require-slots
                              unless (getf initargs slot) do (push slot result)
                                finally (return result))))
             (when (> (length slots) 0)
               (error error :slot-list slots)))))
    (unless store-type (error '<clipper-incomplete-config> :slot-list (list :store-type)))
    (if clipper-class
        (progn
          (when (symbolp clipper-class)
            (setf (getf initargs :clipper-class) (find-class clipper-class)))
          (setf initargs (complete-slot-of-class initargs)))
        (error '<clipper-incomplete-config> :slot-list (list :clipper-class)))
    (when image-directory (setf initargs (append (list :image-directory (cl-fad:pathname-as-directory image-directory)) initargs)))
    (case store-type
      (:local (raise-error-if-incomplete *local-requrie-list* '<clipper-incomplete-local-config>))
      (:s3 (raise-error-if-incomplete *s3-require-list* '<Clipper-incomplete-s3-config>)))
    (setf *clipper-config* (apply #'make-clipper-config initargs))))

(defun complete-slot-of-class (initargs)
  (flet ((fill-slot (class slot-name key)
           (unless (getf initargs key)
             (let ((slot (find-slot-by-the-name class slot-name)))
               (if slot
                   (setf initargs
                         (append (list key slot) initargs))
                   (error "No slot found"))))))
    (let ((clipper-class (getf initargs :clipper-class)))
      (fill-slot clipper-class "ID" :id-slot)
      (fill-slot clipper-class "URL" :url-slot)
      (fill-slot clipper-class "IMAGE-FILE-NAME" :image-file-name-slot)
      (fill-slot clipper-class "IMAGE-CONTENT-TYPE" :image-content-type-slot)
      (fill-slot clipper-class "IMAGE-FILE-SIZE" :image-file-size-slot))))

(defun find-slot-by-the-name (class name)
  (find name (mapcar #'c2mop:slot-definition-name (c2mop:class-direct-slots class))
        :test #'equal
        :key #'symbol-name))
