(in-package :cl-user)
(defpackage clipper.config
  (:use :cl
        :clipper.error
        :annot.class)
  (:export :*clipper-config*))
(in-package :clipper.config)

(syntax:use-syntax :annot)

(defvar *clipper-config* nil)
(defconstant +s3-require-list+ '(:aws-access-key :aws-secret-key :s3-endpoint :s3-bucket-name))
(defconstant +local-requrie-list+ '(:root))

@export
@export-accessors
(defstruct clipper-config
  (store-type)
  (root)
  (project)
  (relative)
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
(defun setup-clipper (&rest initargs &key store-type root project relative aws-access-key aws-secret-key
                                       s3-endpoint s3-bucket-name clipper-class id-slot url-slot image-file-name-slot
                                       image-content-type-slot image-file-size-slot format)
  (declare (ignore relative aws-access-key aws-secret-key s3-endpoint s3-bucket-name id-slot
                   url-slot image-file-name-slot image-content-type-slot image-file-size-slot format))
  (when (not store-type) (error '<clipper-incomplete-config> :slot-list (list :store-type)))
  (flet ((raise-error-if-incomplete (require-slots error)
           (let ((slots (loop with result = nil
                              for slot in require-slots
                              unless (getf initargs slot) do (push slot result)
                                finally (return result))))
             (when (> (length slots) 0)
               (error error :slot-list slots)))))
    (when clipper-class (setf initargs (complete-slot-of-class initargs)))
    (when (and (eq store-type :local) (null root) project)
      (setf initargs (append (list :root (asdf:system-source-directory project))
                             initargs)))
    (ecase store-type
      (:local (raise-error-if-incomplete +local-requrie-list+ '<clipper-incomplete-local-config>))
      (:s3 (raise-error-if-incomplete +s3-require-list+ '<clipper-incomplete-s3-config>)))
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
