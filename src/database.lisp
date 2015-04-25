(in-package :cl-user)
(defpackage clipper.database
  (:use :cl
        :clipper.config
        :annot.class)
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
(defun clip-id (clip)
  (let ((id-slot (clipper-config-id-slot *clipper-config*)))
    (when (slot-boundp clip id-slot)
      (slot-value clip (or (clipper-config-id-slot *clipper-config*) 'id)))))

@export
(defun clip-url (clip)
  (let ((url-slot (clipper-config-url-slot *clipper-config*)))
    (when (slot-boundp clip url-slot)
      (slot-value clip url-slot))))

@export
(defun (setf clip-url) (url clip)
  (let ((url-slot (clipper-config-url-slot *clipper-config*)))
    (setf (slot-value clip url-slot) url)))

@export
(defun clip-image-file-name (clip)
  (let ((image-file-name-slot (clipper-config-image-file-name-slot *clipper-config*)))
    (when (slot-boundp clip image-file-name-slot)
      (slot-value clip image-file-name-slot))))

@export
(defun (setf clip-image-file-name) (image-file-name clip)
  (let ((image-file-name-slot (clipper-config-image-file-name-slot *clipper-config*)))
    (setf (slot-value clip image-file-name-slot) image-file-name)))

@export
(defun clip-image-content-type (clip)
  (let ((image-content-type-slot (clipper-config-image-content-type-slot *clipper-config*)))
    (when (slot-boundp clip image-content-type-slot)
      (slot-value clip image-content-type-slot))))

@export
(defun (setf clip-image-content-type) (image-content-type clip)
  (let ((image-content-type-slot (or (clipper-config-image-content-type-slot *clipper-config*) 'image-content-type)))
    (setf (slot-value clip image-content-type-slot) image-content-type)))

@export
(defun clip-image-file-size (clip)
  (let ((image-file-size-slot (clipper-config-image-file-size-slot *clipper-config*)))
    (when (slot-boundp clip image-file-size-slot)
      (slot-value clip image-file-size-slot))))

@export
(defun (setf clip-image-file-size) (image-file-size clip)
  (let ((image-file-size-slot (clipper-config-image-file-size-slot *clipper-config*)))
    (setf (slot-value clip image-file-size-slot) image-file-size)))

@export
(defun clip-image-file-name-without-extension (clip)
  (let ((splitted (split-sequence #\. (clip-image-file-name clip))))
    (apply #'concatenate 'string (subseq splitted 0 (1- (length splitted))))))

@export
(defun clip-extension (clip)
  (get-extension (clip-image-file-name clip)))

@export
(defun get-extension (file-name)
  (lastcar (split-sequence #\. file-name)))
