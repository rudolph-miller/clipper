(in-package :cl-user)
(defpackage clipper.image
  (:use :cl
        :clipper.database))
(in-package :clipper.image)

(syntax:use-syntax :annot)

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
