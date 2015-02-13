(in-package :cl-user)
(defpackage clipper.image
  (:use :cl
        :quri
        :fast-io
        :opticl
        :clipper.error
        :clipper.config
        :clipper.database)
  (:import-from :alexandria
                :make-keyword
                :lastcar)
  (:import-from :split-sequence
                :split-sequence))
(in-package :clipper.image)

(syntax:use-syntax :annot)

@export
(defgeneric attach-image (object &optional src)
  (:method (object &optional src)
    (unless src
      (if (and (slot-boundp object (clipper-config-url-slot *clipper-config*)) (clip-url object))
          (setf src (clip-url object))
          (error '<clipper-no-source-specified> :object object)))
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

(defmethod store-image :around (object image type)
  (if (or (clipper-config-width *clipper-config*) (clipper-config-height *clipper-config*))
      (progn
        (setf image (convert-image image (extension->type (clip-extension object))))
        (call-next-method))
      (call-next-method)))

@export
(defun convert-image (image-vec type)
  (let* ((pathname (cl-fad:with-output-to-temporary-file (out :direction :io
                                                             :element-type '(unsigned-byte 8)
                                                              :template (temporary-file-template type))
                    (write-image-to-out image-vec out)))
        (image (read-image-file pathname))
        (width (clipper-config-width *clipper-config*))
        (height (clipper-config-height *clipper-config*)))
    (write-image-file pathname (fit-image-into image :x-max width :y-max height))
    (with-open-file (input pathname
                           :direction :input
                           :element-type '(unsigned-byte 8))
      (read-image-to-vector input))))

(defun temporary-file-template (type)
  (format nil "temporary-files:%.~a" (symbol-name type)))

@export
(defun write-image-to-out (image out)
  (loop for byte across image
        do (write-byte byte out)
        finally (return t)))

@export
(defun read-image-to-vector (input)
  (with-fast-output (buffer :vector)
    (loop for byte = (read-byte input nil nil)
          while byte
          do (fast-write-byte byte buffer)
          finally (return buffer))))

(defun extension->type (extension)
  (make-keyword (string-upcase extension)))
