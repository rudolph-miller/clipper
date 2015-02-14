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
(defvar *supported-content-types* '(:jpg :jpeg :png))

@export
(defgeneric attach-image (object &key url image path-name file-name)
  (:method (object &key url image path-name file-name)
    (cond
      (url (let* ((file-name (or file-name (lastcar (split-sequence #\/ (uri-path (uri url))))))
                  (content-type (extension->type (get-extension file-name)))
                  (image (drakma:http-request url)))
             (setf (clip-url object) url)
             (%attach-image object image file-name content-type)))
      (path-name (let* ((file-name (or file-name (lastcar (split-sequence #\/ (enough-namestring path-name)))))
                        (content-type (extension->type (get-extension file-name)))
                        (image (read-image-file path-name)))
                   (%attach-image object image file-name content-type)))
      (image (unless file-name
               (error '<clipper-incomplete-for-attach-image> :type :image :args '(:file-name)))
             (let ((content-type (extension->type (get-extension file-name))))
               (%attach-image object image file-name content-type)))
      (t (if (clip-url object)
             (attach-image object :url (clip-url object) :file-name file-name)
             (error '<clipper-no-source-specified> :object object))))))

(defun %attach-image (object image file-name content-type)
  (unless (find content-type *supported-content-types*)
    (error '<clipper-unsupported-content-type> :content-type content-type))
  (setf (clip-image-file-name object) file-name)
  (setf (clip-image-content-type object) (format nil "image/~(~a~)" content-type))
  (store-image object image (clipper-config-store-type *clipper-config*)))

(defmethod attach-image :around (object &key url image path-name file-name)
  (declare (ignore url image path-name file-name))
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
