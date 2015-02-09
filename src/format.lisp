(in-package :cl-user)
(defpackage clipper.format
  (:use :cl
        :clipper.config
        :clipper.database)
  (:export :*format-keys*))
(in-package :clipper.format)

(syntax:use-syntax :annot)

(defvar *format-keys*
  (list :ID #'picture-id
        :URL #'picture-url
        :FILE-NAME #'picture-image-file-name-without-extension
        :EXTENSION #'picture-extension))

@export
(defun store-format (picture)
  (ecase (clipper-config-store-type *clipper-config*)
    (:local (local-store-format picture))
    (:s3 (s3-store-format picture))))

(defun local-store-format (picture)
  (declare (ignore picture))) ;; TODO: local format

(defun s3-store-format (picture)
  (replace-format picture))

(defun replace-format (picture)
  (loop with format = (clipper-config-format *clipper-config*)
        for (key fn) on *format-keys* by #'cddr
        do (setf format
                 (ppcre:regex-replace-all (format nil "~s" key)
                                          format
                                          (format nil "~a" (funcall fn picture))))
        finally (return format)))

@export
(defun retrieve-url (picture)
  (ecase (clipper-config-store-type *clipper-config*)
    (:local (local-retrieve-url picture))
    (:s3 (s3-retrieve-url picture))))

(defun local-retrieve-url (picture)
  (declare (ignore picture))) ;; TODO: local url

(defun s3-retrieve-url (picture)
  (let ((uri (quri.uri.http:make-uri-https
              :host (clipper-config-s3-endpoint *clipper-config*)
              :path (concatenate 'string (format nil "/~a/" (clipper-config-s3-bucket-name *clipper-config*))
                                 (store-format picture)))))
    (quri:render-uri uri)))

