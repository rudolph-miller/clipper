(in-package :cl-user)
(defpackage clipper.format
  (:use :cl
        :clipper.config
        :clipper.database)
  (:export :*format-keys*))
(in-package :clipper.format)

(syntax:use-syntax :annot)

(defvar *format-keys*
  (list :ID #'clip-id
        :URL #'clip-url
        :FILE-NAME #'clip-image-file-name-without-extension
        :EXTENSION #'clip-extension))

@export
(defun store-format (object)
  (ecase (clipper-config-store-type *clipper-config*)
    (:local (local-store-format object))
    (:s3 (s3-store-format object))))

(defun local-store-format (object)
  (declare (ignore object))) ;; TODO: local format

(defun s3-store-format (object)
  (replace-format object))

(defun replace-format (object)
  (loop with format = (clipper-config-format *clipper-config*)
        for (key fn) on *format-keys* by #'cddr
        do (setf format
                 (ppcre:regex-replace-all (format nil "~s" key)
                                          format
                                          (format nil "~a" (funcall fn object))))
        finally (return format)))

@export
(defun retrieve-url (object)
  (ecase (clipper-config-store-type *clipper-config*)
    (:local (local-retrieve-url object))
    (:s3 (s3-retrieve-url object))))

(defun local-retrieve-url (object)
  (declare (ignore object))) ;; TODO: local url

(defun s3-retrieve-url (object)
  (let ((uri (quri.uri.http:make-uri-https
              :host (clipper-config-s3-endpoint *clipper-config*)
              :path (concatenate 'string (format nil "/~a/" (clipper-config-s3-bucket-name *clipper-config*))
                                 (store-format object)))))
    (quri:render-uri uri)))

