(in-package :cl-user)
(defpackage clipper
  (:use :cl)
  (:import-from :clipper.config
                :setup-clipper)
  (:import-from :clipper.format
                :retrieve-url)
  (:import-from :clipper.database
                :create-picture-by-url)
  (:export :setup-clipper
             :create-picture-by-url
             :image-url))
(in-package :clipper)

(defun image-url (object)
  (retrieve-url object))
