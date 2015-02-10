(in-package :cl-user)
(defpackage clipper
  (:use :cl)
  (:import-from :clipper.config
                :setup-clipper)
  (:import-from :clipper.format
                :retrieve-url)
  (:import-from :clipper.database
                :attach-image)
  (:export :setup-clipper
           :attach-image
           :image-url))
(in-package :clipper)

(defun image-url (object)
  (retrieve-url object))
