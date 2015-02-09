(in-package :cl-user)
(defpackage clipper
  (:use :cl)
  (:import-from :clipper.config
                :setup-clipper)
  (:import-from :clipper.format
                :retrieve-url)
  (:import-from :clipper.database
                :create-picture)
  (:export :setup-clipper
             :create-picture
             :image-url))
(in-package :clipper)

(defun image-url (object)
  (retrieve-url object))
