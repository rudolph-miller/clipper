(in-package :cl-user)
(defpackage clipper-test.image
  (:use :cl
        :prove
        :integral
        :clipper.error
        :clipper-test.init
        :clipper
        :clipper.image)
  (:import-from :clipper.image
                :%attach-image))
(in-package :clipper-test.image)

(plan 5)

(with-open-file (input *clipper-image-test-file-path-name*
                       :direction :input
                       :element-type '(unsigned-byte 8))
  (let ((image (read-image-to-vector input)))
    (subtest "read-image-to-vector"
      (is-type image
               'simple-array
               "read-image-to-vector returned vector."))

    (subtest "write-image-to-out"
      (let ((pathname (cl-fad:with-output-to-temporary-file (out :direction :io
                                                                 :element-type '(unsigned-byte 8))
                        (write-image-to-out image out))))
        (ok (probe-file pathname))))

    (subtest "convert-image"
      (is-type (convert-image image *clipper-image-test-file-type*)
               'simple-array
               "convert-image returned vector."))))

(subtest "attach-image"
  (let ((%http-request (symbol-function 'drakma:http-request)))

    (setf (symbol-function 'drakma:http-request)
          (lambda (url)
            (declare (ignore url))
            (with-open-file (input *clipper-image-test-file-path-name*
                                   :direction :input
                                   :element-type '(unsigned-byte 8))
              (read-image-to-vector input))))

    (connect-to-testdb)

    (defclass picture ()
      ((id :col-type (:integer 11)
           :primary-key t
           :auto-increment t
           :not-null t
           :initarg :id)
       (image-file-name :col-type (:varchar 255)
                        :initarg :image-file-name)
       (image-content-type :col-type (:varchar 255)
                           :initarg :image-content-type)
       (image-file-size :col-type (:integer 11)
                        :initarg :image-file-size)
       (url :type string
            :initarg :url))
      (:metaclass <dao-table-class>)
      (:table-name "pictures"))

    (execute-sql "DROP TABLE IF EXISTS pictures")
    (execute-sql (table-definition 'picture))

    (setup-clipper :store-type :sample
                   :clipper-class (find-class 'picture)
                   :format ":ID/:FILE-NAME.:EXTENSION")

    (defmethod store-image (object image (type (eql :sample)))
      (declare (ignore object image type)) t)

    (subtest "no source"
      (let ((object (create-dao 'picture)))
        (is-error (attach-image object)
                  '<clipper-no-source-specified>)

        (setf (slot-value object 'url) "http://lisp-alien.org/lisp-alien.png")

        (ok (attach-image object))
        (ok (slot-value object 'url))
        (ok (slot-value object 'image-file-name))
        (ok (slot-value object 'image-content-type))))

    (subtest ":url"
      (let ((object (create-dao 'picture)))

        (setf (symbol-function 'drakma:http-request)
              (lambda (url)
                (declare (ignore url))
                (with-open-file (input *clipper-image-test-file-path-name*
                                       :direction :input
                                       :element-type '(unsigned-byte 8))
                  (read-image-to-vector input))))
        (ok (attach-image object :url "http://lisp-alien.org/lisp-alien.png"))
        (ok (slot-value object 'url))
        (ok (slot-value object 'image-file-name))
        (ok (slot-value object 'image-content-type))))

    (subtest ":path-name"
      (let ((object (create-dao 'picture)))
        (ok (attach-image object :path-name *clipper-image-test-file-path-name*))
        (ok (slot-value object 'image-file-name))
        (ok (slot-value object 'image-content-type))))

    (subtest ":image"
      (with-open-file (input *clipper-image-test-file-path-name*
                             :direction :input
                             :element-type '(unsigned-byte 8))
        (let ((object (create-dao 'picture))
              (image (read-image-to-vector input)))
          (is-error (attach-image object :image image)
                    '<clipper-incomplete-for-attach-image>)
          (ok (attach-image object :image image :file-name *clipper-image-test-file-name*))
          (ok (slot-value object 'image-file-name))
          (ok (slot-value object 'image-content-type)))))

    (setf (symbol-function 'drakma:http-request) %http-request)))

(subtest "supported-content-type"
  (let ((object (create-dao 'picture)))
    (%attach-image object "dummy-image" "dummy-file-name" :jpg)
    (is (slot-value object 'image-content-type)
        "image/jpg")

    (%attach-image object "dummy-image" "dummy-file-name" :jpeg)
    (is (slot-value object 'image-content-type)
        "image/jpeg")

    (%attach-image object "dummy-image" "dummy-file-name" :png)
    (is (slot-value object 'image-content-type)
        "image/png")))

(finalize)
