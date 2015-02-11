# Clipper

[![Build Status](https://travis-ci.org/Rudolph-Miller/clipper.svg)](https://travis-ci.org/Rudolph-Miller/clipper)

Clipper is a file attachment library.

## Usage

```Lisp
(in-package :cl-user)
(defpackage clipper.sample
  (:use :cl
        :integral
        :clipper))
(in-package :clipper.sample)

(connect-toplevel :mysql :database-name "clipper_sample" :username "root")

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

(setup-clipper :aws-access-key (asdf::getenv "AWS_ACCESS_KEY")
               :aws-secret-key (asdf::getenv "AWS_SECRET_KEY")
               :s3-endpoint "s3-ap-northeast-1.amazonaws.com"
               :s3-bucket-name "clipper-sample"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((picture (create-dao 'picture)))
  (save-dao (attach-image picture "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url picture))

=> "https://s3-ap-northeast-1.amazonaws.com/clipper-sample/1/lisplogo_alien_256.png"
```

## See Also

- [Integral](https://github.com/fukamachi/integral) - Object relational mapper for Common Lisp

## Author

* Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## Copyright

Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
