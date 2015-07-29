(in-package :cl-user)
(defpackage clipper.error
  (:use :cl))
(in-package :clipper.error)

(syntax:use-syntax :annot)

@export
(define-condition <clipper-error> (simple-error)
  ()
  (:documentation "Base error condition."))

@export
(define-condition <clipper-invalid-store-type> (<clipper-error>)
  ((type :initarg :type :documentation "Type slot."))
  (:report
   (lambda (condition stream)
     (format stream
             "Invalid store type: ~a."
             (slot-value condition 'type))))
  (:documentation "Invalid stare type error."))

@export
(define-condition <clipper-incomplete-config> (<clipper-error>)
  ((slot-list :initarg :slot-list :documentation "Invalid slot list slot."))
  (:report
   (lambda (condition stream)
     (format stream
             "Incomplete config: ~{~A ~}"
             (slot-value condition 'slot-list))))
  (:documentation "Incomplete config error."))

@export
(define-condition <clipper-incomplete-local-config> (<clipper-incomplete-config>)
  ()
  (:documentation "Incomplete local config error."))

@export
(define-condition <clipper-incomplete-s3-config> (<clipper-incomplete-config>)
  ()
  (:documentation "Incomplete S3 config error."))

@export
(define-condition <clipper-no-source-specified> (<clipper-error>)
  ((object :initarg :object :documentation "Object slot."))
  (:report
   (lambda (condition stream)
     (format stream
             "Please provide URL or vector data with ~a."
             (slot-value condition 'object))))
  (:documentation "No source specified error."))

@export
(define-condition <clipper-unsupported-content-type> (<clipper-error>)
  ((content-type :initarg :content-type :documentation "Content type slot."))
  (:report
   (lambda (condition stream)
     (format stream
             "Unsupported content-type : ~a."
             (slot-value condition 'content-type))))
  (:documentation "Unsupported content type error."))

@export
(define-condition <clipper-incomplete-for-attach-image> (<clipper-error>)
  ((type :initarg :type :documentation "Type slot.")
   (args :initarg :args :documentation "Args slot."))
  (:report
   (lambda (condition stream)
     (format stream
             "attach-image with ~a needs~{ ~a~}"
             (slot-value condition 'type)
             (slot-value condition 'args))))
  (:documentation "Incomplete for attach-image."))

@export
(define-condition <clipper-image-type-error> (<clipper-error>)
  ((type :initarg :type :documentation "Type slot."))
  (:report
   (lambda (condition stream)
     (format stream
             "Type of image: ~a does not satisfy '(simple-array (unsigned-byte 8))."
             (slot-value condition 'type))))
  (:documentation "Image type error."))
