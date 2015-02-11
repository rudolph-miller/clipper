(in-package :cl-user)
(defpackage clipper.error
  (:use :cl))
(in-package :clipper.error)

(syntax:use-syntax :annot)

@export
(define-condition <clipper-error> (simple-error) ())

@export
(define-condition <clipper-invalid-store-type> (<clipper-error>)
  ((type :initarg :type))
  (:report
   (lambda (condition stream)
     (format stream
             "Invalid store type: ~a."
             (slot-value condition 'type)))))

@export
(define-condition <clipper-incomplete-config> (<clipper-error>)
  ((slot-list :initarg :slot-list))
  (:report
   (lambda (condition stream)
     (format stream
             "Incomplete config: ~{~A ~}" (slot-value condition 'slot-list)))))

@export
(define-condition <clipper-incomplete-local-config> (<clipper-incomplete-config>) ())

@export
(define-condition <clipper-incomplete-s3-config> (<clipper-incomplete-config>) ())
