(define-module (gram output)
  #:use-module (srfi srfi-9 gnu)
  #:export (get-resolution))

(define-immutable-record-type output
  (make-output)
  _output?)
