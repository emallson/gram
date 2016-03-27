(define-module (gram view)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gram support utils)
  #:export (view? set-output set-geometry))

(define-immutable-record-type view
  (make-view)
  _view?)

(define (view? v)
  (if (equal? v 'test-view)
      #t
      (error (format "view? called with input ~a" v))))

(define-dead-mock set-output)
(define-dead-mock set-geometry)
