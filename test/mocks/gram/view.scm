(define-module (gram view)
  #:use-module (gram support utils)
  #:export (view? set-output set-geometry))

(define (view? v)
  (if (equal? v 'test-view)
      #t
      (error (format "view? called with input ~a" v))))

(define-dead-mock set-output)
(define-dead-mock set-geometry)
