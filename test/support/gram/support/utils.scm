(define-module (gram support utils)
  #:export (define-dead-mock))

(define-syntax-rule (define-dead-mock name)
  (define* (name #:rest args)
    (error (format "~a called with arguments ~a" name args))))
