(define-module (gram lib motion)
  #:use-module (gram lib zipper)
  #:use-module ((gram lib render-hooks) #:select (transform-layout! current-view))
  #:use-module ((gram view) #:renamer (symbol-prefix-proc 'view-))
  #:export (move-cursor))

(define (move-cursor dir)
  (let ((f (eval (symbol-append 'go- dir) (current-module))))
    (transform-layout! (lambda (z)
                         (z-> z (f))))
    (let ((v (current-view)))
      (when v
        (view-focus v)))))
