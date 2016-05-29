(define-module (gram lib motion)
  #:use-module (gram lib zipper)
  #:use-module ((gram lib render-hooks) #:select (transform-layout! current-view))
  #:use-module ((gram view) #:renamer (symbol-prefix-proc 'view-))
  #:export (move-cursor move-window))

(define (function s)
  (eval s (current-module)))

(define (move-cursor dir)
  "Move the focus cursor in the specified direction in the current
layer."
  (transform-layout! (lambda (z)
                       (z-> z (go dir))))
  (let ((v (current-view)))
    (when v
      (view-focus v))))

(define (move-window dir)
  "Move the window and cursor in the specified direction in the
current layer."
  (transform-layout! (lambda (z)
                       (let ((w (zipper-node z)))
                         (z-> z (swap dir) (go dir)))))
  (let ((v (current-view)))
    (when v
      (view-focus v))))
