(define-module (gram lib motion)
  #:use-module (gram lib zipper)
  #:use-module ((gram lib render-hooks) #:select (transform-layout! current-view %current-workspace set-focused-layout! containing-layer focus-layer))
  #:use-module ((gram view) #:renamer (symbol-prefix-proc 'view-))
  #:export (move-cursor move-window window-focus))

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
                       (z-> z (rotate dir) (go dir)))))

(define (move-cursor-to-layout layout)
  "Move the focus cursor to the specified layout."
  (if (member layout '(tiling floating))
      (begin
        (set-focused-layout! %current-workspace layout)
        (let ((v (current-view)))
          (when v
            (view-focus v))))
      (error "~a is not a valid layout (try 'tiling or 'floating)" layout)))

(define (move-window-to-layout layout)
  "Move the current window to the specified layout and focus it."
  (if (member layout '(tiling floating))
      (begin
        (let ((v (current-view)))
          (transform-layout! (lambda (z)
                               (z-> z (del))))
          (set-focused-layout! %current-workspace layout)
          (transform-layout! (lambda (z)
                               (z-> z (insert v 'right))))
          (move-cursor 'right)))))

(define (window-focus view)
  "Change the current focus to `view'. This function should be
preferred to `view-focus' because it maintains the cursor state
correctly."
  (let ([layer (containing-layer view)])
    (focus-layer layer)
    (transform-layout! (lambda (z)
                         (find z (lambda (x)
                                   (equal? x view)))))
    (view-focus (current-view))))
