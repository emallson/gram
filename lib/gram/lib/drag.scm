(define-module (gram lib drag)
  #:export (drag-move-view drag-resize-view drag-stop-acting drag-setup)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (gram pointer)
  #:use-module (gram pointer hooks)
  #:use-module (gram keysym hooks)
  #:use-module (gram keysym)
  #:use-module (gram lib keymap)
  #:use-module (gram lib render-hooks)
  #:use-module (gram lib zipper)
  #:use-module ((gram view) #:renamer (symbol-prefix-proc 'view-)))

;;; starting position for a drag. Can't drag multiple at once, but I
;;; can't imagine how you would manage that.
(define %origin #f)
;;; original geometry
(define %origin-geometry #f)
;;; action taking place, either do-move or do-resize
(define %action #f)
;;; view being acted on
(define %action-target #f)

(define (acting?)
  "Returns #t if the user is currently acting on (moving or resizing)
a view."
  (and %origin %origin-geometry %action %action-target))

(define (drag-stop-acting)
  "Stops the interaction. Only one interaction can occur at a time."
  (set! %origin #f)
  (set! %origin-geometry #f)
  (set! %action #f)
  (set! %action-target #f))

(define (distance-moved origin position)
  (values
   (- (car position) (car origin))
   (- (cdr position) (cdr origin))))

(define (on-move _ position)
  (when (acting?)
    (%action position)))

(define (do-move! position)
  (when (acting?)
    (let-values ([(dx dy) (distance-moved %origin position)])
      (match %origin-geometry
        [((x . y) . dims)
         (view-set-geometry %action-target (cons
                                            (cons (+ x dx) (+ y dy))
                                            dims))]
        [_ (error "Invalid origin geometry ~a" %origin-geometry)]))))

(define (do-resize! position)
  (when (acting?)
    (let-values ([(dx dy) (distance-moved %origin position)])
      (match %origin-geometry
        [(pos w . h)
         (view-set-geometry %action-target (cons
                                            pos
                                            (cons (+ w dx) (+ h dy))))]
        [_ (error "Invalid origin geometry ~a" %origin-geometry)]))))

(define-syntax-rule (define-interaction name act docstring)
  (define (name view)
    docstring
    (unless (acting?)
      (let ((eq (lambda (x) (equal? view x))))
        (transform-workspace! 'tiling (lambda (z)
                                        (transform z eq del)))
        (transform-workspace! 'floating (lambda (z)
                                          (if (find z eq)
                                              z
                                              (or (add-view z view) z)))))
      (view-bring-to-front view)
      (re-render!)
      (set! %origin (pointer-position))
      (set! %origin-geometry (view-get-geometry view))
      (set! %action act)
      (set! %action-target view))))

(define-interaction drag-move-view do-move!
  "Begin moving the given view with the given position as the origin.")
(define-interaction drag-resize-view do-resize!
  "Begin resizing the given view with the given position as the origin.")

(define (drag-setup km interaction key)
  "Set up the given interaction to begin when `key' is pressed and end
when the unmodified `key' is released."
  (define-key! km key interaction)
  (add-hook! keyup-hook (lambda (released view)
                          (let ((rel (unmodified released)))
                            (when (equal? rel (unmodified key))
                              (drag-stop-acting)))))
  (unless (member on-move (hook->list pointer-motion-hook))
    (add-hook! pointer-motion-hook on-move)))
