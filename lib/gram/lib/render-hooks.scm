(define-module (gram lib render-hooks)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (gram lib zipper)
  #:use-module (gram lib render)
  #:use-module ((gram lib layout) #:select (tall simple))
  #:use-module ((gram view) #:renamer (symbol-prefix-proc 'view-))
  #:use-module (gram view hooks)
  #:use-module ((gram output) #:renamer (symbol-prefix-proc 'output-))
  #:use-module (gram output hooks)
  #:export (transform-workspace! transform-layout! current-view))

(define %default-layout (tall))
(define %default-floating-layout (simple))
(define %output-list '())
(define %current-output #nil)
(define %current-workspace #nil)

(define-record-type workspace
  (make-workspace tiling-layout floating-layout float? focused)
  workspace?
  (focused focused-layout set-focused-layout!)
  (tiling-layout tiling-layout set-tiling-layout!)
  (floating-layout floating-layout set-floating-layout!)
  (float? float? set-float!))

(define (re-render!)
  (render! %current-output (tiling-layout %current-workspace))
  (render! %current-output (floating-layout %current-workspace)))

(define (transform-workspace! layer f)
  "Transform the `layer' layout of the current workspace by applying f
to the appropriate layout.

`layer' should be either 'floating or 'tiling.

`f' should be a function taking a layout zipper and returning another
layout zipper."
  (case layer
    ((floating) (set-floating-layout! %current-workspace
                                      (f (floating-layout %current-workspace))))
    ((tiling) (set-tiling-layout! %current-workspace
                                  (f (tiling-layout %current-workspace))))
    ((both) (begin
              (transform-workspace! 'tiling f)
              (transform-workspace! 'floating f))))
  (re-render!))

(define (transform-layout! f)
  "Transform the currently focused layout by `f'. See the definition
of `transform-workspace!' for more information."
  (transform-workspace! (focused-layout %current-workspace) f))

(define (current-view)
  (let ((vol (case (focused-layout %current-workspace)
               ((floating) (zipper-node (floating-layout %current-workspace)))
               ((tiling) (zipper-node (tiling-layout %current-workspace))))))
    (if (view-view? vol)
        vol
        #f)))

(define (should-float? view)
  (let ((types (view-get-types view)))
    (not (null? types))))

(define (new-workspace)
  "Create a new, empty workspace with the default tiling and floating layouts."
  (make-workspace
   (go (mkzip %default-layout) 'down)
   (go (mkzip %default-floating-layout) 'down)
   should-float?
   'tiling))

(define (output-created out)
  (set! %output-list (acons out (new-workspace) %output-list)))

(define (output-focused out focused?)
  (when focused?
    (unless (null? %current-output)
      (assoc-set! %output-list out %current-workspace))
    (set! %current-output out)
    (set! %current-workspace (assoc-ref %output-list out))))

(define (zipper-in-layout? zipper)
  (let ((up (go zipper 'up)))
    (if (zipper? up)
        (layout? (zipper-node up))
        #f)))

(define (add-view zipper view)
  (when (zipper-in-layout? zipper)
    (z-> zipper
         (insert view 'right)
         (go 'right))))

(define (view-created view)
  (transform-workspace! (if ((float? %current-workspace) view)
                            'floating 'tiling)
                        (cute add-view <> view))
  (view-focus view)
  (view-bring-to-front view)
  (render! %current-output (tiling-layout %current-workspace))
  (render! %current-output (floating-layout %current-workspace)))

(define (top-level? z)
  (match z
    (($ zipper _ #f #f #f) #t)
    (_ #f)))

(define (view-destroyed)
  (transform-workspace! 'both (lambda (z) (zfilter z view-active?)))
  (when (current-view)
      (view-focus (current-view)))
  (re-render!))

(define (view-handle-geometry view geo)
  (let* ((rv (filter (lambda (rv)
                      (eq? (rview-view rv) view))
                    (append
                     (place (unzip (tiling-layout %current-workspace))
                            %current-output
                            '(0 . 0) (output-get-resolution %current-output))
                     (place (unzip (floating-layout %current-workspace))
                            %current-output
                            '(0 . 0) (output-get-resolution %current-output)))))
        (new-geo (cons (rview-origin (car rv))
                       (rview-dimensions (car rv)))))
    (view-set-geometry view new-geo)))

(add-hook! output-created-hook output-created)
(add-hook! output-focus-hook output-focused)
(add-hook! view-created-hook view-created)
(add-hook! view-request-geometry-hook view-handle-geometry)
(add-hook! view-destroyed-hook view-destroyed)
