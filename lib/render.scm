(define-module (gram lib render)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module ((gram view)
                #:renamer (symbol-prefix-proc 'view-))
  #:use-module ((gram output)
                #:renamer (symbol-prefix-proc 'output-))
  #:export     (define-layout make-renderable output render
                make-rview
                rview-view set-rview-view
                rview-output set-rview-output
                rview-origin set-rview-origin
                rview-dimensions set-rview-dimensions))

(define-immutable-record-type rview
  (make-rview view output origin dimensions)
  rview?
  (view rview-view set-rview-view)
  (output rview-output set-rview-output)
  (origin rview-origin set-rview-origin)
  (dimensions rview-dimensions set-rview-dimensions))

(define (remove-keys ls)
  "Remove all keys and key-value pairs from list `ls'."
  (let ((key-indices (filter (lambda (v) (not (unspecified? v)))
                             (map-in-order (lambda (v i)
                                             (when (keyword? v)
                                               i))
                                           ls (iota (length ls))))))
    (filter
     (lambda (v) (not (unspecified? v)))
     (map-in-order (lambda (v i)
                            (unless (or (member i key-indices)
                                    (member (- i 1) key-indices))
                                v))
                          ls (iota (length ls))))))

(define (flatten-once ls)
  (apply append
         (map-in-order (lambda (v)
                         (if (list? v)
                             v
                             (list v)))
                       ls)))

(define-syntax-rule (define-layout name (opts ...) (views geo out) docstring body ...)
  "Defines a layout function that takes options `opts' as keywords and
rest argument `views' and produces a lambda accepting arguments named
`geo' and `out'. The body of this lambda is `body'."
  (define* (name #:key opts ... #:rest _views)
    docstring
    (let ((views (remove-keys _views)))
      (lambda (geo out)
        (flatten-once (begin body ...))))))

(define (cons-add a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(define (shift-origins vols origin)
  (map (lambda (vol)
         (if (rview? vol)
             (set-rview-origin vol (cons-add origin (rview-origin vol)))
             (shift-origins vol origin)))
       vols))

(define (make-renderable view-or-layout output origin dimensions)
  (if (procedure? view-or-layout)
      (shift-origins (view-or-layout dimensions output) origin)
      (make-rview view-or-layout output origin dimensions)))

(define (render-rview rv)
  (match rv
    (($ rview view output origin dimensions)
     (when (view-view? view)
       (view-set-output view output)
       (view-set-geometry view (cons origin dimensions))))
    (_ #f)))

(define* (output out #:rest trees)
  "Place each layout in `trees' on output `out'."
  (map (lambda (t)
         (t (output-get-resolution out) out)) trees))

(define (render tree)
  "Render a layout tree. `tree' should be a syntax tree that can be evaluated
in the scope of the (gram lib render) module."
  (letrec ((mod (resolve-module '(gram lib render)))
           (rviews (eval tree mod)))
    (map render-rview rviews)))
