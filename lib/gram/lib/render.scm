(define-module (gram lib render)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module ((gram lib zipper)
                #:select (children extract))
  #:use-module ((gram view)
                #:renamer (symbol-prefix-proc 'view-))
  #:use-module ((gram output)
                #:renamer (symbol-prefix-proc 'output-))
  #:export     (define-layout place output render!
                layout-with
                rview?
                rview-view rview-set-view
                rview-origin rview-set-origin
                rview-output rview-set-output
                rview-dimensions rview-set-dimensions))

(define-generic place)
(define-method (place (view <view>) (output <output>) (origin <pair>) (dims <pair>))
  (make-rview view output origin dims))


(define-immutable-record-type rview
  (make-rview view output origin dimensions)
  rview?
  (view rview-view rview-set-view)
  (output rview-output rview-set-output)
  (origin rview-origin rview-set-origin)
  (dimensions rview-dimensions rview-set-dimensions))

(define-immutable-record-type layout
  (make-layout type render views opts)
  layout?
  (type layout-type layout-set-type)
  (render layout-render-fn layout-set-render-fn)
  (views layout-views layout-set-views)
  (opts layout-opts layout-set-opts))

(set-record-type-printer!
 layout
 (lambda (rec port)
   (match rec
     (($ layout type _ views opts)
      (format port "(~s~{ ~s~}~{ ~s~})" type (alist->kvs opts) views))
     (_ (error "Unable to display record" rec)))))

(define-method (children (lout <layout>))
  (layout-views lout))

(define-method (extract (views <list>) (old-layout <layout>))
  (layout-set-views old-layout views))

(define (remove-keys ls)
  "Remove all keys and key-value pairs from list `ls'."
  (let ((key-indices (filter (lambda (v) (not (unspecified? v)))
                             (map-in-order (lambda (v i) (when (keyword? v) i))
                                           ls (iota (length ls))))))
    (filter
     (lambda (v) (not (unspecified? v)))
     (map-in-order (lambda (v i)
                            (unless (or (member i key-indices)
                                    (member (- i 1) key-indices)) v))
                          ls (iota (length ls))))))

(define (only-keys ls)
  "Remove all keys and key-value pairs from list `ls'."
  (let ((key-indices (filter (lambda (v) (not (unspecified? v)))
                             (map-in-order (lambda (v i)
                                             (when (keyword? v) i))
                                           ls (iota (length ls))))))
    (filter
     (lambda (v) (not (unspecified? v)))
     (map-in-order (lambda (v i)
                            (when (or (member i key-indices)
                                    (member (- i 1) key-indices))
                                v))
                          ls (iota (length ls))))))

(define (kvs->alist kvs)
  (if (null? kvs)
      kvs
      (acons (keyword->symbol (car kvs))
             (cadr kvs)
             (kvs->alist (cddr kvs)))))

(define (alist->kvs alist)
  (if (null? alist)
      alist
      (flatten-once (map (lambda (pair)
                           (list (symbol->keyword (car pair))
                                 (cdr pair)))
                         alist))))

(define-syntax define-layout
  (syntax-rules ()
    ((_ name (opts ...) render)
     (define-layout name (opts ...) "" render))
    ((_ name (opts ...) docstring render)
     (begin
       (define* (name #:key opts ... #:rest views)
         docstring
         (make-layout (procedure-name name)
                      render
                      (remove-keys views)
                      (kvs->alist (only-keys views))))))))

(define (flatten-once ls)
  (apply append
         (map-in-order
          (lambda (v)
            (if (list? v) v (list v)))
          ls)))

(define (cons-add a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(define (shift-origins origin vols)
  (map (lambda (vol)
         (rview-set-origin vol (cons-add origin (rview-origin vol))))
       vols))

(define-method (place (layout <layout>) (output <output>) (origin <pair>) (dims <pair>))
  (let ((render (layout-render-fn layout)))
    (shift-origins origin (flatten-once (render (layout-views layout) (layout-opts layout) output dims)))))

(define (layout-with layout-fn views opts output dims)
  "Layout the given `views' with `layout-fn'.
Use this function to compose layouts. A practical example of this is
given in the `rows' layout, which is defined in terms of the `columns'
layout."
  (place (apply layout-fn (append (alist->kvs opts) views))
         output '(0 . 0) dims))

(define (render-rview! rv)
  "Renders an individual rview onto the screen by setting the output
and geometry of the view smob."
  (match rv
    (($ rview view output origin dimensions)
     (when (view-view? view)
       (view-set-output view output)
       (view-set-geometry view (cons origin dimensions))))
    (_ #f)))

(define* (output out #:rest layouts)
  "Place each layout in `trees' on output `out'."
  (map (lambda (t)
         (place t out '(0 . 0) (output-get-resolution out))) layouts))

(define (render! tree)
  "Render a layout tree. `tree' should be a syntax tree that can be evaluated
in the scope of the (gram lib render) module."
  (letrec ((mod (resolve-module '(gram lib render)))
           (rviews (eval tree mod)))
    (map render-rview! rviews)))
