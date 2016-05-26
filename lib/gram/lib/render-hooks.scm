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
  #:use-module (gram output hooks))

(define %default-layout (tall))
(define %default-floating-layout (simple))
(define %output-list '())
(define %current-output #nil)
(define %current-workspace #nil)

(define-record-type workspace
  (make-workspace tiling-layout floating-layout float?)
  workspace?
  (tiling-layout tiling-layout set-tiling-layout!)
  (floating-layout floating-layout set-floating-layout!)
  (float? float? set-float!))

(define (should-float? view)
  (let ((types (view-get-types view)))
    (not (null? types))))

(define (new-workspace)
  "Create a new, empty workspace with the default tiling and floating layouts."
  (make-workspace
   (go-down (mkzip %default-layout))
   (go-down (mkzip %default-floating-layout))
   should-float?))

(define (output-created out)
  (set! %output-list (acons out (new-workspace) %output-list)))

(define (output-focused out focused?)
  (when focused?
    (unless (null? %current-output)
      (assoc-set! %output-list out %current-zipper))
    (set! %current-output out)
    (set! %current-workspace (assoc-ref %output-list out))))

(define (zipper-in-layout? zipper)
  (let ((up (go-up zipper)))
    (if (zipper? up)
        (layout? (zipper-node up))
        #f)))

(define (add-view zipper view)
  (when (zipper-in-layout? zipper)
    (let* ((nz (insert-right zipper view))
           (rz (go-right nz)))
      (if rz rz nz))))

(define (view-created view)
  (if ((float? %current-workspace) view)
      (set-floating-layout!
       %current-workspace
       (add-view (floating-layout %current-workspace) view))
      (set-tiling-layout!
       %current-workspace
       (add-view (tiling-layout %current-workspace) view)))
  (view-focus view)
  (view-bring-to-front view)
  (render! %current-output (tiling-layout %current-workspace))
  (render! %current-output (floating-layout %current-workspace)))

(define (top-level? z)
  (match z
    (($ zipper _ #f #f #f) #t)
    (_ #f)))

(define (view-destroyed view)
  (set-tiling-layout! %current-workspace (transform (tiling-layout %current-workspace) view del))
  (set-floating-layout! %current-workspace (transform (floating-layout %current-workspace) view del))
  (render! %current-output (tiling-layout %current-workspace))
  (render! %current-output (floating-layout %current-workspace)))

(define (view-handle-geometry view geo)
  (let ((rv (filter (lambda (rv)
                      (eq? (rview-view rv) view))
                    (append
                     (place (unzip (tiling-layout %current-workspace))
                            %current-output
                            '(0 . 0) (output-get-resolution %current-output))
                     (place (unzip (floating-layout %current-workspace))
                            %current-output
                            '(0 . 0) (output-get-resolution %current-output))))))
    (unless (null? rv)
      (view-set-geometry view (cons (rview-origin (car rv))
                                    (rview-dimensions (car rv)))))))

(add-hook! output-created-hook output-created)
(add-hook! output-focus-hook output-focused)
(add-hook! view-created-hook view-created)
(add-hook! view-request-geometry-hook view-handle-geometry)
(add-hook! view-destroyed-hook view-destroyed)
