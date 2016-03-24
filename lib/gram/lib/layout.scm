(define-module (gram lib layout)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (gram lib render)
  #:export     (tall wide))

(define-layout tall (weights) (views geometry output)
  "Lay out windows in columns, with the weights option specifying
their relative sizes."
  (match geometry
    ((width . height)
     (let ((view-width (floor/ width (length views))))
       (map-in-order
        (lambda (v i)
          (make-renderable v output
                      (cons (* i view-width) 0)
                      (cons view-width height)))
        views (iota (length views)))))))

(define (cons-rev c)
  (cons (cdr c) (car c)))

(define-layout wide (weights) (views geometry output)
  "Lay out windows in rows, with relative sizes specified by the
weights option."
  (map-in-order
   (lambda (rv)
     (let ((irv (set-rview-origin rv (cons-rev (rview-origin rv)))))
       (set-rview-dimensions irv (cons-rev (rview-dimensions rv)))))
   ((apply tall (append (list #:weights weights) views)) (cons-rev geometry) output)))
