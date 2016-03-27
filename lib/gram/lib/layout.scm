(define-module (gram lib layout)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (gram lib render)
  #:export     (tall wide))

(define-layout columns ((weights #nil))
  "Lay out windows in columns, with the weights option specifying
their relative sizes."
  (lambda (views opts output dims)
    (match dims
      ((width . height)
       (let ((view-width (floor/ width (length views))))
         (map
          (lambda (v i)
            (place v output
                   (cons (* i view-width) 0)
                   (cons view-width height)))
          views (iota (length views))))))))

(define (cons-rev c)
  (cons (cdr c) (car c)))

(define-layout rows ((weights #nil))
  "Lay out windows in rows, with relative sizes specified by the
weights option."
  (lambda (views opts output dims)
    (map (lambda (rv)
           (let ((irv (set-rview-origin rv (cons-rev (rview-origin rv)))))
             (set-rview-dimensions irv (cons-rev (rview-dimensions rv)))))
         (layout-with columns views opts output (cons-rev dims)))))
