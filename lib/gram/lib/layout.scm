(define-module (gram lib layout)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gram lib render)
  #:export     (columns rows tall))

(define-layout columns ((weights #nil))
  "Lay out windows in columns, with the weights option specifying
their relative sizes."
  (lambda (views opts output dims)
    (match dims
      ((width . height)
       (let ((view-width (floor/ width (length views))))
         (map
          (lambda (v i)
            (if (= i (- (length views) 1))
                (place v output
                       (cons (* i view-width) 0)
                       (cons (- width (* i view-width)) height))
                (place v output
                       (cons (* i view-width) 0)
                       (cons view-width height))))
          views (iota (length views))))))))

(define (cons-rev c)
  (cons (cdr c) (car c)))

(define-layout rows ((weights #nil))
  "Lay out windows in rows, with relative sizes specified by the
weights option."
  (lambda (views opts output dims)
    (map (lambda (rv)
           (let ((irv (rview-set-origin rv (cons-rev (rview-origin rv)))))
             (rview-set-dimensions irv (cons-rev (rview-dimensions rv)))))
         (layout-with columns views opts output (cons-rev dims)))))

(define-layout tall ((weights #nil) (main #nil))
  "Lay out one window (the master)"
  (lambda (views opts output dims)
    (let* ((main (or (assoc-ref opts 'main) (car views)))
           (rest (delq main views)))
      (layout-with columns (if (null? rest)
                               (list main)
                               (list main (apply rows #:weights (assoc-ref opts 'weights) rest)))
                   '() output dims))))
