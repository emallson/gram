(define-module (gram lib zipper)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (zipper? zip extract swap
                    insert-left insert-right
                    go-left go-right go-up go-down))

(define-record-type zipper
  (make-zipper node left up right)
  zipper?
  (node zipper-node)
  (left zipper-left)
  (up zipper-up)
  (right zipper-right))

(define (go-left z)
  (match z
    (($ zipper node (next rest ...) up right)
     (make-zipper next rest up (cons node right)))
    (_ #f)))

(define (go-right z)
  (match z
    (($ zipper node left up (next rest ...))
     (make-zipper next (cons node left) up rest))
    (_ #f)))

(define (go-up z)
  (match z
    (($ zipper node left ($ zipper unode uleft uup uright) right)
     (make-zipper (append (reverse left) (list node) right) uleft uup uright))
    (_ #f)))

(define (go-down z)
  (match z
    (($ zipper (next rest ...) _ _ _)
     (make-zipper next '() z rest))
    (_ #f)))

(define (insert-right z new)
  (match z
    (($ zipper node left up right)
     (make-zipper node left up (cons new right)))
    (_ #f)))

(define (insert-left z new)
  (match z
    (($ zipper node left up right)
     (make-zipper node (cons new left) up right))
    (_ #f)))

(define (swap z new)
  (match z
    (($ zipper _ left up right)
     (make-zipper new left up right))
    (_ #f)))

(define (extract z)
  (match z
    (($ zipper node left #f right)
     (append (reverse left) (list node) right))
    (($ zipper _ _ _ _)
     (extract (go-up z)))
    (_ #f)))

(define (zip l)
  (match l
    ((first rest ...)
     (make-zipper first '() #f rest))
    (_ #f)))
