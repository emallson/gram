(define-module (gram lib zipper)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (zipper? mkzip unzip swap kill
                    zipper-node
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
    (($ zipper #nil '() ($ zipper _ left up right) '())
     (make-zipper '() left up right))
    (($ zipper node left ($ zipper _ uleft uup uright) right)
     (make-zipper (append (reverse left) (list node) right) uleft uup uright))
    (_ #f)))

(define (go-down z)
  (match z
    (($ zipper (next rest ...) _ _ _)
     (make-zipper next '() z rest))
    (_ #f)))

(define (insert-right new z)
  (match z
    (($ zipper #nil '() #f '())
     (swap new z))
    (($ zipper node left up right)
     (make-zipper node left up (cons new right)))
    (_ #f)))

(define (insert-left new z)
  (match z
    (($ zipper #nil '() #f '())
     (swap new z))
    (($ zipper node left up right)
     (make-zipper node (cons new left) up right))
    (_ #f)))

(define (swap new z)
  (match z
    (($ zipper _ left up right)
     (make-zipper new left up right))
    (_ #f)))

(define (kill z)
  (match z
    (($ zipper node left up (next rest ...))
     (make-zipper next left up rest))
    (($ zipper node (next rest ...) up '())
     (make-zipper next rest up '()))
    (($ zipper node '() up '())
     (make-zipper #nil '() up '()))
    (_ #f)))

(define (unzip z)
  (match z
    (($ zipper #nil left #f right)
     (append (reverse left) right))
    (($ zipper node left #f right)
     (append (reverse left) (list node) right))
    (($ zipper _ _ _ _)
     (unzip (go-up z)))
    (_ #f)))

(define (mkzip l)
  (match l
    ((first rest ...)
     (make-zipper first '() #f rest))
    (()
     (make-zipper #nil '() #f '()))
    (_ #f)))
