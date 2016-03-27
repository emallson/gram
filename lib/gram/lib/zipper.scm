(define-module (gram lib zipper)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (zipper? mkzip unzip set swap kill
                    zipper-node
                    insert-left insert-right
                    go-left go-right go-up go-down
                    extract children))

(define-immutable-record-type zipper
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

(define-generic extract)
(define-method (extract (kids <list>) (old-parent <list>))
  kids)

(define (go-up z)
  (match z
    (($ zipper #nil '() ($ zipper old left up right) '())
     (make-zipper (extract '() old)
                  left up right))
    (($ zipper node left ($ zipper old uleft uup uright) right)
     (make-zipper (extract (append (reverse left) (list node) right) old)
                  uleft uup uright))
    (_ #f)))

(define-generic children)
(define-method (children (lst <list>)) lst)
(define-method (children atom) #nil)

(define (go-down z)
  (match z
    (($ zipper node _ _ _)
     (let ((kids (children node)))
       (if (eq? kids #nil)
           #f
           (make-zipper (car kids) '() z (cdr kids)))))
    (_ #f)))

(define (insert-right new z)
  (match z
    (($ zipper #nil '() #f '())
     (set new z))
    (($ zipper node left up right)
     (make-zipper node left up (cons new right)))
    (_ #f)))

(define (insert-left new z)
  (match z
    (($ zipper #nil '() #f '())
     (set new z))
    (($ zipper node left up right)
     (make-zipper node (cons new left) up right))
    (_ #f)))

(define (set new z)
  (match z
    (($ zipper _ left up right)
     (make-zipper new left up right))
    (_ #f)))

(define (swap z f . args)
  (set (apply f (cons (zipper-node z) args)) z))

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
