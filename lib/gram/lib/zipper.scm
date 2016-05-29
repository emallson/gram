(define-module (gram lib zipper)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (zipper? mkzip unzip set swap del
                    zipper zipper-node
                    insert go swap
                    extract children
                    top find path replay transform z->))

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
       (cond
         ((eq? #nil kids) #f)
         ((null? kids) (make-zipper #nil '() z '()))
         (#t (make-zipper (car kids) '() z (cdr kids))))))
    (_ #f)))

(define (insert-right z new)
  (match z
    (($ zipper #nil '() _ '())
     (set z new))
    (($ zipper node left up right)
     (make-zipper node left up (cons new right)))
    (_ #f)))

(define (insert-left z new)
  (match z
    (($ zipper #nil '() _ '())
     (set z new))
    (($ zipper node left up right)
     (make-zipper node (cons new left) up right))
    (_ #f)))

(define (set z new)
  (match z
    (($ zipper _ left up right)
     (make-zipper new left up right))
    (_ #f)))

(define (swap z f . args)
  (set (apply f (cons (zipper-node z) args)) z))

(define (del z)
  (match z
    (($ zipper node left up (next rest ...))
     (make-zipper next left up rest))
    (($ zipper node (next rest ...) up '())
     (make-zipper next rest up '()))
    (($ zipper node '() up '())
     (make-zipper #nil '() up '()))
    (($ zipper _ #f #f #f)
     (make-zipper #nil #f #f #f))
    (_ #f)))

(define (unzip z)
  (match z
    (($ zipper node #f #f #f)
     node)
    (($ zipper _ _ _ _)
     (unzip (go-up z)))
    (_ #f)))

(define (mkzip l)
  (make-zipper l #f #f #f))

(define (top z)
  (mkzip (unzip z)))

(define (find-dfs x z)
  (if (or (not (zipper? z)) (equal? (zipper-node z) x))
      z
      (let ((down (find-dfs x (go-down z))))
        (if down
            down
            (find-dfs x (go-right z))))))

(define (find x z)
  (find-dfs x (top z)))

(define (path z)
  (match z
    (($ zipper node #f #f #f)
     '())
    (($ zipper _ '() _ _)
     (append (path (go z 'up)) (list 'down)))
    (($ zipper _ (a b ...) _ _)
     (append (path (go z 'left)) (list 'right)))
    (_ #f)))

(define (replay path z)
  (if (null? path)
      z
      (or (replay (cdr path) (go z (car path))) z)))

(define (transform z x f . rest)
  "Transforms the given zipper by finding element `dst` with `x' in
it, calling `(apply f dst rest)`, and then returning to the original
position."
  (let ((track (path z))
        (dst (find x z)))
    (if dst
      (replay track (top (apply f dst rest)))
      z)))

(define-syntax z->
  (syntax-rules ()
    [(z-> z (xform args ...))
     (or (xform z args ...) z)]
    [(z-> z (xform args ...) xforms ...)
     (let [(zp (xform z args ...))]
       (if zp
           (z-> zp xforms ...)
           z))]))

(define (swap-left z)
  (match z
    [($ zipper node (left rest ...) up right)
     (make-zipper left (cons node rest) up right)]
    [_ #f]))

(define (swap-right z)
  (match z
    [($ zipper node left up (right rest ...))
     (make-zipper right left up (cons node rest))]
    [_ #f]))

(define (go z dir)
  (case dir
    ((left) (go-left z))
    ((right) (go-right z))
    ((up) (go-up z))
    ((down) (go-down z))
    (else #f)))

(define (swap z dir)
  (case dir
    ((left) (swap-left z))
    ((right) (swap-right z))
    (else #f)))

(define (insert z new dir)
  (case dir
    ((left) (insert-left z new))
    ((right) (insert-right z new))
    (else #f)))
