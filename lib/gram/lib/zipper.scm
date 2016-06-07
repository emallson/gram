(define-module (gram lib zipper)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (zipper? mkzip unzip set swap del
                    zipper zipper-node
                    insert go rotate
                    extract children
                    top find path replay transform z-> zmap zfilter
                    contains?))

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
(define-method (children atom) #f)

(define (leaf? z)
  (eq? #f (children z)))

(define (go-down z)
  (match z
    (($ zipper node _ _ _)
     (let ((kids (children node)))
       (cond
         ((eq? #f kids) #f)
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

(define (rotate-left z)
  (match z
    [($ zipper node (left rest ...) up right)
     (make-zipper left (cons node rest) up right)]
    [_ #f]))

(define (rotate-right z)
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

(define (rotate z dir)
  (case dir
    ((left) (rotate-left z))
    ((right) (rotate-right z))
    (else #f)))

(define (insert z new dir)
  (case dir
    ((left) (insert-left z new))
    ((right) (insert-right z new))
    (else #f)))
(define (set z new)
  (match z
    (($ zipper _ left up right)
     (make-zipper new left up right))
    (_ #f)))

(define (swap z f . args)
  (set z (apply f (zipper-node z) args)))

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

(define (find-dfs p? z)
  (if (or (not (zipper? z)) (p? (zipper-node z)))
      z
      (let ((down (find-dfs p? (go z 'down))))
        (if down
            down
            (find-dfs p? (go z 'right))))))

(define (find z p?)
  (find-dfs p? (top z)))

(define (path z)
  (match z
    (($ zipper node #f #f #f)
     '())
    (($ zipper _ '() _ _)
     (append (path (go z 'up)) (list 'down)))
    (($ zipper _ (a b ...) _ _)
     (append (path (go z 'left)) (list 'right)))
    (_ #f)))

(define (replay z path)
  (if (null? path)
      z
      (or (replay (go z (car path)) (cdr path)) z)))

(define (transform z p? f . rest)
  "Transforms the given zipper by finding the first element `dst`
satisfying predicate `p?', calling `(apply f dst rest)`, and then
returning to the original position.

If `p?' does not satisfy `procedure?' then it is instead compared with
`equal?'."
  (let ((track (path z))
        (dst (find z (if (procedure? p?)
                       p?
                       (lambda (x) (equal? p? x))))))
    (if dst
      (replay (top (apply f dst rest)) track)
      z)))

(define (find-dfs p? z)
  (if (or (not (zipper? z)) (p? (zipper-node z)))
      z
      (let ((down (find-dfs p? (go z 'down))))
        (if down
            down
            (find-dfs p? (go z 'right))))))

(define-syntax z->
  (syntax-rules ()
    [(z-> z (xform args ...))
     (or (xform z args ...) z)]
    [(z-> z (xform args ...) xforms ...)
     (let [(zp (xform z args ...))]
       (if zp
           (z-> zp xforms ...)
           z))]))

(define (-zmap z f)
  (if (zipper? z)
      (let* ((down (z-> z
                        (swap f)
                        (go 'down)
                        (-zmap f)
                        (go 'up))))
        (z-> down
             (go 'right)
             (-zmap f)))
      #f))

(define (zmap z f . rest)
  "Applies f x rest to each leaf node of zipper `z' in depth-first
order."
  (let ((track (path z))
        (result (-zmap (top z) (lambda (x) (if (leaf? x) (apply f x rest) x)))))
    (replay (top result) track)))

(define (-zfilter z p?)
  (if (zipper? z)
      (let ((zp (if (p? (zipper-node z)) z (del z))))
        (let* ((next* (z-> zp (go 'down) (-zfilter p?) (go 'up)))
               (next (if (null? (zipper-node next*))
                         (del next*)
                         next*)))
          (z-> next  (go 'right) (-zfilter p?))))
      #f))

(define (zfilter z p? . rest)
  "Returns the zipper containing the leaf elements of `z' which
satisfy (apply p? x rest)."
  (let ((track (path z))
        (result (-zfilter (top z) (lambda (x)
                                    (or (not (leaf? x))
                                        (apply p? x rest))))))
    (replay (top result) track)))

(define (contains? z x)
  (not (eq? #f (find z (lambda (y) (equal? x y))))))
