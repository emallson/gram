(use-modules ((ggspec lib)
              #:select (assert-all assert-false assert-equal assert-true))
             (srfi srfi-9 gnu)
             (oop goops)
             (gram lib zipper)
             (gram support test-setup))

(suite "mkzip"
       ("should return a zipper"
        (assert-true (zipper? (mkzip '(a b c d))))))

(suite "unzip"
       ("should return #f for non-zippers"
        (assert-all
         (assert-false (unzip '(a b c)))
         (assert-false (unzip 2.7))))
       ("should return the tree stored in the zipper"
        (assert-equal '(a b c d) (unzip (mkzip '(a b c d))))))

(suite "go-left"
       ("should return #f for non-zippers"
        (assert-false (go-left #f)))
       ("should return #f if there is nothing to the left"
        (assert-false (go-left (mkzip '(a b c d)))))
       ("should return a zipper to the left otherwise"
        (assert-equal 'a (zipper-node (go-left (go-right (go-down (mkzip '(a b c d)))))))))

(suite "go-right"
       ("should return #f for non-zippers"
        (assert-false (go-right #f)))
       ("should return #f if there is nothing to the right"
        (assert-false (go-right (go-right (go-right (go-right (go-down (mkzip '(a b c d)))))))))
       ("should return a zipper to the right otherwise"
        (assert-equal 'b (zipper-node (go-right (go-down (mkzip '(a b c d))))))))

(define-immutable-record-type zippable-test
  (make-zt children other)
  zt?
  (children zt-children set-zt-children)
  (other zt-other))

(define-method (children (zt <zippable-test>))
  (zt-children zt))

(define-method (extract kids (old-zt <zippable-test>))
  (set-zt-children old-zt kids))

(suite "go-up"
       ("should return #f for non-zippers"
        (assert-false (go-up #f)))
       ("should return #f if there is nothing upwards"
        (assert-false (go-up (mkzip '(a b c d)))))
       ("should return a zipper up otherwise"
        (assert-equal '(a) (zipper-node (go-up (go-down (go-down (mkzip '((a) b c))))))))
       ("should use `extract' to go up from inside a record"
        (assert-equal (make-zt '(a b) 'c)
                      (zipper-node (go-up (insert-right (go-down (mkzip (make-zt '(a) 'c))) 'b))))))

(suite "go-down"
       ("should return #f for non-zippers"
        (assert-false (go-down #f)))
       ("should return #f if there is nothing downwards"
        (assert-false (go-down (go-down (mkzip '(a b c d))))))
       ("should return a zipper down otherwise"
        (assert-equal 'a (zipper-node (go-down (go-down (mkzip '((a) b c)))))))
       ("should use `children' to go down into a record"
        (assert-equal 'a
                      (zipper-node (go-down (mkzip (make-zt '(a) 'c)))))))

(suite "insert-left"
       ("should return #f for non-zippers"
        (assert-false (insert-left #f 'a)))
       ("should add an element to the left"
        (assert-equal '(a b) (unzip (insert-left (go-down (mkzip '(b))) 'a))))
       ("should add an element to the left -- even when nested"
        (assert-equal '((b) (a c)) (unzip (insert-left (go-down (go-right (go-down (mkzip '((b) (c)))))) 'a)))))

(suite "insert-right"
       ("should return #f for non-zippers"
        (assert-false (insert-right #f 'a)))
       ("should effectively set for the empty zipper"
        (assert-equal '(a) (unzip (insert-right (go-down (mkzip '())) 'a))))
       ("should add an element to the right"
        (assert-equal '(b a) (unzip (insert-right (go-down (mkzip '(b))) 'a))))
       ("should add an element to the right -- even when nested"
        (assert-equal '((b) (c a)) (unzip (insert-right (go-down (go-right (go-down (mkzip '((b) (c)))))) 'a)))))

(suite "set"
       ("should return #f for non-zippers"
        (assert-false (set #f 'a)))
       ("should replace the current node in the new zipper"
        (assert-equal '(b) (unzip (set (go-down (mkzip '(a))) 'b))))
       ("should replace the current node in the new zipper -- even when nested"
        (assert-equal '((b) (a)) (unzip (set (go-down (go-right (go-down (mkzip '((b) (c)))))) 'a)))))

(suite "del"
       ("should return #f for non-zippers"
        (assert-false (del 'a)))
       ("should replace the current node with #nil if no right or left"
        (assert-equal #nil (zipper-node (del (go-down (mkzip '(a)))))))
       ("should replace the current node with (car right) if it exists"
        (assert-equal 'b (zipper-node (del (go-down (mkzip '(a b)))))))
       ("should replace the current node with (car left) if it exists but not (car right)"
        (assert-equal 'a (zipper-node (del (go-right (go-down (mkzip '(a b))))))))
       ("should remove the element from the zipper entirely"
        (assert-equal '() (unzip (del (go-down (mkzip '(a))))))))
