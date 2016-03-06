(use-modules (ggspec lib)
             (gram lib zipper))

(define-macro (suite name . tests)
  `((@ (ggspec lib) suite) ,name
          (tests ,@(map (lambda (test)
                          `(test ,(string-append name " " (car test)) e ,(cadr test)))
                        tests))
          (options
           (option 'output-cb output-tap))))

(suite "mkzip"
       ("should return #f for non-lists"
        (assert-all
         (assert-equal #f (mkzip #f))
         (assert-equal #f (mkzip "something"))
         (assert-equal #f (mkzip 3.14156))))
       ("should return a zipper with node #nil for the empty list"
        (assert-equal #nil (zipper-node (mkzip '()))))
       ("should return a zipper for other lists"
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
        (assert-equal 'a (zipper-node (go-left (go-right (mkzip '(a b c d))))))))

(suite "go-right"
       ("should return #f for non-zippers"
        (assert-false (go-right #f)))
       ("should return #f if there is nothing to the right"
        (assert-false (go-right (go-right (go-right (go-right (mkzip '(a b c d))))))))
       ("should return a zipper to the right otherwise"
        (assert-equal 'b (zipper-node (go-right (mkzip '(a b c d)))))))

(suite "go-up"
       ("should return #f for non-zippers"
        (assert-false (go-up #f)))
       ("should return #f if there is nothing upwards"
        (assert-false (go-up (mkzip '(a b c d)))))
       ("should return a zipper up otherwise"
        (assert-equal '(a) (zipper-node (go-up (go-down (mkzip '((a) b c))))))))

(suite "go-down"
       ("should return #f for non-zippers"
        (assert-false (go-down #f)))
       ("should return #f if there is nothing downwards"
        (assert-false (go-down (mkzip '(a b c d)))))
       ("should return a zipper down otherwise"
        (assert-equal 'a (zipper-node (go-down (mkzip '((a) b c)))))))

(suite "insert-left"
       ("should return #f for non-zippers"
        (assert-false (insert-left 'a #f)))
       ("should add an element to the left"
        (assert-equal '(a b) (unzip (insert-left 'a (mkzip '(b))))))
       ("should add an element to the left -- even when nested"
        (assert-equal '((b) (a c)) (unzip (insert-left 'a (go-down (go-right (mkzip '((b) (c))))))))))

(suite "insert-right"
       ("should return #f for non-zippers"
        (assert-false (insert-right 'a #f)))
       ("should effectively swap for the empty zipper"
        (assert-equal '(a) (unzip (insert-right 'a (mkzip '())))))
       ("should add an element to the right"
        (assert-equal '(b a) (unzip (insert-right 'a (mkzip '(b))))))
       ("should add an element to the right -- even when nested"
        (assert-equal '((b) (c a)) (unzip (insert-right 'a (go-down (go-right (mkzip '((b) (c))))))))))

(suite "swap"
       ("should return #f for non-zippers"
        (assert-false (swap 'a #f)))
       ("should replace the current node in the new zipper"
        (assert-equal '(b) (unzip (swap 'b (mkzip '(a))))))
       ("should replace the current node in the new zipper -- even when nested"
        (assert-equal '((b) (a)) (unzip (swap 'a (go-down (go-right (mkzip '((b) (c))))))))))

(suite "kill"
       ("should return #f for non-zippers"
        (assert-false (kill 'a)))
       ("should replace the current node with #nil if no right or left"
        (assert-equal #nil (zipper-node (kill (mkzip '(a))))))
       ("should replace the current node with (car right) if it exists"
        (assert-equal 'b (zipper-node (kill (mkzip '(a b))))))
       ("should replace the current node with (car left) if it exists but not (car right)"
        (assert-equal 'a (zipper-node (kill (go-right (mkzip '(a b)))))))
       ("should remove the element from the zipper entirely"
        (assert-equal '() (unzip (kill (mkzip '(a)))))))
