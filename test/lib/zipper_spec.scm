(use-modules (srfi srfi-9 gnu)
             (oop goops)
             (gram lib zipper)
             (gram support test-setup))

(describe "(gram lib zipper)"
  (describe "mkzip"
    (it "should return a zipper"
      (zipper? (mkzip '(a b c d)))))

  (describe "unzip"
    (it "should return #f for non-zippers"
      (and
       (not (unzip '(a b c)))
       (not (unzip 2.7))))
    (it "should return the tree stored in the zipper"
      (equal? '(a b c d) (unzip (mkzip '(a b c d))))))

  (describe "go-left"
    (it "should return #f for non-zippers"
      (not (go-left #f)))
    (it "should return #f if there is nothing to the left"
      (not (go-left (mkzip '(a b c d)))))
    (it "should return a zipper to the left otherwise"
      (equal? 'a (zipper-node (go-left (go-right (go-down (mkzip '(a b c d)))))))))

  (describe "go-right"
    (it "should return #f for non-zippers"
      (not (go-right #f)))
    (it "should return #f if there is nothing to the right"
      (not (go-right (go-right (go-right (go-right (go-down (mkzip '(a b c d)))))))))
    (it "should return a zipper to the right otherwise"
      (equal? 'b (zipper-node (go-right (go-down (mkzip '(a b c d))))))))

  (define-immutable-record-type zippable-test
    (make-zt children other)
    zt?
    (children zt-children set-zt-children)
    (other zt-other))

  (define-method (children (zt <zippable-test>))
    (zt-children zt))

  (define-method (extract kids (old-zt <zippable-test>))
    (set-zt-children old-zt kids))

  (describe "go-up"
    (it "should return #f for non-zippers"
      (not (go-up #f)))
    (it "should return #f if there is nothing upwards"
      (not (go-up (mkzip '(a b c d)))))
    (it "should return a zipper up otherwise"
      (equal? '(a) (zipper-node (go-up (go-down (go-down (mkzip '((a) b c))))))))
    (it "should use `extract' to go up from inside a record"
      (equal? (make-zt '(a b) 'c)
              (zipper-node (go-up (insert-right (go-down (mkzip (make-zt '(a) 'c))) 'b))))))

  (describe "go-down"
    (it "should return #f for non-zippers"
      (not (go-down #f)))
    (it "should return #f if there is nothing downwards"
      (not (go-down (go-down (mkzip '(a b c d))))))
    (it "should return a zipper down otherwise"
      (equal? 'a (zipper-node (go-down (go-down (mkzip '((a) b c)))))))
    (it "should use `children' to go down into a record"
      (equal? 'a
              (zipper-node (go-down (mkzip (make-zt '(a) 'c)))))))

  (describe "insert-left"
    (it "should return #f for non-zippers"
      (not (insert-left #f 'a)))
    (it "should add an element to the left"
      (equal? '(a b) (unzip (insert-left (go-down (mkzip '(b))) 'a))))
    (it "should leave the same node focused"
        (let ((z (go-down (mkzip '(b)))))
          (equal? (zipper-node z) (zipper-node (insert-left z 'a)))))
    (it "should add an element to the left -- even when nested"
      (equal? '((b) (a c)) (unzip (insert-left (go-down (go-right (go-down (mkzip '((b) (c)))))) 'a)))))

  (describe "insert-right"
    (it "should return #f for non-zippers"
      (not (insert-right #f 'a)))
    (it "should effectively set for the empty zipper"
      (equal? '(a) (unzip (insert-right (go-down (mkzip '())) 'a))))
    (it "should add an element to the right"
      (equal? '(b a) (unzip (insert-right (go-down (mkzip '(b))) 'a))))
    (it "should leave the same node focused"
        (let ((z (go-down (mkzip '(b)))))
          (equal? (zipper-node z) (zipper-node (insert-right z 'a)))))
    (it "should add an element to the right -- even when nested"
      (equal? '((b) (c a)) (unzip (insert-right (go-down (go-right (go-down (mkzip '((b) (c)))))) 'a)))))

  (describe "set"
    (it "should return #f for non-zippers"
      (not (set #f 'a)))
    (it "should replace the current node in the new zipper"
      (equal? '(b) (unzip (set (go-down (mkzip '(a))) 'b))))
    (it "should replace the current node in the new zipper -- even when nested"
      (equal? '((b) (a)) (unzip (set (go-down (go-right (go-down (mkzip '((b) (c)))))) 'a)))))

  (describe "del"
    (it "should return #f for non-zippers"
      (not (del 'a)))
    (it "should replace the current node with #nil if no right or left"
      (equal? #nil (zipper-node (del (go-down (mkzip '(a)))))))
    (it "should replace the current node with (car right) if it exists"
      (equal? 'b (zipper-node (del (go-down (mkzip '(a b)))))))
    (it "should replace the current node with (car left) if it exists but not (car right)"
      (equal? 'a (zipper-node (del (go-right (go-down (mkzip '(a b))))))))
    (it "should remove the element from the zipper entirely"
      (equal? '() (unzip (del (go-down (mkzip '(a))))))))

  (describe "path"
    (it "should return #f for non-zippers"
        (not (path 'a)))
    (it "should return the empty list if at the top of the zipper"
        (null? (path (mkzip '(a)))))
    (it "should return the sequence to reach the current zipper position otherwise"
        (equal? (list go-down go-right go-down)
                (path (go-down (go-right (go-down (mkzip '(a (b c))))))))))

  (describe "replay"
    (it "should return z itself for an empty path"
        (let ((z (mkzip '(a))))
          (eq? (replay '() z) z)))
    (it "should return z after the steps in the path have been applied"
        (let ((z (mkzip '(a (b c)))))
          (equal? (replay (list go-down go-right go-down) z)
                  (go-down (go-right (go-down z)))))))

  (describe "transform"
    (it "should return z itself if z is not a zipper"
        (eq? 'a (transform 'a 'b identity)))
    (it "should return z itself if the element is not in z"
        (let ((z (mkzip '(a (b c)))))
          (eq? z (transform z 'd del))))
    (it "should return z with the element transformed by `f dst . rest` otherwise"
        (let ((z (mkzip '(a (b c))))
              (zp (mkzip '(a (b)))))
          (equal? zp (transform z 'c del))))
    (it "should return a zipper in the same position"
        (let ((z (go-down (go-right (mkzip '(a (b c))))))
              (zp (go-down (go-right (mkzip '(a (b)))))))
          (equal? zp (transform z 'c del))))))
