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

  (describe "go"
    (describe "left"
      (it "should return #f for non-zippers"
        (not (go #f 'left)))
      (it "should return #f if there is nothing to the left"
        (not (go (mkzip '(a b c d)) 'left)))
      (it "should return a zipper to the left otherwise"
        (equal? 'a (zipper-node (z-> (mkzip '(a b c d))
                                     (go 'down)
                                     (go 'right)
                                     (go 'left))))))

    (describe "right"
      (it "should return #f for non-zippers"
        (not (go #f 'right)))
      (it "should return #f if there is nothing to the right"
        (not (go (go (go (mkzip '(a b)) 'down) 'right) 'right)))
      (it "should return a zipper to the right otherwise"
        (equal? 'b (zipper-node (z-> (mkzip '(a b c d))
                                     (go 'down)
                                     (go 'right))))))

    (define-immutable-record-type zippable-test
      (make-zt children other)
      zt?
      (children zt-children set-zt-children)
      (other zt-other))

    (define-method (children (zt <zippable-test>))
      (zt-children zt))

    (define-method (extract kids (old-zt <zippable-test>))
      (set-zt-children old-zt kids))

    (describe "up"
      (it "should return #f for non-zippers"
        (not (go #f 'up)))
      (it "should return #f if there is nothing upwards"
        (not (go (mkzip '(a b c d)) 'up)))
      (it "should return a zipper up otherwise"
        (equal? '(a) (zipper-node (z-> (mkzip '((a) b c))
                                       (go 'down)
                                       (go 'down)
                                       (go 'up)))))
      (it "should use `extract' to go up from inside a record"
        (equal? (make-zt '(a b) 'c)
                (zipper-node (z-> (mkzip (make-zt '(a) 'c))
                                  (go 'down)
                                  (insert 'b 'right)
                                  (go 'up))))))

    (describe "down"
      (it "should return #f for non-zippers"
        (not (go #f 'down)))
      (it "should return #f if there is nothing downwards"
        (not (go (go (mkzip '(a b c d)) 'down) 'down)))
      (it "should return a zipper down otherwise"
        (equal? 'a (zipper-node (z-> (mkzip '((a) b c))
                                     (go 'down)
                                     (go 'down)))))
      (it "should use `children' to go down into a record"
        (equal? 'a
                (zipper-node (z-> (mkzip (make-zt '(a) 'c))
                                  (go 'down)))))))

  (describe "insert"
    (describe "left"
      (it "should return #f for non-zippers"
        (not (insert #f 'a 'left)))
      (it "should add an element to the left"
        (equal? '(a b) (unzip (z-> (mkzip '(b))
                                   (go 'down)
                                   (insert 'a 'left)))))
      (it "should leave the same node focused"
        (let ((z (go (mkzip '(b)) 'down)))
          (equal? (zipper-node z) (zipper-node (insert z 'a 'left)))))
      (it "should add an element to the left -- even when nested"
        (equal? '((b) (a c)) (unzip (z-> (mkzip '((b) (c)))
                                         (go 'down)
                                         (go 'right)
                                         (go 'down)
                                         (insert 'a 'left))))))

    (describe "right"
      (it "should return #f for non-zippers"
        (not (insert #f 'a 'right)))
      (it "should effectively set for the empty zipper"
        (equal? '(a) (unzip (z-> (mkzip '())
                                 (go 'down)
                                 (insert 'a 'right)))))
      (it "should add an element to the right"
        (equal? '(b a) (unzip (z-> (mkzip '(b))
                                   (go 'down)
                                   (insert 'a 'right)))))
      (it "should leave the same node focused"
        (let ((z (go (mkzip '(b)) 'down)))
          (equal? (zipper-node z) (zipper-node (insert z 'a 'right)))))
      (it "should add an element to the right -- even when nested"
        (equal? '((b) (c a)) (unzip (z-> (mkzip '((b) (c)))
                                         (go 'down)
                                         (go 'right)
                                         (go 'down)
                                         (insert 'a 'right)))))))

  (describe "set"
    (it "should return #f for non-zippers"
      (not (set #f 'a)))
    (it "should replace the current node in the new zipper"
      (equal? '(b) (unzip (set (go (mkzip '(a)) 'down) 'b))))
    (it "should replace the current node in the new zipper -- even when nested"
      (equal? '((b) (a)) (unzip (z-> (mkzip '((b) (c)))
                                     (go 'down)
                                     (go 'right)
                                     (go 'down)
                                     (set 'a))))))

  (describe "del"
    (it "should return #f for non-zippers"
      (not (del 'a)))
    (it "should replace the current node with #nil if no right or left"
      (equal? #nil (zipper-node (del (go (mkzip '(a)) 'down)))))
    (it "should replace the current node with (car right) if it exists"
      (equal? 'b (zipper-node (del (go (mkzip '(a b)) 'down)))))
    (it "should replace the current node with (car left) if it exists but not (car right)"
      (equal? 'a (zipper-node (z-> (mkzip '(a b))
                                   (go 'down)
                                   (go 'right)
                                   (del)))))
    (it "should remove the element from the zipper entirely"
      (equal? '() (unzip (del (go (mkzip '(a)) 'down))))))

  (describe "path"
    (it "should return #f for non-zippers"
        (not (path 'a)))
    (it "should return the empty list if at the top of the zipper"
        (null? (path (mkzip '(a)))))
    (it "should return the sequence to reach the current zipper position otherwise"
        (equal? (list 'down 'right 'down)
                (path (z-> (mkzip '(a (b c)))
                           (go 'down)
                           (go 'right)
                           (go 'down))))))

  (describe "replay"
    (it "should return z itself for an empty path"
        (let ((z (mkzip '(a))))
          (eq? (replay z '()) z)))
    (it "should return z after the steps in the path have been applied"
        (let ((z (mkzip '(a (b c)))))
          (equal? (replay z (list 'down 'right 'down))
                  (z-> z (go 'down) (go 'right) (go 'down))))))

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
      (let ((z (z-> (mkzip '(a (b c)))
                    (go 'down)
                    (go 'right)))
            (zp (z-> (mkzip '(a (b)))
                     (go 'down)
                     (go 'right))))
          (equal? zp (transform z 'c del))))))
