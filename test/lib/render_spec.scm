(use-modules (ggspec lib)
             (gram lib render)
             (gram support test-setup))

(define remove-keys (@@ (gram lib render) remove-keys))

(suite "remove-keys"
       ("should remove all keywords from the input"
        (assert-false (> 0 (length (remove-keys '(#:a #:b))))))
       ("should remove all key/value pairs from the input"
        (assert-false (> 0 (length (remove-keys '(#:a 1 #:b 2))))))
       ("should not remove any value that is not a keyword and not paired with a keyword"
        (assert-equal '(2 3 4 5) (remove-keys '(#:a 1 2 3 #:b 2 4 5 #:c)))))

(define flatten-once (@@ (gram lib render) flatten-once))

(suite "flatten-once"
       ("should flatten a list by exactly one level"
        (assert-equal '(a (b (c d)) e f g) (flatten-once '(a ((b (c d)) e f) g)))))
