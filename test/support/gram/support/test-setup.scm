(define-module (gram support test-setup)
  #:export (suite))
(use-modules (ggspec lib))

(define-macro (suite name . tests)
  `((@ (ggspec lib) suite) ,name
          (tests ,@(map (lambda (test)
                          `(test ,(string-append name " " (car test)) e ,(cadr test)))
                        tests))
          (options
           (option 'output-cb output-tap))))
