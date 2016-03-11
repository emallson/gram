(define-module (gram support test-setup)
  #:use-module (srfi srfi-64)
  #:export (suite))

(define-macro (suite name . tests)
  `((@ (ggspec lib) suite) ,name
          (tests ,@(map (lambda (test)
                          `(test ,(string-append name " " (car test)) e ,(cadr test)))
                        tests))
          (options
           (option 'output-cb output-tap))))

(define-syntax-rule (describe name tests ...)
  (begin
    (map (lambda (test)
           (cons (list 'it name) (cdr test)))
         tests)))

(define-syntax-rule (it name desc body ...)
  (test-begin (string-append name " " desc)
              body ...))

(describe "foo"
          (it "should bar"
           #t)
          (it "should baz" #f))
