(define-module (gram support test-setup)
  #:use-module (srfi srfi-64)
  #:export (tap-runner describe it))

(define (tap-test-end runner)
  (let ((name (string-join (cdr (test-runner-group-path runner)) " "))
        (desc (test-runner-test-name runner)))
    (case (test-result-kind runner)
      ((pass xfail) (format #t "ok ~a ~a~%" name desc))
      ((fail xpass) (format #t "not ok ~a ~a~%" name desc))
      ((skip)       (format #t "ok ~a ~a # SKIP~%" name desc)))))

(define (tap-group-begin runner name count)
  (format #t "# Describe ~a~%" name)
  (test-runner-aux-value! runner (+ (test-runner-aux-value runner)
                                    (or count 0))))

(define (tap-test-final runner)
  (format #t "1..~a~%" (test-runner-aux-value runner)))

(define (tap-bad-count runner actual expected)
  (format #t "Bail out! Expected ~a tests in ~a. Ran ~a.~%"
          expected
          (car (test-runner-group-stack runner))
          actual)
  (exit -1))

(define (tap-runner)
  (let ((runner (test-runner-null)))
    (test-runner-on-test-end! runner tap-test-end)
    (test-runner-on-group-begin! runner tap-group-begin)
    (test-runner-on-bad-count! runner tap-bad-count)
    (test-runner-on-final! runner tap-test-final)
    (test-runner-aux-value! runner 0)
    runner))

(define-macro (describe name . tests)
  `(begin
     ,(let ((test-count (length (filter (lambda (test) (eq? (car test) 'it)) tests))))
        (if (= test-count 0)
            `(test-begin ,name)
            `(test-begin ,name ,test-count)))
     ,@tests
     (test-end ,name)))

(define-macro (it should . body)
  `(begin
     (test-assert ,should (begin ,@body))))
