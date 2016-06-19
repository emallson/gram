(define-module (gram lib keymap)
  #:export (default-keymap define-key! clear-key! keymap-hook)
  #:use-module (gram keysym)
  #:use-module (ice-9 optargs))

(define default-keymap '("default" . ()))

(define* (define-key! km key fn #:optional (swallow? #t))
  "Adds `KEY' as a binding for `FN' to keymap `KM'."
  (set-cdr! km (assoc-set! (cdr km) key (cons fn swallow?))))

(define (clear-key! km key)
  "Removes `KEY' as a binding from keymap `KM'."
  (set-cdr! km (assoc-remove! (cdr km) key)))

(define (keymap-hook km)
  (lambda (key view)
    (let ((pair (assoc-ref km key)))
      (when pair
        (let ((fn (car pair))
              (swallow? (cdr pair)))
          (let ((arity (car (assoc-ref (procedure-properties fn) 'arity))))
            ;; call either (fn) or (fn view) or (fn key view) based on
            ;; procedure arity
            (case arity
              [(0) (fn)]
              [(1) (fn view)]
              [(2) (fn key view)]))
          (if swallow?
              (swallow-next-key)))))))
