(define-module (gram lib keymap)
  #:export (default-keymap define-key! keymap-hook)
  #:use-module (gram keysym))

(define default-keymap '("default" . ()))

(define (define-key! km key fn)
  "Adds `KEY' as a binding for `FN' to keymap `KM'."
  (set-cdr! km (assoc-set! (cdr km) key fn)))

(define (keymap-hook km)
  (lambda (key view)
    (let ((fn (assoc-ref km key)))
      (when fn
        (let ((arity (car (assoc-ref (procedure-properties fn) 'arity))))
          ;; call either (fn) or (fn view) or (fn key view) based on
          ;; procedure arity
          (case arity
            [(0) (fn)]
            [(1) (fn view)]
            [(2) (fn key view)]))
        (swallow-next-key)))))
