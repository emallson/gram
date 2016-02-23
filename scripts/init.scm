(use-modules (ice-9 popen)
             (srfi srfi-26)
             ((gram view)
              #:renamer (symbol-prefix-proc 'view-))
             (gram keysym)
             ((gram output)
              #:renamer (symbol-prefix-proc 'output-)))

(display "Test from guile!\n")

(add-hook! view-created-hook (lambda (v) (display (output-get-views (view-get-output v)))))
(add-hook! view-created-hook (lambda (v) (display v)))

(define (run cmd)
  "Alias for `open-input-output-pipe'."
  (open-input-output-pipe cmd))

(define default-keymap '())

(define-syntax-rule (define-key! km key fn)
  "Adds `KEY' as a binding for `FN' to keymap `KM'."
  (set! km (assoc-set! km key fn)))

(define (keymap-hook km)
  (lambda (key)
    (let ((fn (assoc-ref (primitive-eval km) key)))
      (when fn
        (fn)
        (swallow-next-key)))))

(add-hook! keydown-hook (keymap-hook 'default-keymap))

(define-key! default-keymap (kbd "M-x") (cute run "bemenu-run"))
(define-key! default-keymap (kbd "M-<Space>") (cute run "st"))
