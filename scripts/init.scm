(use-modules (ice-9 popen)
             (srfi srfi-26)
             (system repl server)
             ((gram view)
              #:renamer (symbol-prefix-proc 'view-))
             (gram view hooks)
             (gram keysym)
             (gram keysym hooks)
             ((gram output)
              #:renamer (symbol-prefix-proc 'output-))
             (gram lib zipper)
             (gram lib motion)
             (gram lib render-hooks))

(spawn-server)

(display "Test from guile!\n")

;; (add-hook! view-created-hook (lambda (v) (display (output-get-views (view-get-output v)))))
;; (add-hook! view-created-hook (lambda (v) (display v)))

(define (run cmd)
  "Alias for `open-input-output-pipe'."
  (open-input-output-pipe cmd))

(define default-keymap '())

(define-syntax-rule (define-key! km key fn)
  "Adds `KEY' as a binding for `FN' to keymap `KM'."
  (set! km (assoc-set! km key fn)))

(define (keymap-hook km)
  (lambda (key view)
    (let ((fn (assoc-ref (primitive-eval km) key)))
      (when fn
        (let ((arity (car (assoc-ref (procedure-properties fn) 'arity))))
          ;; call either (fn) or (fn view) or (fn key view) based on
          ;; procedure arity
          (case arity
            [(0) (fn)]
            [(1) (fn view)]
            [(2) (fn key view)]))
        (swallow-next-key)))))

(add-hook! keydown-hook (keymap-hook 'default-keymap))

(define-key! default-keymap (kbd "M-x") (cute run "dmenu_run"))
(define-key! default-keymap (kbd "M-<Space>") (cute run "st"))
(define-key! default-keymap (kbd "M-b") (cute run "evince"))
(define-key! default-keymap (kbd "M-n") (cute move-cursor 'right))
(define-key! default-keymap (kbd "M-e") (cute move-cursor 'left))
(define-key! default-keymap (kbd "C-M-n") (cute move-window 'right))
(define-key! default-keymap (kbd "C-M-e") (cute move-window 'left))
(define-key! default-keymap (kbd "Mouse1") view-focus)
