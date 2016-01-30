(use-modules (ice-9 popen))

(display "Test from guile!\n")

(add-hook! view-created-hook display)

(define (run cmd)
  "Alias for `open-input-output-pipe'."
  (open-input-output-pipe cmd))

(define (m-x-menu keysym)
  (when (equal? keysym (kbd "M-x"))
    (swallow-next-key)
    (run "bemenu-run")))

(add-hook! keydown-hook display)
(add-hook! keydown-hook m-x-menu)
