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
             (gram lib render-hooks)
             (gram lib keymap))

(spawn-server)

(display "Test from guile!\n")

;; (add-hook! view-created-hook (lambda (v) (display (output-get-views (view-get-output v)))))
;; (add-hook! view-created-hook (lambda (v) (display v)))

(define (run cmd)
  "Alias for `open-input-output-pipe'."
  (open-input-output-pipe cmd))


(add-hook! keydown-hook (keymap-hook 'default-keymap))

(define-key! default-keymap (kbd "M-x") (cute run "dmenu_run"))
(define-key! default-keymap (kbd "M-<Space>") (cute run "st"))
(define-key! default-keymap (kbd "M-b") (cute run "evince"))
(define-key! default-keymap (kbd "M-n") (cute move-cursor 'right))
(define-key! default-keymap (kbd "M-e") (cute move-cursor 'left))
(define-key! default-keymap (kbd "C-M-n") (cute move-window 'right))
(define-key! default-keymap (kbd "C-M-e") (cute move-window 'left))
(define-key! default-keymap (kbd "Mouse1") view-focus)
