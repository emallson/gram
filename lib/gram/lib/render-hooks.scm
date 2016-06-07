(define-module (gram lib render-hooks)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (gram lib zipper)
  #:use-module (gram lib render)
  #:use-module ((gram lib layout) #:select (tall simple))
  #:use-module ((gram view) #:renamer (symbol-prefix-proc 'view-))
  #:use-module (gram view hooks)
  #:use-module ((gram output) #:renamer (symbol-prefix-proc 'output-))
  #:use-module (gram output hooks)
  #:export (transform-workspace! transform-layout! current-view add-view re-render!))

(define %default-layout (tall))
(define %default-floating-layout (simple))
(define %output-list '())
(define %workspace-list '())
(define %current-output #nil)
(define %current-workspace #nil)

(define-record-type workspace
  (make-workspace name tiling-layout floating-layout float? focused)
  workspace?
  (name workspace-name set-workspace-name!)
  (focused focused-layout set-focused-layout!)
  (tiling-layout tiling-layout set-tiling-layout!)
  (floating-layout floating-layout set-floating-layout!)
  (float? float? set-float!))

(define (re-render!)
  (display "re-render! called") (newline)
  (render! %current-output (tiling-layout %current-workspace))
  (render! %current-output (floating-layout %current-workspace)))

(define (transform-workspace! layer f)
  "Transform the `layer' layout of the current workspace by applying f
to the appropriate layout.

`layer' should be either 'floating or 'tiling.

`f' should be a function taking a layout zipper and returning another
layout zipper."
  (case layer
    ((floating) (set-floating-layout! %current-workspace
                                      (f (floating-layout %current-workspace))))
    ((tiling) (set-tiling-layout! %current-workspace
                                  (f (tiling-layout %current-workspace))))
    ((both) (begin
              (transform-workspace! 'tiling f)
              (transform-workspace! 'floating f)))))

(define (transform-layout! f)
  "Transform the currently focused layout by `f'. See the definition
of `transform-workspace!' for more information."
  (transform-workspace! (focused-layout %current-workspace) f)
  (re-render!))

(define (current-view)
  (let ((vol (case (focused-layout %current-workspace)
               ((floating) (zipper-node (floating-layout %current-workspace)))
               ((tiling) (zipper-node (tiling-layout %current-workspace))))))
    (if (view-view? vol)
        vol
        #f)))

(define (should-float? view)
  (let ((types (view-get-types view)))
    (not (null? types))))

(define (get-workspace name)
  "Get a workspace from the alist if it exists. If not, create it with
the default tiling and floating layouts."
  (if (assoc name %workspace-list)
      (assoc-ref %workspace-list name)
      (let ((ws (make-workspace
                 name
                 (go (mkzip %default-layout) 'down)
                 (go (mkzip %default-floating-layout) 'down)
                 should-float?
                 'tiling)))
        (set! %workspace-list (acons name ws %workspace-list))
        ws)))

(define (get-first-unused-workspace out)
  "Returns the first workspace that is not used by any other output.
If none exists, one is created. If the given output has a workspace
already, it is returned."
  (if (assoc out %output-list)
      (assoc-ref %workspace-list (assoc-ref %output-list out))
      (let* ((names (map car %workspace-list))
             (used (map cadr %output-list))
             (unused (filter (lambda (s) (not (member s used))) names)))
        (if (null? unused)
            (get-workspace (string-append "default-" (output-get-name out)))
            (get-workspace (car unused))))))

(define (output-created out)
  (set! %output-list (acons out (workspace-name (get-first-unused-workspace out)) %output-list)))

(define (output-focused out focused?)
  (when focused?
    (unless (null? %current-output)
      (assoc-set! %output-list out (workspace-name %current-workspace)))
    (set! %current-output out)
    (set! %current-workspace (get-workspace (assoc-ref %output-list out)))))

(define (hide-workspace! ws)
  "Hides all of the views in the workspace `ws'."
  (transform-workspace! 'both (lambda (z) (zmap z (lambda (v) (if (view-view? v) (view-hide v) v))))))

(define (show-workspace! ws out)
  "Shows all of the views in workspace `ws' on the output `out'."
  (transform-workspace! 'both (lambda (z) (zmap z (lambda (v) (if (view-view? v) (view-show v out) v))))))

(define (switch-to-workspace name)
  "Switch to the workspace named `name'. If it does not exist, it is
created."
  (let ((ws (get-workspace name)))
    (hide-workspace! %current-workspace)
    (set! %current-workspace ws)
    (show-workspace! %current-workspace %current-output)
    (re-render!)))

(define (zipper-in-layout? zipper)
  (let ((up (go zipper 'up)))
    (if (zipper? up)
        (layout? (zipper-node up))
        #f)))

(define (add-view zipper view)
  (when (zipper-in-layout? zipper)
    (z-> zipper
         (insert view 'right)
         (go 'right))))

(define (view-created view)
  (transform-workspace! (if ((float? %current-workspace) view)
                            'floating 'tiling)
                        (cute add-view <> view))
  (view-show view %current-output)
  (view-focus view)
  (view-bring-to-front view)
  (re-render!))

(define (top-level? z)
  (match z
    (($ zipper _ #f #f #f) #t)
    (_ #f)))

(define (view-destroyed)
  (transform-workspace! 'both (lambda (z) (zfilter z view-active?)))
  (when (current-view)
      (view-focus (current-view)))
  (re-render!))

(define (view-handle-geometry view geo)
  (format #t "View ~a requested geometry ~a\n" view geo))

(add-hook! output-created-hook output-created)
(add-hook! output-focus-hook output-focused)
(add-hook! view-created-hook view-created)
(add-hook! view-request-geometry-hook view-handle-geometry)
(add-hook! view-destroyed-hook view-destroyed)
