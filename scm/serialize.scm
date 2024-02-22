;;; zapisywanie i odczyt z plików.

(define (serialize:bounceable->sexp b)
  (let ((type (car b)))
    (cond
     ((eqv? type 'mirror)
      `(add-mirror (quote ,(list-ref b 1)) (quote ,(list-ref b 2))))
     ((eqv? type 'prism)
      `(create-prism (quote ,(list-ref b 1)) ,(list-ref b 5) ,(list-ref b 6)))
     ((eqv? type 'lens)
      `(create-lens (quote ,(list-ref b 4)) ,(list-ref b 5) ,(list-ref b 3)))
     (else
      (error "unsupported type: " type)))))

(define (serialize:source->sexp s)
  (let ((pt (list-ref s 0))
        (angle (list-ref s 1))
        (thickness (list-ref s 2))
        (reactive (list-ref s 3))
        (n-beams (list-ref s 4))
        (color (list-ref s 5)))
    `(create-source '((pos . ,pt)
                      (angle . ,angle)
                      (thickness . ,thickness)
                      (reactive . ,reactive)
                      (n-beams . ,n-beams)
                      (color . ,color)))))

(define (serialize:print sexp)
  (display sexp)
  (newline))

(define (serialize:save-to filename)
  (with-output-to-file filename
    (→ (for-each
        serialize:print
        (map serialize:bounceable->sexp (get-all-bounceables)))
       (for-each
        serialize:print
        (map serialize:source->sexp *sources*))))
  (tracelog 'info (string-append "saved to " filename)))

(define (serialize:read-sexps f acc)
  (let ((sexp (read f)))
    (cond
     ((eof-object? sexp) acc)
     (else
      (serialize:read-sexps f (append acc (list sexp)))))))

(define serialize:read-from load)
