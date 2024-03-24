;;-- już-zrobione przykłady które można załadować

(define *examples* '())
(define *current-example* nil)

(define (e:delete-all)
  (delete-all-sources)
  (for-each
   delete-bounceable
   (map car (append *mirrors* *customs* *prisms* *lenss*))))

(define (load-example n)
  ((cdr (list-ref *examples* n))))

(define (define-example nam user-f)
  (let ((f (→ (eval '(e:delete-all))
              (user-f)
              (tracelog 'info (string-append "załadowano przykład \"" nam "\"")))))
    (set! *examples* (append *examples* (list (cons nam f))))))

(define-example "źródło i pryzmaty"
  (→ (create-prism '(305 . 326) 100.0 1.309999943)
     (create-prism '(552 . 364) 203.0 1.7)
     (create-source '((pos 88 . 359) (angle . 353) (n-beams . 7) (reactive . #f) (n-beams . 1) (color 255 255 255 255)))))

(define-example "źródło światła białego i pryzmat"
  (→ (create-source `((pos . (150 . 370))
                      (reactive . #f)
                      (angle . 340)
                      (n-beams . 10)
                      (color . ,white)))
     (create-prism '(400 . 320) 120 1.31)))

(define-example "laser i soczewka"
  (→ (create-lens '(413 . 282) 20.0 163.1666718)
     (create-source '((pos 45 . 283) (angle . 0) (reactive . #f) (n-beams . 19) (color 255 0 0)))))

(define-example "pełzające źródło"
  (→ (let* ((pos '(100 . 0))
            (ang 45)
            (s-id (cdr (create-source
                        `((pos . ,pos)
                          (angle . ,ang)
                          (color . ,white)
                          (n-beams . 8)
                          (reactive . #f)))))
            (sz (get-screen-size))
            (step-size (/ 45.0 (cdr sz)))
            (prism-id (create-prism '(439 . 321) 183.0 1.309999943))
            (f-id (add-hook
                   'frame
                   (→ (set! pos (cons (car pos) (+ 0.5 (cdr pos))))
                      (set! ang (- ang step-size))
                      (when (<= ang -45)
                        (set! pos (cons 100 0))
                        (set! ang 45))
                      (set-source-e! s-id 'angle (normalize-angle ang))
                      (set-source-e! s-id 'pos pos))))
            (k-id (add-hook
                   'keypress
                   (→2 (when (and *keypress-can-be-handled* (eqv? x #\;))
                         (delete-hook 'frame f-id)
                         (delete-hook 'keypress k-id))))))
       (tracelog 'info "kliknij średnik by zakończyć"))))

(define (rand-float)
  (/ (random-next) 2147483647))

;; (define (px pt)
;;   (add-mirror pt (cons (+ 2 (car pt)) (+ 2 (cdr pt)))))

;; lol
;; (define-example "fern"
;;   (→ (let ((max-iter 4096)
;;            (x 0.0)
;;            (y 0.0)
;;            (xn 0.0)
;;            (yn 0.0))
;;        (for-each
;;         (lambda (n) (let ((r (rand-float)))
;;                  (cond
;;                   ((< r 0.01)
;;                    (set! xn 0.0)
;;                    (set! yn (* 0.16 y)))
;;                   ((< r 0.86)
;;                    (set! xn (+ (* 0.85 x) (* 0.04 y)))
;;                    (set! yn (+ (* -0.04 x) (* 0.85 y) 1.6)))
;;                   ((< r 0.93)
;;                    (set! xn (- (* 0.2 x) (* 0.26 y)))
;;                    (set! yn (+ (* 0.23 x) (* 0.22 y) 1.6)))
;;                   (else
;;                    (set! xn (+ (* -0.15 x) (* 0.28 y)))
;;                    (set! yn (+ (* 0.26 x) (* 0.24 y) 0.44))))
;;                  (let ((xr (* (/ (+ xn 3) 6) *SCREEN-WIDTH*))
;;                        (yr (* (/ yn 10) *SCREEN-HEIGHT*)))
;;                    (px (cons xr yr)))
;;                  (set! x xn)
;;                  (set! y yn)))
;;         (⍳ 0 1 max-iter)))))
