;;--- system-hooks.scm
;; ten plik definiuje hooki "systemowe", t.j. odpowiedzialne za menu, gui, etc


; TODO: HACK: to powinna byc funkcja pytajaca typu (can-handle-click?)
; i ogolnie user-hooki zalezne od tego i system-hooki niezalezne
(define *current-mode* nil)

(define *clipboard* nil)

(define *click-can-be-handled* #t)
(define *keypress-can-be-handled* #t)

(define *current-click-handler* #f)
(define *current-keypress-handler* #f)

;; ZMIANA POZYCJI DANEGO SOURCE_T
(define *source-size* 20) ;; TODO: ?????
(define repositioning-source #f)
(define repositioning-dx 0)
(define repositioning-dy 0)

(define (reposition-source-hook first left right)
  (when (or *click-can-be-handled* repositioning-source)
    (let ((mp (get-mouse-position)))
      (when (and first left)
        (for-each
          (lambda (n)
            (let* ((s (list-ref *sources* n))
                   (x (- (caar s) *source-size*))
                   (y (- (cdr (car s)) *source-size*)))
              (when (point-in-rect?
                     mp (list (+ x (/ *source-size* 2))
                              (+ y (/ *source-size* 2)) *source-size* *source-size*))
                (set-cursor MOUSE-CURSOR-RESIZE-ALL)
                (set! *click-can-be-handled* #f)
                (set! *current-click-handler* 'REPOSITION-SOURCE-HOOK)
                (set! repositioning-dx (- (car mp) x *source-size*))
                (set! repositioning-dy (- (cdr mp) y *source-size*))
                (set! repositioning-source n))))
          (iota 0 1 (length *sources*))))
      (when (and (not first) repositioning-source)
        (let ((pos (cons (- (car mp) repositioning-dx)
                         (- (cdr mp) repositioning-dy))))
          (set-source-e! repositioning-source 'pos pos))))))

(define (reposition-source-end-hook first left right)
  (when repositioning-source
    (set-cursor MOUSE-CURSOR-DEFAULT)
    (set! *click-can-be-handled* #t)
    (set! repositioning-source #f)))

(add-system-hook 'click reposition-source-hook)
(add-system-hook 'unclick reposition-source-end-hook)

;; RYSOWANIE ZWIERCIADEŁ
(define mirror-last-x 0)
(define mirror-last-y 0)
(define drawing-new-mirror #f)

(define (start-drawing-mirror-hook first left right)
  (when (and (eqv? *current-mode* 'mirror-drawing) (or *click-can-be-handled* drawing-new-mirror) (not right))
    (set! *click-can-be-handled* #f)
    (set! *current-click-handler* 'START-DRAWING-MIRROR-HOOK)
    (when (and first left)
      (set! drawing-new-mirror #t)
      (set! mirror-last-x (car (get-mouse-position)))
      (set! mirror-last-y (cdr (get-mouse-position))))
    (when (and (not first) drawing-new-mirror)
      (draw-line `(,mirror-last-x . ,mirror-last-y)
                 `(,(car (get-mouse-position)) . ,(cdr (get-mouse-position)))
                 2
                 (aq 'drawing-new-mirror *colorscheme*)))))

(define (end-drawing-mirror-hook first left right)
  (when drawing-new-mirror
    (set-cursor MOUSE-CURSOR-DEFAULT)
    (set! *click-can-be-handled* #t)
    (set! drawing-new-mirror #f)
    (set! *current-mode* nil)
    (create-mirror
      mirror-last-x mirror-last-y
      (car (get-mouse-position)) (cdr (get-mouse-position)))))

(add-system-hook 'click start-drawing-mirror-hook)
(add-system-hook 'unclick end-drawing-mirror-hook)

;; używa *wait-alist* z scm/util.scm
(define (wait-handler time)
  "handler dla funkcji `(wait)`"
  (for-each
    (→1 ((cdr x)))
    (filter (→1 (<= (car x) time)) *wait-alist*))
  (set! *wait-alist* (filter (→1 (not (<= (car x) time))) *wait-alist*)))

(add-system-hook 'clock wait-handler)

(define (create-source-at-mouse-position)
  (create-source `((pos . ,(get-mouse-position)) (reactive . #f))))

;; DOMYŚLNE KEYBINDINGI
(define hook-status-dest nil)
(define window-opts-dest nil)
(define fps-dest nil)

(define (keypress-default-hook c _)
  (when *keypress-can-be-handled*
    (let* ((mp (get-mouse-position)))
      (cond
        ((eqv? c #\A) (create-source-at-mouse-position))
        ((eqv? c #\e) (letrec ((id (add-hook
                                   'frame
                                   (→ (gui/input-popup "eval scheme" loads)
                                      (delete-hook 'frame id)))))))
        ((eqv? c #\`)
         (if (null? fps-dest)
             (begin
               (set! fps-dest (gui/show-fps '(16 . 16)))
               (set! hook-status-dest (gui/show-hook-status)))
             (begin
               (hook-status-dest)
               (fps-dest)
               (set! hook-status-dest nil)
               (set! fps-dest nil))))
        ((eqv? c #\R) (experimental/toggle-resizable))
        ((eqv? c #\M) (toggle-mode-display))
        ((eqv? c #\~)
         (if (null? window-opts-dest)
             (set! window-opts-dest (gui/show-window-opts))
             (begin
               (window-opts-dest)
               (set! window-opts-dest nil))))
        ((eqv? c #\q) (exit 0))))))

(add-system-hook 'keypress keypress-default-hook)

(define (src->rect pos)
  (list (- (car pos) (/ *source-size* 2))
        (- (cdr pos) (/ *source-size* 2)) *source-size* *source-size*))

(define (option-menu-for-lens ids)
  `(("zmień r" . ,(→ (gui/mp-slider+ok
                      5 200.0
                      (lambda (v) (map (→1 (set-lens-e! x 'r v)) ids))
                      1)))
    ;; ("zmień r2" . ,(→ (gui/mp-slider+ok
    ;;                    0.1 100.0
    ;;                    (→1 (set-lens-e! id 'r2 id))
    ;;                    1)))
    ("zmień d" . ,(→ (gui/mp-slider+ok
                      5 200.0
                      (lambda (v) (map (→1 (set-lens-e! x 'd v)) ids))
                      0)))
    ("kopiuj" . ,(→ (set!
                     *clipboard*
                     (map (→1 (serialize:bounceable->sexp (get-bounceable x))) ids))))
    ("usuń" . ,(→ (for-each delete-bounceable ids)
                  (when (eqv? *current-mode* 'selected)
                    (set! sel-mode:should-end-selected-mode #t))))))

(define (option-menu-for-prism ids)
  `(("zmień współczynnik załamania pryzmatu" . ,(→ (gui/mp-slider+ok
                                                    1.0 2.0
                                                    (lambda (v) (map (→1 (set-prism-e! x 'n v)) ids))
                                                    3)))
    ("zmień wielkość boku" . ,(→ (gui/mp-slider+ok
                                  1 500
                                  (lambda (v) (map (→1 (set-prism-e! x 'vert-len v)) ids))
                                  0)))
    ("kopiuj" . ,(→ (set!
                     *clipboard*
                     (map (→1 (serialize:bounceable->sexp (get-bounceable x))) ids))))
    ("usuń" . ,(→ (for-each delete-bounceable ids)
                  (when (eqv? *current-mode* 'selected)
                    (set! sel-mode:should-end-selected-mode #t))))))

(define (option-menu-for-source ids)
  (append
   `(("zmień kąt" . ,(→ (gui/mp-slider+ok
                         0 359
                         (lambda (v)
                           (map (→1 (set-source-e! x 'mouse-reactive #f)
                                    (set-source-e! x 'angle v))
                                ids))
                         0)))
     ("zmień kolor" . ,(→ (gui/change-source-color-form
                           (get-mouse-position)
                           (lambda (v) (map (→1 (set-source-e! x 'color v)) ids)))))
     ("'mouse-reactive" . ,(→ (map (→1 (set-source-e! x 'mouse-reactive (not (list-ref (list-ref *sources* x) 3)))) ids))))
   (if (all (→1 (white? (list-ref (list-ref *sources* x) 5))) ids)
       `(("zmień szerokość wiązki" . ,(→ (gui/mp-slider+ok
                                          1 10
                                          (lambda (v) (map (→1 (set-source-e! x 'n-beams v)) ids))
                                          0))))
       '())
   (if (all (→1 (not (white? (list-ref (list-ref *sources* x) 5)))) ids)
       `(("zmień ilość wiązek" . ,(→ (gui/mp-slider+ok
                          0 *source-size*
                          (lambda (v) (map (→1 (set-source-e! x 'n-beams v)) ids))
                          0))))
       '())
   `(("kopiuj" . ,(→ (set!
                      *clipboard*
                      (map (→1 (serialize:source->sexp (list-ref *sources* x))) ids))))
     ("usuń" . ,(→ (delete-sources ids)
                   (when (eqv? *current-mode* 'selected)
                     (set! sel-mode:should-end-selected-mode #t)))))))

(define (option-menu-for T id-or-ids)
  (let ((ids (if (list? id-or-ids) id-or-ids (list id-or-ids))))
    (if (memv T '(source prism lens))
        (let* ((f-name (string->symbol (string-append "option-menu-for-" (symbol->string T))))
               (f (eval f-name)))
          (f ids))
        (error "option-menu-for: unknown T: " T))))

;; r-click dla źródeł
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((pos (car (list-ref *sources* x)))
                  (cur (list-ref *sources* x)))
              (when (point-in-rect? mp (src->rect pos))
                (when (null? *current-mode*)
                  (set! *current-mode* 'r-click-source))
                (set! *gui/option-menu-force-can-be-handled* #t)
                (gui/option-menu
                 (get-mouse-position)
                 (option-menu-for 'source x)
                 (→ (when (eqv? *current-mode* 'r-click-source)
                      (set! *current-mode* nil)))))))
        (⍳ 0 1 (length *sources*)))))))

;; r-click dla pryzmatów
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((id (list-ref x 0))
                  (center (list-ref x 1))
                  (vert-len (list-ref x 5)))
              (when (point-in-triangle? mp center vert-len)
                (when (null? *current-mode*)
                  (set! *current-mode* 'r-click-prism))
                (set! *gui/option-menu-force-can-be-handled* #t)
                (gui/option-menu
                 (get-mouse-position)
                 (option-menu-for 'prism (car x))
                 (→ (when (eqv? *current-mode* 'r-click-prism)
                      (set! *current-mode* nil)))))))
        *prisms*)))))

;; r-click dla soczewek
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((id (list-ref x 0)))
              (when (point-in-lens? mp id)
                (when (null? *current-mode*)
                  (set! *current-mode* 'r-click-lens))
                (set! *gui/option-menu-force-can-be-handled* #t)
                (gui/option-menu
                 (get-mouse-position)
                 (option-menu-for 'lens (car x))
                 (→ (when (eqv? *current-mode* 'r-click-lens)
                      (set! *current-mode* nil)))))))
        *lenss*)))))

;; mouse-menu
(define (_open-menu vs)
  (set! *click-can-be-handled* #f)
  (set! *gui/option-menu-force-can-be-handled*)
  (gui/option-menu
   (get-mouse-position) vs (→ (set! *click-can-be-handled* #t))))

(define new-thing-menu
  `(("źródło" . ,(→ (set! gui/new-source-form:pos (get-mouse-position))
                    (gui/new-source-form)))
    ("zwierciadło" . ,(→ (when (eqv? *current-mode* nil)
                           (set-cursor MOUSE-CURSOR-CROSSHAIR)
                           (tracelog 'info "narysuj nowe zwierciadło...")
                           (set! *current-mode* 'mirror-drawing))))
    ("pryzmat" . ,(→ (create-prism (get-mouse-position) 100 1.31)))
    ("soczewkę" . ,(→ (create-lens (get-mouse-position) 20 120)))))

(define advanced-menu
  `(("wyrażenie scheme" . ,(→ (gui/input-popup "eval" loads)))
    ("wyczyść *tracelog-queue*" . ,(→ (set! *tracelog-queue* nil)))))

(define mouse-menu
  `(("wstaw" . ,(→ (_open-menu new-thing-menu)))
    ("wklej" . ,(→ (if (> (length *clipboard*) 0)
                     (let ((ids (map eval *clipboard*)))
                       (set! sel-mode:selected-bounceable-ids (filter number? ids))
                       (set! sel-mode:selected-source-ids (map cdr (filter pair? ids)))
                       (start-selected-mode))
                     (tracelog 'info "*clipboard* jest puste"))))
    ("zapisz scenę do pliku" . ,(→ (gui/save-current)))
    ("załaduj przykład" . ,(→ (gui/load-example-menu)))
    ("zaawansowane" . ,(→ (_open-menu advanced-menu)))))

(add-hook
 'unclick
 (lambda (first l r)
   (when (and *click-can-be-handled* r (eqv? *current-mode* nil))
     (_open-menu mouse-menu))))

;;;; tracelog
(define *tracelog-queue* '())

;; witam chcialem tylko powiedziec ze system tracelogow trzyma sie na dykcie i gownie
;; pozdrawiam serdecznie
;; ~ kpm
(add-hook
 'log
 (lambda (type s)
   (set! *tracelog-queue*
         (append *tracelog-queue* `(((s . ,s) (time . ,(time)) (type . ,type)))))))

(define (display-next-log)
  "ale fajna funkcja ciekawe jak dziala :333"
  (if (> (length *tracelog-queue*) 0)
    (let* ((tl (car *tracelog-queue*))
           (s (string-append
               "[" (number->string (aq 'time tl)) "] "
               (symbol->string (aq 'type tl)) ": "
               (aq 's tl)))
           (id (add-hook 'frame (→ (draw-text s '(0 . 0) 16 (aq 'font *colorscheme*) *default-spacing*)))))
      (set! *tracelog-queue* (cdr *tracelog-queue*))
      (wait 2 (→ (delete-hook 'frame id)
                 (display-next-log))))
    (letrec ((id (add-hook 'log (→2 (display-next-log)        ; korzystam tu z faktu,
                                    (delete-hook 'log id))))) ; że hooki wykonywane są kolejno
      nil)))                                                  ; (od najstarszych do najnowszych)

(display-next-log) ; lol

(add-hook
 'resize
 (→2 (set! *SCREEN-WIDTH* x)
     (set! *SCREEN-HEIGHT* y)))

;; TODO: przenieś do util.scm
(define (rect-collision? r1 r2)
  "sprawdza czy dwa `r1` i `r2` mają punkty wspólne. zwraca `#f | #t`"
  (not (eqv? (sum (rect-collision r1 r2)) 0.0)))

;; przyjmuję trójkąt równoboczny
;; :3333
(define (triangle->rect p1 p2 p3)
  (let* ((x1 (car p1))
         (y1 (cdr p1))
         (x2 (car p2))
         (y2 (cdr p2))
         (x3 (car p3))
         (y3 (cdr p3))
         (a (- (max x1 x2 x3) (min x1 x2 x3)))
         (h (* a (sqrt 3) 0.5)))
    (list (min x1 x2 x3) (min y1 y2 y3) a h)))

(define (prism->ptlist p)
  (list (list-ref p 2) (list-ref p 3) (list-ref p 4)))

(define (reposition-source-by-delta id ∆)
  (let ((pos (car (list-ref *sources* id))))
    (set-source-e! id 'pos (cons (+ (car pos) (car ∆))
                                 (+ (cdr pos) (cdr ∆))))))

(define (reposition-mirror-by-delta id ∆)
  (let* ((mirror (get-bounceable id))
         (p1 (cadr mirror))
         (p2 (caddr mirror))
         (p1-new (cons (+ (car p1) (car ∆))
                       (+ (cdr p1) (cdr ∆))))
         (p2-new (cons (+ (car p2) (car ∆))
                       (+ (cdr p2) (cdr ∆)))))
    (set-mirror! id p1-new p2-new)))

(define (reposition-prism-by-delta id ∆)
  (let* ((prism (get-bounceable id))
         (center (cadr prism))
         (center-new (cons (+ (car center) (car ∆))
                           (+ (cdr center) (cdr ∆))))
         (vert-len (list-ref prism 5))
         (n (list-ref prism 6)))
    (set-prism! id center-new vert-len n)))

(define (reposition-lens-by-delta id ∆)
  (let* ((lens (get-bounceable id))
         (r (list-ref lens 3))
         (center (list-ref lens 4))
         (d (list-ref lens 5)))
    (set-lens! id (cons (+ (car center) (car ∆)) (+ (cdr center) (cdr ∆))) d r)))

(define (reposition-bounceable-by-delta id ∆)
  (let* ((thing (get-bounceable id))
         (type (car thing)))
    (cond
     ((eqv? type 'mirror) (reposition-mirror-by-delta id ∆))
     ((eqv? type 'prism)  (reposition-prism-by-delta id ∆))
     ((eqv? type 'lens)   (reposition-lens-by-delta id ∆))
     (else
      (error (string-append (->string type) " unsupported"))))))

(define (lens->rect lens)
  (let* ((p1 (list-ref lens 1))
         (p2 (list-ref lens 2))
         (d (list-ref lens 5)))
    (list (- (car p1) (/ d 2))
             (cdr p1)
             d
             (- (cdr p2) (cdr p1)))))

(define (thing->rect thing)
  (let ((type (car thing)))
    (cond
     ((eqv? type 'mirror) (pts->rect (cadr thing) (caddr thing)))
     ((eqv? type 'prism) (apply triangle->rect (prism->ptlist thing)))
     ((eqv? type 'lens) (lens->rect thing))
     (else
      (error (string-append "thing->rect: unsupported" (->string thing)))))))

;;; "toplist"y - listy z przedmiotami
(define *mirrors* nil)
(define *prisms* nil)
(define *customs* nil) ;; tak, wiem, że te nazwy nie znaczą tego co mają znaczyć
(define *lenss* nil)   ;; i są niepoprawnymi słowami
                       ;; ale ułatwiają potem ustawianie rzeczy

(define (update-toplist l id)
  (eval `(set! ,l (map (→1 (if (eqv? (car x) ,id)
                               (append (list ,id) (cdr (get-bounceable ,id)))
                               x))
                       ,l))))

(define (add-bounceable-to-toplist l id)
  (eval `(set! ,l (append ,l (list (append (list ,id) (cdr (get-bounceable ,id))))))))

(define (delete-from-toplist l id)
  (eval `(set! ,l (filter (→1 (not (eqv? (car x) ,id))) ,l))))

;;;----- HOOKI dla bounceable_t i *mirrors*, *prisms* etc.
;; hooki wykonywane z argumentami 'TYP ..dane
;; jako że toplisty nazywają się *TYPs*, dodaję po prostu do typu gwiazdki po obu stronach i -s na koniec
;; i mam nazwę zmiennej
;; stąd właśnie (string->symbol (string-append "*" (symbol->string x) "s*"))
;; XDDD
;; ~ kpm

(add-hook
 'new
 (→2 (add-bounceable-to-toplist (string->symbol (string-append "*" (symbol->string x) "s*")) y)))

(add-hook
 'update
 (→2 (update-toplist (string->symbol (string-append "*" (symbol->string x) "s*")) y)))

(add-hook
 'delete
 (→2 (delete-from-toplist (string->symbol (string-append "*" (symbol->string x) "s*")) y)))

(define *mode-display-on* #t)

(define (toggle-mode-display)
  (set! *mode-display-on* (not *mode-display-on*)))

(add-hook
 'frame
 (→ (when *mode-display-on*
      (draw-text
       (string-append
        (if (null? *current-mode*)
            "normal"
            (symbol->string *current-mode*))
        "-mode")
       (cons 16 (- *SCREEN-HEIGHT* 32)) 16 (aq 'font *colorscheme*)))))

;; ładowanie wrzuconych plików
(define (load-files-handler . vs)
  (for-each load vs))

(add-hook 'files-dropped load-files-handler)

