; TODO: sprawdź czy przypadkiem używanie wbudowanej funkcji w raylib nie będzie
; szybsze

(define gui/draw-text draw-text)

(define (gui/rect rect c)
  (let ((x (list-ref rect 0))
        (y (list-ref rect 1))
        (w (list-ref rect 2))
        (h (list-ref rect 3)))
    (draw-line `(,(- x 1) . ,y) `(,(+ x w) . ,y)       1 c)
    (draw-line `(,x . ,(+ y h)) `(,(+ x w) . ,(+ y h)) 1 c)
    (draw-line `(,x . ,y)       `(,x       . ,(+ y h)) 1 c)
    (draw-line `(,(+ x w) . ,y) `(,(+ x w) . ,(+ y h)) 1 c)))

(define gui/window-top-bar-size 16)

(define (gui/window-box-get-empty-space rect)
  (list (car rect) (+ (cadr rect) gui/window-top-bar-size)
        (caddr rect) (- (cadddr rect) gui/window-top-bar-size)))

(define (gui/window-box rect title)
  "rysuje bounding-box okienka wraz z tytułem, zwraca miejsce, które pozostało na elementy"
  (args
   '((rect . "prostokąt `(x y w h)`")
     (title . "tytuł")))

  (gui/rect rect (aq 'frame *colorscheme*)) ; bounding box
  (gui/rect `(,(car rect) ,(cadr rect) ,(caddr rect) ,gui/window-top-bar-size)
            (aq 'frame *colorscheme*))
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) (aq 'font *colorscheme*))
  (gui/window-box-get-empty-space rect))

(define (gui/window-box-retained rect title)
  "rysuje window-box, tylko, że dodaje hooki dla 'frame. zwraca `(destruktor to-co-gui/window-box)`"
  (let ((frame-id (add-hook 'frame (→ (gui/window-box rect title)))))
    (list
     (→ (delete-hook 'frame frame-id))
     (gui/window-box-get-empty-space rect))))

(define (gui/input-box rect text)
  (error "not implemented"))

(define (gui/label rect text)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) (aq 'font *colorscheme*)))

(define (gui/get-max-text-length-for-width w sz)
  (letrec ((f (lambda (s)
                (cond
                  ((>= (car (measure-text s sz)) w) (- (string-length s) 1))
                  (else
                    (f (string-append s "a")))))))
    (f "a")))

(define (gui/multiline-text rect txt cursor-at)
  (let* ((w (list-ref rect 2))
         (text-height (cdr (measure-text "a" 18)))
         (max-len (gui/get-max-text-length-for-width w 18))
         (text (map list->string (split-every (string->list txt) max-len)))
         (cursor-x (modulo cursor-at max-len))
         (cursor-y (round-off (/ cursor-at max-len) 0)))
    (for-each
     (lambda (n)
       (when (equal? n cursor-y)
         (let* ((pt-orig (cons (+ (car rect) (car (measure-text (substring (list-ref text n) 0 cursor-x) 18)))
                               (+ (cadr rect) (* cursor-y text-height))))
                (pt (cons (+ 2 (car pt-orig)) (cdr pt-orig))))
           (draw-line pt (cons (car pt) (+ (cdr pt) text-height)) 1 (aq 'font *colorscheme*))))
       (draw-text (list-ref text n)
                  `(,(list-ref rect 0) . ,(+ (list-ref rect 1)
                                             (* n text-height) 2)) 18 (aq 'font *colorscheme*)))
     (iota 0 1 (length text)))))

(define gui/input-popup:ident 'GUI-input-popup)
(define *gui/input-popup-force-can-be-handled* #f)

(define (gui/input-popup title callback . force)
  (when (or *click-can-be-handled* force)
    (stop-simulation)
    (define state "")
    (define cursor-at 0)
    (set! *current-mode* 'input-popup)
    (set! *click-can-be-handled* #f)
    (set! *keypress-can-be-handled* #f)
    (set! *current-keypress-handler* gui/input-popup:ident)
    (set! *current-click-handler*    gui/input-popup:ident)

    (let* ((frame-handler-id
            (add-hook
             'frame
             (→ (gui/window-box '(100 100 600 400) title)
                (gui/multiline-text '(200 200 400 200) state cursor-at))))
           (key-handler-id
            (add-hook
             'keypress
             (lambda (c k)
               (cond
                ((and (< k 128) (> k 31))
                 (let ((prev (substring state 0 cursor-at))
                       (rest (substring state cursor-at (+ cursor-at (- (string-length state) cursor-at)))))
                   (set! state (string-append prev (string c) rest)))
                 (++ cursor-at))
                ((eqv? k 259)
                 (let ((prev (substring state 0 (- cursor-at 1)))
                       (rest (substring state cursor-at (+ cursor-at (- (string-length state) cursor-at)))))
                   (set! state (string-append prev rest)))
                 (-- cursor-at))
                ((eqv? k 257) ;; RET (end popup)
                 (start-simulation)
                 (set! *current-mode* nil)
                 (set! *click-can-be-handled* #t)
                 (set! *keypress-can-be-handled* #t)
                 (set! *current-keypress-handler* #f)
                 (set! *current-click-handler* #f)
                 (delete-hook 'frame frame-handler-id)
                 (delete-hook 'keypress key-handler-id)
                 (callback state))
                ((eqv? k 263) ; ←
                 (set! cursor-at (max (- cursor-at 1) 0)))
                ((eqv? k 262) ; →
                 (set! cursor-at (min (+ cursor-at 1) (string-length state))))))))))))

(define (gui/message title text timeout . rect)
  "wyświetla wiadomość"
  (args
   '((title . "tytuł przekazany do gui/window-box")
     (text . "tekst wiadomości")
     (timeout . "czas po którym wiadomość znika")
     (rect . "rect dla wiadomości (nieobowiązkowy)")))

  (let* ((r (if (null? rect) '(10 10 300 100) (car rect)))
         (id (add-hook 'frame
                       (→ (let ((R (gui/window-box r title)))
                            (gui/multiline-text R text))))))
    (wait timeout (→ (delete-hook 'frame id)))))

(define (gui/msg text)
  "wyświetla gui/message"
  (gui/message "" text 5))

;; option-menu
(define *gui/option-menu-force-can-be-handled* #f)

(define gui/option-menu:ident 'GUI-option-menu)
(define gui/option-menu:text-size 16)

(define (gui/option-menu user-pos opts . exit-handler)
  (args
   '((user-pos . "pozycja lewego-górnego punktu opcji w formie `(x . y)`")
     (opts . "opcje w formie ((tekst . funkcja) (tekst . funkcja) ...)")))
  (when (or (and *click-can-be-handled* (eqv? *current-mode* nil))
            *gui/option-menu-force-can-be-handled*)

    (set! *gui/button:skip-unclick* #t)
    (let* ((onexit (if (null? exit-handler)
                       (→ 0)
                       (car exit-handler)))
           (messages (map car opts))
           (measures (map (→1 (measure-text x gui/option-menu:text-size)) messages))
           (full-w (+ (* 2 gui/button:padding) (maxl (map car measures))))
           (avg-h (avg (map cdr measures)))
           (full-h (+ (* avg-h (length opts))
                      (* gui/button:padding (length opts))))
           (full-rect (gui/rect-fit-into-screen (list (car user-pos) (cdr user-pos) full-w full-h)))
           (pos (cons (car full-rect) (cadr full-rect)))
           (rects (map
                   (→1 (list (car pos) (- (+ (cdr pos) x (* x avg-h) (* x gui/button:padding)) x)
                             full-w (+ avg-h gui/button:padding)))
                   (⍳ 0 1 (length opts))))
           (destroy-all
            (→ (for-each (→1 (x)) buttons)
               (delete-hook 'frame cursor-handler-id)
               (delete-hook 'click c-id)

               (set! *gui/option-menu-force-can-be-handled* #f)
               (set-cursor MOUSE-CURSOR-DEFAULT)
               (onexit)))
           (mk-cb (→1 (→ (x) (destroy-all) (onexit)))) ;; WOW
           (cursor-handler-id
            (add-hook
             'frame
             (→ (if (point-in-rect? (get-mouse-position) full-rect)
                    (set-cursor MOUSE-CURSOR-POINTING-HAND)
                    (set-cursor MOUSE-CURSOR-DEFAULT)))))
           (buttons (map
                     (→1 (let ((opt (list-ref opts x)))
                           (gui/button (list-ref rects x) (car opt) (mk-cb (cdr opt)) #t)))
                     (⍳ 0 1 (length opts))))
           (c-id
            (add-hook
             'click
             (→3 (when (and (or (not (point-in-rect? (get-mouse-position) full-rect)) z)
                            (not *gui/button:skip-unclick*))
                   (destroy-all)))))))))


;;; gui/button
;; to troche posrana i skomplikowana sprawa, ale że nie mam innego pomysłu to cusz
;; ~ kpm

(define gui/button:padding 2)
(define gui/button:text-size 16)
(define gui/button:text-spacing *default-spacing*)
(define *gui/button-force-can-be-handled* #f)

(define *gui/button:skip-unclick* #f)

(define (gui/button-textfn rect text-fn cb . ignore-all)
  "tworzy przycisk w `rect` z tekstem zwróconym przez `text-fn`. po przyciśnięciu wykonuje `cb`.
zwraca **destruktor** - funkcję usuwającą go"
  (let ((frame-id
         (add-hook
          'frame
          (→ (fill-rect rect (aq 'background *colorscheme*))
             (gui/rect rect (aq 'frame *colorscheme*))
             (draw-text
              (text-fn)
              (cons
               (+ (list-ref rect 0) gui/button:padding)
               (+ (list-ref rect 1) gui/button:padding))
              gui/button:text-size (aq 'font *colorscheme*) gui/button:text-spacing))))
        (click-id
         (add-hook
          'unclick
          (→3
           (if *gui/button:skip-unclick*
               (set! *gui/button:skip-unclick* #f)
               (when (and y (or *click-can-be-handled* *gui/button-force-can-be-handled* ignore-all)
                          (point-in-rect? (get-mouse-position) rect))
                 (cb)))))))
    (→ (delete-hook 'frame frame-id)
       (delete-hook 'unclick click-id))))

(define (gui/button rect text cb)
  (gui/button-textfn rect (→ text) cb))

(define (gui/btn pos text cb)
  "wykonuje gui/button, tylko sam liczy jak szeroki i wysoki ma być przycisk się zmieścił. zwraca (destruktor szerokosc wysokosc)"
  (args '((pos . "pozycja w formacie (x . y)")
          (text . "tekst w przycisku")
          (cb . "callback")))
  (let* ((measure (measure-text text gui/button:text-size gui/button:text-spacing))
         (w (+ (* 2 gui/button:padding) (car measure)))
         (h (+ (* 2 gui/button:padding) (cdr measure))))
    (list (gui/button `(,(car pos) ,(cdr pos) ,w ,h) text cb)
          w h)))

;;; gui/slider

(define *gui/slider-force-can-be-handled* #f)
(define gui/slider:ident 'GUI-slider)

(define (gui/slider rect from to cb)
  "tworzy slider. wywołuje `cb` z wynikiem za każdym `'unclick` eventem. zwraca destruktor."
  (args
   '((rect  . "w formacie `(x y w h)`")
     (from  . "minimum")
     (to    . "maksimum")
     (cb    . "callback")))

  (let* ((sl-h (/ (list-ref rect 3) 2))
         (sl-w (list-ref rect 2))
         (inner-rect (list (list-ref rect 0)
                           (+ (list-ref rect 1)
                              (/ sl-h 2))
                           sl-w
                           sl-h))
         (slider-rect nil)
         (slider-rect-w 10)
         (maxx (- (+ (car rect) sl-w (/ slider-rect-w 2)) slider-rect-w))
         (minx (- (car rect) (/ slider-rect-w 2)))
         (current-x minx)
         (holding #f)
         (update-slider-rect
          (lambda () (set!
                 slider-rect
                 `(,current-x ,(list-ref rect 1) ,slider-rect-w ,(list-ref rect 3)))))
         (_ (update-slider-rect))
         (frame-id
          (add-hook
           'frame
           (→ (fill-rect inner-rect (aq 'background *colorscheme*))
              (gui/rect inner-rect (aq 'frame *colorscheme*))
              (update-slider-rect)
              (fill-rect slider-rect (aq 'frame *colorscheme*))

              (let ((mp (get-mouse-position))) ;; HACK
                (when *click-can-be-handled*
                  (if (point-in-rect? mp rect)
                      (begin
                        (set! *current-click-handler* gui/slider:ident)
                        (set! *click-can-be-handled* #f))
                      (set! *click-can-be-handled* #t)))))))

         (click-id
          (add-hook
           'click
           (lambda (first l r)
             (let ((mp (get-mouse-position)))
               (when (or (and (or (point-in-rect? mp inner-rect) (point-in-rect? mp slider-rect))
                              (or (or *click-can-be-handled* *gui/slider-force-can-be-handled*)
                                  (eqv? *current-click-handler* gui/slider:ident))
                              l
                              first)
                         holding)
                 (set! holding #t)
                 (set! *click-can-be-handled* #f)
                 (set! current-x (max2 minx (min2 maxx (car mp))))
                 (let* ((v (- current-x minx))
                        (∆range (- to from))
                        (∆ (/ v sl-w))
                        (r (* ∆range ∆)))
                   (cb (+ r from))))))))
         (unclick-id
          (add-hook
           'unclick
           (→3 (when (or (eqv? *current-click-handler* gui/slider:ident) *gui/slider-force-can-be-handled*)
                 (set! holding #f)
                 (set! *current-click-handler* nil)
                 (when (not *gui/slider-force-can-be-handled*)
                   (set! *click-can-be-handled* #t)))))))

    (→ (delete-hook 'frame frame-id)
       (delete-hook 'click click-id)
       (delete-hook 'unclick unclick-id))))

;;; gui/checkbox

(define *gui/checkbox-force-can-be-handled* #f)
(define (gui/checkbox rect cb . state)
  "tworzy checkbox. zwraca destruktor."
  (args
   '((rect . "prostokąt na checkbox")
     (cb . "callback wykonywany po kliknięciu, jako argument przekazuje aktualną wartość (#t | #f)")
     (state . "(opcjonalnie) początkowa wartość (#t | #f)")))

  (define checked (if (null? state) #f (car state)))
  (let* ((padding (/ (list-ref rect 3) 4))
         (checked-rect (list (+ padding (list-ref rect 0))
                             (+ padding (list-ref rect 1))
                             (- (- (list-ref rect 2) 1) (* 2 padding))
                             (- (- (list-ref rect 3) 1) (* 2 padding))))
         (frame-id
          (add-hook
           'frame
           (→ (gui/rect rect (aq 'frame *colorscheme*))
              (when checked
                (fill-rect checked-rect (aq 'frame *colorscheme*))))))

         (unclick-id
          (add-hook
           'unclick
           (lambda (_ l r)
             (when (or *click-can-be-handled* *gui/checkbox-force-can-be-handled*)
               (when (point-in-rect? (get-mouse-position) rect)
                 (set! checked (not checked))
                 (cb checked)))))))

    (→ (delete-hook 'frame frame-id)
       (delete-hook 'unclick unclick-id))))

(define (gui/draw-text-persist . args)
  "zostawia narysowany tekst. zwraca destruktor. ***argumenty jak do `(draw-text)`***"
  (let ((id (add-hook 'frame (→ (apply draw-text args)))))
    (→ (delete-hook 'frame id))))

(define gui/new-source-form:padding 40)
(define gui/new-source-form:pos nil)

(define (gui/new-source-form)
  "form pytający użytkownika o dane nowego źródła"

  (stop-simulation)
  (set! *click-can-be-handled* #f)
  (set! *keypress-can-be-handled* #f)

  (set! *gui/slider-force-can-be-handled* #t)
  (set! *gui/button-force-can-be-handled* #t)
  (set! *gui/checkbox-force-can-be-handled* #t)

  (set! *current-mode* 'new-source)

  (define n-beams 1)
  (define mouse-reactive #f)
  (define angle 0)

  (define default-light (aq 'default-light *colorscheme*))
  (define color-r (list-ref default-light 0))
  (define color-g (list-ref default-light 1))
  (define color-b (list-ref default-light 2))
  (define thickness 1)

  (define color-a 255)

  (let* ((window-box-rect
          (list
           gui/new-source-form:padding
           gui/new-source-form:padding
           (- *SCREEN-WIDTH* (* 2 gui/new-source-form:padding))
           (- *SCREEN-HEIGHT* (* 2 gui/new-source-form:padding))))
         (window-box-data (gui/window-box-retained window-box-rect "nowe źródło"))
         (d-window-box (car window-box-data))
         (rect (cadr window-box-data))
         (d-n-beam-slider (gui/slider
                           (list (+ 10 (car rect))
                                 (+ 10 (cadr rect))
                                 128
                                 32)
                           1 20 (→1 (set! n-beams (round x)))))
         (d-n-beam-label
          (let ((id (add-hook
                    'frame
                    (→
                     (gui/draw-text
                      (string-append "ilość wiązek: " (number->string n-beams))
                      (cons (+ 10 (car rect) 128 10)
                            (+ 10 (cadr rect) (/ (cdr (measure-text "A" 16)) 2)))
                      16 (aq 'font *colorscheme*))))))
            (→ (delete-hook 'frame id))))
         (_1-line-height (+ 10 (cadr rect) 32))

         (d-mouse-r-checkbox (gui/checkbox (list (+ (car rect) 10)
                                                 (+ 10 _1-line-height)
                                                 20 20)
                                           (→1 (set! mouse-reactive x))
                                           mouse-reactive))
         (d-mouse-r-label (gui/draw-text-persist
                           "czy wiązka wskazuje na myszkę?"
                           (cons (+ (car rect) 10 16 10)
                                 (+ _1-line-height (/ (cdr (measure-text "A" 16)) 2)))
                           16 (aq 'font *colorscheme*)))

         (_2-line-height (+ _1-line-height 32))

         (d-angle-slider (gui/slider
                           (list (+ 10 (car rect))
                                 (+ 10 _2-line-height)
                                 128
                                 32)
                           0 360 (→1 (set! angle (round x)))))
         (d-angle-label
          (let ((id (add-hook
                    'frame
                    (→
                     (gui/draw-text
                      (string-append "kąt: " (number->string angle))
                      (cons (+ 10 (car rect) 128 10)
                            (+ 10 _2-line-height (/ (cdr (measure-text "A" 16)) 2)))
                      16 (aq 'font *colorscheme*))))))
            (→ (delete-hook 'frame id))))

         (_3-line-height (+ _2-line-height 32 16))

         (d-col-label
          (gui/draw-text-persist
           "kolor"
           (cons (+ 10 (car rect))
                 (+ 10 _3-line-height ))
           16 (aq 'font *colorscheme*)))


         (d-r-slider (gui/slider (list (+ 10 (car rect))
                                       (+ 10 10 _3-line-height (cdr (measure-text "A" 16)))
                                       128 32)
                                 0 255 (→1 (set! color-r x))))

         (d-g-slider (gui/slider (list (+ 128 16 10 (car rect))
                                       (+ 10 10 _3-line-height (cdr (measure-text "A" 16)))
                                       128 32)
                                 0 255 (→1 (set! color-g x))))

         (d-b-slider (gui/slider (list (+ 256 32 10 (car rect))
                                       (+ 10 10 _3-line-height (cdr (measure-text "A" 16)))
                                       128 32)
                                 0 255 (→1 (set! color-b x))))

         (_4-line-height (+ 32 10 10 _3-line-height (cdr (measure-text "A" 16))))

         (d-color-fill
          (let ((id (add-hook 'frame (→ (fill-rect (list (+ 10 (car rect))
                                                         (+ 10 _4-line-height)
                                                         128 32)
                                                   (list color-r color-g color-b color-a))))))
            (→ (delete-hook 'frame id))))

         (_5-line-height (+ 32 10 _4-line-height 32))

         (d-thickness-slider (gui/slider
                              (list (+ 10 (car rect))
                                    (+ 10 _5-line-height)
                                    128
                                    32)
                              1 10 (→1 (set! thickness (round x)))))
         (d-thickness-label
          (let ((id (add-hook
                    'frame
                    (→ (gui/draw-text
                        (string-append "szerokość wiązki (tylko przy kolorze białym): " (number->string thickness))
                        (cons (+ 10 (car rect) 128 10)
                              (+ 10 _5-line-height (/ (cdr (measure-text "A" 16)) 2)))
                        16 (aq 'font *colorscheme*))))))
            (→ (delete-hook 'frame id))))


         ;; końcowy przycisk "ok"
         (d-OK-btn (car (gui/btn (cons (+ (car rect) 10)
                                       (- *SCREEN-HEIGHT* 80))
                                 "Ok"
                                 (→ (create-source
                                   `((n-beams . ,n-beams)
                                     (reactive . ,mouse-reactive)
                                     (angle . ,angle)
                                     (pos . ,gui/new-source-form:pos)
                                     (thickness . ,(if (all (→1 (eqv? x 255))
                                                            (list color-r color-g color-b))
                                                       thickness
                                                       1))
                                     (color . ,(list color-r
                                                     color-g
                                                     color-b
                                                     color-a))))


                                  ;; cleanup gui
                                  (d-window-box)
                                  (d-n-beam-slider)
                                  (d-n-beam-label)

                                  (d-mouse-r-checkbox)
                                  (d-mouse-r-label)

                                  (d-angle-slider)
                                  (d-angle-label)

                                  (d-col-label)
                                  (d-r-slider)
                                  (d-g-slider)
                                  (d-b-slider)

                                  (d-color-fill)

                                  (d-thickness-label)
                                  (d-thickness-slider)

                                  (d-OK-btn)

                                  ;; cleanup vars
                                  (set! *click-can-be-handled* #t)
                                  (set! *keypress-can-be-handled* #t)
                                  (set! *gui/slider-force-can-be-handled* #f)
                                  (set! *gui/button-force-can-be-handled* #f)
                                  (set! *gui/checkbox-force-can-be-handled* #f)

                                  (set! *current-mode* nil)

                                  (start-simulation)
                                  )))))
    nil))


(define (gui/rect-fit-into-screen rect)
  "zwraca `rect`, który zmieści się na ekranie"
  (let ((x (list-ref rect 0))
        (y (list-ref rect 1))
        (w (list-ref rect 2))
        (h (list-ref rect 3)))
    (list
     (if (> (+ x w) *SCREEN-WIDTH*)
         (- x (abs (- *SCREEN-WIDTH* (+ x w))))
         x)
     (if (> (+ y h) *SCREEN-HEIGHT*)
         (- y (abs (- *SCREEN-HEIGHT* (+ y h))))
         y)
     w h)))

;; (add-hook
;;  'frame
;;  (→ (let* ((mp (get-mouse-position))
;;            (rect (list (car mp) (cdr mp) 150 50))
;;            (fit-rect (gui/rect-fit-into-screen rect)))
;;       (gui/rect rect white)
;;       (gui/rect fit-rect pink))))

(define gui/mp-slider+ok:ident 'mp-slider+ok)
(define (gui/mp-slider+ok from to cb n-after-comma)
  (set! *click-can-be-handled* #f)
  (set! *gui/button-force-can-be-handled* #t)
  (set! *gui/slider-force-can-be-handled* #t)

  (when (eqv? *current-mode* nil)
    (set! *current-mode* gui/mp-slider+ok:ident))

  (when (eqv? *current-mode* 'selected)
    (set! sel-mode:menu-open #t)
    (set! sel-mode:wait-a-sec #t))

  (define V from)
  (let* ((mp (get-mouse-position))
         (start-time (time))
         (after-comma-dummy (apply string (map (→ #\A) (⍳ 0 1 (if (eqv? 0 n-after-comma) 0 (+ n-after-comma 1))))))
         (max-text-size (measure-text
                         (string-append "Ok: " (number->string to) after-comma-dummy)
                         gui/button:text-size))
         (rect (gui/rect-fit-into-screen (list (car mp) (cdr mp) 240 32)))
         (sl-rect (list (list-ref rect 0)
                        (list-ref rect 1)
                        180
                        (list-ref rect 3)))
         (real-cb (→1 (set! V (round-off x n-after-comma))
                      (set! *gui/button:skip-unclick* #t)
                      (cb x))))
    (letrec ((slider-dest (gui/slider sl-rect from to real-cb))
             (btn-dest
              (gui/button-textfn
               (list (+ (car sl-rect) (caddr sl-rect) 16) (+ (cadr sl-rect) 6)
                     (+ (* 2 gui/button:padding) (car max-text-size))
                     (+ (* 2 gui/button:padding) (cdr max-text-size)))
               (→ (string-append "Ok: " (number->string V)))
               (→ (when (> (time) start-time)
                    (set! *click-can-be-handled* #t)
                    (set! *gui/button-force-can-be-handled* #f)
                    (set! *gui/slider-force-can-be-handled* #f)
                    (when (eqv? *current-mode* gui/mp-slider+ok:ident)
                      (set! *current-mode* nil))
                    (when (eqv? *current-mode* 'selected)
                      (set! sel-mode:menu-open #f)
                      (set! sel-mode:wait-a-sec #f))
                    (slider-dest)
                    (btn-dest))))))
      nil)))

(define winopts-names
  '(FLAG-VSYNC-HINT
    FLAG-FULLSCREEN-MODE
    FLAG-WINDOW-RESIZABLE
    FLAG-WINDOW-UNDECORATED
    FLAG-WINDOW-HIDDEN
    FLAG-WINDOW-MINIMIZED
    FLAG-WINDOW-MAXIMIZED
    FLAG-WINDOW-UNFOCUSED
    FLAG-WINDOW-TOPMOST
    FLAG-WINDOW-ALWAYS-RUN
    FLAG-WINDOW-TRANSPARENT
    FLAG-WINDOW-HIGHDPI
    FLAG-WINDOW-MOUSE-PASSTHROUGH
    FLAG-MSAA-4X-HINT
    FLAG-INTERLACED-HINT))

;; lol
;; to jest mój wykręt do nie-pisania faktycznie ważnego kodu
;; może jeszcze to, że nie mam na to teraz psychy
;; mi psycha siada??
;; może
;; ~ kpm
(define (gui/show-window-opts)
  (let* ((cb-dests
          (map
           (→1 (let ((nam (list-ref winopts-names x)))
                 (gui/checkbox
                  (list 10 (+ 20 (* 30 x)) 22 22)
                  (lambda (v) (set-window-flag (eval nam) v))
                  (get-window-flag (eval nam)))))
           (⍳ 0 1 (length winopts-names))))
         (frame-id
          (add-hook
           'frame
           (→ (for-each
               (→1 (gui/draw-text
                    (symbol->string (list-ref winopts-names x))
                    (cons 40 (+ 20 (* 30 x)))
                    18 (aq 'font *colorscheme*)))
               (⍳ 0 1 (length winopts-names)))))))

    (→ (for-each (→1 (x)) cb-dests)
       (delete-hook 'frame frame-id))))

(define *hookable* '(keypress click unclick resize clock log new update delete frame))
(define (gui/show-hook-status)
  (let ((id (add-hook
             'frame
             (→ (for-each
                 (→1 (let* ((sym (list-ref *hookable* x))
                            (n (length (get-all-hooks sym))))
                       (draw-text (string-append (symbol->string sym) " " (number->string n))
                                  (cons 16 (- *SCREEN-HEIGHT* 64 (+ 10 (* x 20))))
                                  16
                                  white)))
                 (⍳ 0 1 (length *hookable*)))))))
    (→ (delete-hook 'frame id))))

(define (gui/show-fps pos)
  (let* ((cur-fps 0)
         (fps 0)
         (frame-id
          (add-hook 'frame (→ (draw-text
                               (string-append "fps: " (number->string cur-fps))
                               pos 21 (aq 'font *colorscheme*))
                              (++ fps))))
         (clock-id
          (add-hook 'clock (→ (set! cur-fps fps)
                              (set! fps 0)))))
    (→ (delete-hook 'frame frame-id)
       (delete-hook 'clock clock-id))))

(define (gui/save-current)
  (gui/input-popup "podaj nazwę pliku" (→1 (serialize:save-to x)) #t))

(define (gui/load-example-menu)
  (let ((opts (map
               (→1 (let ((e (list-ref *examples* x)))
                     (cons (string-append (number->string (+ 1 x)) ". " (car e))
                           (cdr e))))
               (⍳ 0 1 (length *examples*)))))
    (gui/option-menu (get-mouse-position) opts)))

(define gui/change-source-color-form:ident 'change-source-color)
(define (gui/change-source-color-form pos cb)
  (set! *click-can-be-handled* #f)
  (set! *gui/slider-force-can-be-handled* #t)
  (set! *gui/button-force-can-be-handled* #t)

  (define r 255)
  (define g 255)
  (define b 255)

  (when (eqv? *current-mode* nil)
    (set! *current-mode* gui/change-source-color-form:ident))

  (when (eqv? *current-mode* 'selected)
    (set! sel-mode:menu-open #t)
    (set! sel-mode:wait-a-sec #t))


  (let* ((w 128)
         (el-h 32)
         (user-rect (list (car pos) (cdr pos) w (* 4 el-h)))
         (rect (gui/rect-fit-into-screen user-rect))
         (call-cb (→ (cb (list r g b))))
         (_ (call-cb))
         (d-r-slider (gui/slider (list (car rect) (cadr rect) w el-h) 0 255 (→1 (set! r x) (call-cb))))
         (d-g-slider (gui/slider (list (car rect) (+ (cadr rect) el-h) w el-h) 0 255 (→1 (set! g x) (call-cb))))
         (d-b-slider (gui/slider (list (car rect) (+ (cadr rect) el-h el-h) w el-h) 0 255 (→1 (set! b x) (call-cb))))
         (d-ok-btn (car (gui/btn (cons (car rect) (+ (cadr rect) (* 3 el-h))) "ok"
                                 (→ (when (eqv? *current-mode* gui/change-source-color-form:ident)
                                      (set! *current-mode* nil))
                                    (when (eqv? *current-mode* 'selected)
                                      (set! sel-mode:wait-a-sec #f)
                                      (set! sel-mode:menu-open #f))

                                    (set! *click-can-be-handled* #t)
                                    (set! *gui/slider-force-can-be-handled* #f)
                                    (set! *gui/button-force-can-be-handled* #f)
                                    (d-r-slider)
                                    (d-g-slider)
                                    (d-b-slider)
                                    (d-ok-btn)
                                    (call-cb))))))))
