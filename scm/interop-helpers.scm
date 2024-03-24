(define *sources* '())

; TODO: 'resize hook
(define *SCREEN-SIZE* (get-screen-size))
(define *SCREEN-WIDTH* (car *SCREEN-SIZE*))
(define *SCREEN-HEIGHT* (cdr *SCREEN-SIZE*))

(define *default-spacing* 0)

;; prosto z raylib.h
;; (4.5)
(define FLAG-VSYNC-HINT #x00000040)
(define FLAG-FULLSCREEN-MODE #x00000002)
(define FLAG-WINDOW-RESIZABLE #x00000004)
(define FLAG-WINDOW-UNDECORATED #x00000008)
(define FLAG-WINDOW-HIDDEN #x00000080)
(define FLAG-WINDOW-MINIMIZED #x00000200)
(define FLAG-WINDOW-MAXIMIZED #x00000400)
(define FLAG-WINDOW-UNFOCUSED #x00000800)
(define FLAG-WINDOW-TOPMOST #x00001000)
(define FLAG-WINDOW-ALWAYS-RUN #x00000100)
(define FLAG-WINDOW-TRANSPARENT #x00000010)
(define FLAG-WINDOW-HIGHDPI #x00002000)
(define FLAG-WINDOW-MOUSE-PASSTHROUGH #x00004000)
(define FLAG-MSAA-4X-HINT #x00000020)
(define FLAG-INTERLACED-HINT #x00010000)

(define MOUSE-CURSOR-DEFAULT       0) ; default pointer shape
(define MOUSE-CURSOR-ARROW         1) ; arrow shape
(define MOUSE-CURSOR-IBEAM         2) ; text writing cursor shape
(define MOUSE-CURSOR-CROSSHAIR     3) ; cross shape
(define MOUSE-CURSOR-POINTING-HAND 4) ; pointing hand cursor
(define MOUSE-CURSOR-RESIZE-EW     5) ; horizontal resize/move arrow shape
(define MOUSE-CURSOR-RESIZE-NS     6) ; vertical resize/move arrow shape
(define MOUSE-CURSOR-RESIZE-NWSE   7) ; top-left to bottom-right diagonal resize/move arrow shape
(define MOUSE-CURSOR-RESIZE-NESW   8) ; the top-right to bottom-left diagonal resize/move arrow shape
(define MOUSE-CURSOR-RESIZE-ALL    9) ; the omnidirectional resize/move cursor shape
(define MOUSE-CURSOR-NOT-ALLOWED   10); the operation-not-allowed shape

(define (aq e alist)
  "zwraca wynik assq bez car"
  (args
   '((e . "element szukany")
     (alist . "lista asocjasyjna")))
  (cdr (assq e alist)))

(define (cdr* l)
  "zwraca cdr dla l jeśli l to lista lub para"
  (if (or (pair? l) (list? l))
    (cdr l)
    l))

(define (aq-or e alist o)
  "zwraca wynik (assq e alist) jeśli e istnieje w alist. w przeciwnym wypadku o"
  (let ((v (assq e alist)))
    (if v
      (cdr v)
      o)))

(define (update-sources)
  "wewnętrzna funkcja aktualizująca *sources* za każdym razem gdy zostaną
  zmienione"
  (set! *sources* (get-all-sources)))

(define (create-source a)
  "tworzy nowe source_t (źródło światła)"

  (args
   '((a . "lista asocjacyjna z elementami 'x, 'y, 'size, 'angle, 'thickness,
           'reactive, 'color, 'n-beam.
           wszystkie elementy mają wartości domyślne i mogą być pominięte.
           zamiast 'x i 'y, może zostać zdefiniowane samo 'pos")))
  (example
   '((create-source '((x . 500) (y . 500) (reactive . #t)))
     "tworzy reagujące na myszkę źródło na pozycji (500 500)"))

  (let* ((pos (aq-or 'pos a `(,(aq-or 'x a 100) . ,(aq-or 'y a 100))))
         (x (car pos))
         (y (cdr pos))
         (size (aq-or 'size a 20))
         (ang (aq-or 'angle a 90))
         (thickness (aq-or 'thickness a 1))
         (reactive (aq-or 'reactive a #t))
         (n-beams (aq-or 'n-beams a 1))
         (color (aq-or 'color a (aq 'default-light *colorscheme*)))
         (id (real-create-source x y size ang thickness reactive n-beams color)))
    (update-sources)
    (cons 'source id)))

(define (draw-line pt1 pt2 thick color)
  "rysuje linię od pt1 do pt2 o grubości thick i kolorze color"
  (args
   '((pt1 . "punkt 1 (x . y)")
     (pt2 . "punkt 2 (x . y)")
     (thick . "grubość")
     (color . "kolor (r g b a)")))

  (let ((x1 (car pt1))
        (y1 (cdr pt1))
        (x2 (car pt2))
        (y2 (cdr pt2)))
    (real-draw-line x1 y1 x2 y2 thick color)))

(define (set-source! n x y ang thickness mouse-reactive n-beams color)
  "aktualizuje źródło o id n ustawiając wszystkie jego wartości. lepiej używać set-source-e!"
  (real-set-source! n x y ang thickness mouse-reactive n-beams color)
  (update-sources))

(define (set-source-e! n sym v)
  "aktualizuje właściwość sym na v w źródle o id n"
  (args
   '((n . "id źródła")
     (sym . "'pos | 'angle | 'thickness | 'color | 'mouse-reactive | 'n-beams w zależności od tego co chcemy zmienić")
     (v . "nowa wartość dla sym")))

  (when (eqv? sym 'thickness)
    (print "setting thickness to" v))

  (if (> n (length *sources*))
    #f
    (let* ((src (get-source n))
           (x (if (eq? 'pos sym) (car v) (car (list-ref src 0))))
           (y (if (eq? 'pos sym) (cdr v) (cdr (list-ref src 0))))
           (ang (if (eq? 'angle sym) v (list-ref src 1)))
           (thickness (if (eq? 'thickness sym) v (list-ref src 2)))
           (mouse-reactive (if (eq? 'mouse-reactive sym) v (list-ref src 3)))
           (n-beams (if (eq? 'n-beams sym) v (list-ref src 4)))
           (color (if (eq? 'color sym) v (list-ref src 5))))
      (set-source! n x y ang thickness mouse-reactive n-beams color)))
  (update-sources))

(define (set-prism-e! id t v)
  (args
   '((t . "`'pt` | `'vert-len` | `'n`")
     (v . "wartość dla `t`")))

  (let* ((prism (cdr (assv id *prisms*)))
         (pt (if (eqv? t 'pt) v (list-ref prism 0)))
         (vert-len (if (eqv? t 'vert-len) v (list-ref prism 4)))
         (n (if (eqv? t 'n) v (list-ref prism 5))))
    (set-prism! id pt vert-len n)))

(define (set-lens-e! id t v)
  (args
   '((t  . "`r | center | d`")))
  (let* ((lens (get-bounceable id))
         (r (if (eqv? t 'r) v (list-ref lens 3)))
         (center (if (eqv? t 'center) v (list-ref lens 4)))
         (d (if (eqv? t 'd) v (list-ref lens 5))))
    (set-lens! id center d r)))

; (measure-text text size . spacing) → (w . h)
(define (measure-text text size . spacing)
  "zwraca (w . h) tekstu text o wielkości size i spacingu spacing (jeśli podany)"
  (let ((spacing (if (null? spacing) *default-spacing* (car spacing))))
    (real-measure-text text size spacing)))

; (real-draw-text text x y sz spacing color) → #t
(define (draw-text text pos sz color . spacing)
  "wypisuje tekst text domyślnym fontem na pozycji pos, o wielkości sz i kolorze
  color. można też podać spacing."
  (args '((color . "w postaci (r g b a)")))
  (let ((x (car pos))
        (y (cdr pos))
        (spc (if (null? spacing) *default-spacing* (car spacing))))
    (real-draw-text text x y sz spc color)))

(define log-trace 1)
(define log-debug 2)
(define log-info 3)
(define log-warning 4)
(define log-error 5)
(define log-fatal 6)

(define (tracelog T . vs)
  "robi TraceLog z typem T i tekstem vs"
  (args
   '((T . "typ logu (moze być `'trace | 'debug | 'info | 'warning | 'error | 'fatal`)")
     (vs . "tekst")))

  (let ((type (if (number? T) T
                  (cond
                   ((eqv? T 'trace)   log-trace)
                   ((eqv? T 'debug)   log-debug)
                   ((eqv? T 'info)    log-info)
                   ((eqv? T 'warning) log-warning)
                   ((eqv? T 'error)   log-error)
                   ((eqv? T 'fatal)   log-fatal)))))
    (real-tracelog type (->string vs))))

(define add-system-hook real-add-hook)

;; to jest takie idiotyczne XDDD
;; GC Tinyscheme usuwa lambdy przekazywane do FF funkcji (tych zdefiniowanych w c)
;; dlatego żeby cały czas o nich "pamiętało" dodaje je do *all-hooks* :33333
;; ~ kpm
(define *all-hooks* nil)

;; tak samo z funkcjami przekazywanymi do register-custom
;; ~ kpm

(define *all-custom-fns* nil)
(define (register-custom poly-points draw-function light-remap-function)
  "tworzy nowy obiekt w obrębie `poly-points` rysowany co klatkę przez `draw-function`, jeśli wiązka światła napotka obiekt, przemieniana jest wg. `light-remap-function`. więcej doc TBD"
  (set! *all-custom-fns* (append *all-custom-fns* (list (cons draw-function light-remap-function))))
  (let ((l (last *all-custom-fns*)))
    (apply real-register-custom (list poly-points (car l) (cdr l)))))

;; TODO: zrobić tak żeby nie trzeba było sprawdzać *can-click-be-handled* etc.
;; ~ kpm
(define (add-user-hook s f)
  "w przyszłości będzie dodawała hooki które mogą być blokowane przez systemowe"
  (set! *all-hooks* (append *all-hooks* (list f)))
  (real-add-hook s (last *all-hooks*)))

(define add-hook add-user-hook)

;; TODO: DRY
;; ~ kpm
(define __state_running 0)
(define __state_stopped 1)

(define (stop-simulation)
  "zatrzymuje wszystko (przestaje renderować)"
  (let* ((conf (get-winconf))
         (next-conf (map
                     (→1 (if (eqv? x 2) __state_stopped (list-ref conf x)))
                     (⍳ 0 1 (length conf)))))
    (apply set-winconf next-conf)))

(define (start-simulation)
  "odpala z powrotem wszystko (zaczyna renderować)"
  (let* ((conf (get-winconf))
         (next-conf (map
                     (→1 (if (eqv? x 2) __state_running (list-ref conf x)))
                     (⍳ 0 1 (length conf)))))
    (apply set-winconf next-conf)))

; (real-fill-rect x y w h color)
(define (fill-rect rect color)
  (let ((x (car rect))
        (y (cadr rect))
        (w (caddr rect))
        (h (cadddr rect)))
    (real-fill-rect x y w h color)))

(define (experimental/toggle-resizable)
  (set-window-flag FLAG-WINDOW-RESIZABLE (not (get-window-flag FLAG-WINDOW-RESIZABLE))))

(define (add-mirror p1 p2)
  "wywołuje `create-mirror`, tylko, że argumenty to punkty w parach"
  (args
   '((p1 . "'(x . y)")
     (p2 . "'(x . y)")))
  (create-mirror (car p1) (cdr p1) (car p2) (cdr p2)))

(define (delete-all-sources)
  (real-delete-all-sources)
  (update-sources))

;; bardzo nieefektywne, ale no cóż
;; patrz: rant w src/scheme-interop.c
(define (delete-source id)
  (let* ((sources-kept (map (→1 (list-ref *sources* x))
                            (filter (→1 (not (eqv? id x)))
                                    (⍳ 0 1 (length *sources*)))))
         (sources-sexps (map serialize:source->sexp sources-kept)))
    (delete-all-sources)
    (for-each eval sources-sexps)))

(define (delete-sources lst)
  (let* ((sources-kept (map (→1 (list-ref *sources* x))
                            (filter (→1 (not (memv x lst)))
                                    (⍳ 0 1 (length *sources*)))))
         (sources-sexps (map serialize:source->sexp sources-kept)))
    (delete-all-sources)
    (for-each eval sources-sexps)))
