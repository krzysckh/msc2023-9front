;;---- cdocs.scm
;; tu jest dokumentacja i sygnatury funkcji zdefiniowanych w src/scheme-interop.c

(document-function
 (time-since-init)
 "zwraca ile czasu minęło od początku działania programu (wg. raylib - od `InitWindow()`)")

(document-function
 (time)
 "zwraca aktualny unix timestamp")

(document-function
 (system s)
 "wykonuje `sh -c $s` i zwraca stdout")

(document-function
 (exit . status)
 "kończy program. zwraca `status` jeśli podany, inaczej 0")

(document-function
 (loads s)
 "wykonuje `s` (to samo co eval, tylko że nie zwraca wartości i akceptuje string, nie sexp)")

(document-function
 (delete-hook sym n)
 "usuwa hook dla `sym` o id `n`"
 (args
  '((sym . "`hookable_event_t` via src/scheme-interop.c")
    (n . "id zwrócone przez `add-hook`"))))

(document-function
 (get-source n)
 "zwraca informacje o źródle n")

(document-function
 (get-all-sources)
 "zwraca listę wszystkich źródeł")

(document-function
 (create-mirror x1 y1 x2 y2)
 "tworzy zwierciadło")

(document-function
 (get-mouse-position)
 "zwraca pozycje myszki na oknie w postaci `(x . y)`")

(document-function
 (get-screen-size)
 "zwraca wielkość okna `(w . h)`")

(document-function
 (get-winconf)
 "zwraca obecny winconf w postaci jak argumenty do `set-winconf`")

(document-function
 (set-winconf bgcolor mirror-color)
 "ustawia winconf"
 (args
  '((bgcolor . "kolor tła w formacie `(r g b a)` *(można pominąć `a`)*")
    (mirror-color . "kolor zwierciadła w formacie j.w."))))

(document-function
 (real-tracelog T s)
 "wykonuje TraceLog(T, s)")

(document-function
 (real-fill-rect x y w h color)
 "lepiej uzywać `fill-rect`")

(document-function
 (set-window-flag flag v)
 "ustawia flagę raylib"
 (args
  '((flag . "flaga zdefiniowana w `interop-helpers.scm` jako `FLAG-*`")
    (v . "`#t | #f`"))))

(document-function
 (get-window-flag flag)
 "getter dla flagi raylib"
 (args
  '((flag . "flaga zdefiniowana w `interop-helpers.scm` jako `FLAG-*`"))))

(document-function
 (rect-collision r1 r2)
 "zwraca wspólny prostokąt dla r1 i r2. w razie braku, zwraca `(0 0 0 0)`")

(document-function
 (get-bounceable id)
 "zwraca dane dla `bounceable_t` od id `id`")

(document-function
 (get-all-bounceables) 0)

(document-function
 (set-mirror! id pt1 pt2)
 "zmienia dane zwierciadła o id `id`")

(document-function
 (real-register-custom pts f1 f2)
 "patrz: register-custom")

(document-function
 (create-prism pt vert-len n)
 "tworzy pryzmat ze środkiem `pt`, długością boku `vert-len` i magicznym numerkiem `n` (wyleciało mi teraz z głowy jak to się nazywa lol)")

(document-function
 (normalize-rectangle rect)
 "zwraca *unormalniony* prostokąt"
 (example
  '((normalize-rectangle '(10 10 -10 -10)) '(0 0 10 10))))

(document-function
 (point-in-line? pt lp1 lp2 thr)
 "robi raylibowe CheckCollisionPointLine(pt, lp1, lp2, thr)")

(document-function
 (angle-between p1 p2)
 "zwraca kąt pomiędzy `p1` a `p2` (w stopniach)")

(document-function
 (normalize-angle ang)
 "zwraca *unormalniony* kąt"
 (example
  '((normalize-angle 380) 20)))

(document-function
 (vec-move-towards vec target maxlen)
 "Vector2MoveTowards(vec, target, maxlen)")

(document-function
 (real-delete-all-sources)
 nil)

(document-function
 (create-lens center d r)
 nil)

(document-function
 (delete-bounceable id)
 nil)

(document-function
 (set-lens! id center d r)
 nil)

(document-function
 (point-in-lens? pt lens-id)
 nil)

(document-function
 (white? color)
 "sprawdza czy kolor jest rozumiany za biały")

(document-function
 (getenv s)
 "`man 3 getenv`")
