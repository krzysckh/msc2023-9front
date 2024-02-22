;;; selection-mode
(define sel-mode:start-position nil)
(define sel-mode:mirror-rects nil)
(define sel-mode:prism-rects nil)
(define sel-mode:lens-rects nil)

(define sel-mode:wait-a-sec #f)
(define sel-mode:menu-open #f)
(define sel-mode:should-end-selected-mode #f)

(define sel-mode:source-rects nil)

(define sel-mode:last-time 0)
(define sel-mode:selected-bounceable-ids nil)
(define sel-mode:selected-source-ids nil)

(define (sel-mode:highlight-rects rects sel-map)
  (for-each
   (→1 (gui/rect
        (list-ref rects x)
        (if (list-ref sel-map x)
            (aq 'green *colorscheme*)
            (aq 'red *colorscheme*))))
   (⍳ 0 1 (length rects))))

(define sel-mode:default-menu
  `(("usuń" . ,(→ (set! sel-mode:menu-open #f)
                  (set! sel-mode:should-end-selected-mode #t)
                  (delete-sources sel-mode:selected-source-ids)
                  (for-each delete-bounceable sel-mode:selected-bounceable-ids)))
    ("kopiuj" . ,(→ (set! sel-mode:menu-open #f)
                    (set!
                     *clipboard*
                     (append
                      (map (→1 (serialize:bounceable->sexp (get-bounceable x)))
                           sel-mode:selected-bounceable-ids)
                      (map (→1 (serialize:source->sexp (list-ref *sources* x)))
                           sel-mode:selected-source-ids)))))))

(define (sel-mode:get-menu)
  (cond
   ((> (length sel-mode:selected-source-ids) 0)
    (if (eqv? (length sel-mode:selected-bounceable-ids) 0)
        (option-menu-for 'source sel-mode:selected-source-ids)
        sel-mode:default-menu))
   ((eqv? (id->Btype (car sel-mode:selected-bounceable-ids)) 'mirror)
    sel-mode:default-menu)
   ((all-same? (map id->Btype sel-mode:selected-bounceable-ids))
    (option-menu-for (id->Btype (car sel-mode:selected-bounceable-ids)) sel-mode:selected-bounceable-ids))
   (else sel-mode:default-menu)))

(add-hook
 'click
 (lambda (first l r)
   (when (and l (> (time) sel-mode:last-time))
     (when (and (not first) *click-can-be-handled*)
       (set! first #t))
     (when (or (and *click-can-be-handled* (eqv? *current-mode* nil)) (eqv? *current-mode* 'selection))
       (if first
           (begin
             (set! *click-can-be-handled* #f)
             (set! sel-mode:start-position (get-mouse-position))
             (set! *current-mode* 'selection)
             (set! sel-mode:source-rects (map src->rect (map car *sources*)))

             (set! sel-mode:lens-rects (map lens->rect *lenss*))
             (set! sel-mode:prism-rects (map (→1 (apply triangle->rect x)) (map prism->ptlist *prisms*)))
             (set! sel-mode:mirror-rects (map (→1 (apply pts->rect x)) (map cdr *mirrors*))))
           (begin
             (let* ((mp (get-mouse-position))
                    (rect (list
                           (car sel-mode:start-position)
                           (cdr sel-mode:start-position)
                           (- (car mp) (car sel-mode:start-position))
                           (- (cdr mp) (cdr sel-mode:start-position))))
                    (selected-source-map (map (→1 (rect-collision? rect x)) sel-mode:source-rects))

                    (selected-lens-map   (map (→1 (rect-collision? rect x)) sel-mode:lens-rects))
                    (selected-mirror-map (map (→1 (rect-collision? rect x)) sel-mode:mirror-rects))
                    (selected-prism-map  (map (→1 (rect-collision? rect x)) sel-mode:prism-rects)))
               (sel-mode:highlight-rects sel-mode:mirror-rects selected-mirror-map)
               (sel-mode:highlight-rects sel-mode:prism-rects selected-prism-map)
               (sel-mode:highlight-rects sel-mode:source-rects selected-source-map)
               (sel-mode:highlight-rects sel-mode:lens-rects selected-lens-map)

               (gui/rect rect (aq 'selection *colorscheme*)))))))))

(add-hook
 'unclick
 (→3 (when (and y (eqv? *current-mode* 'selection))
       (let* ((mp (get-mouse-position))
              (sel-rect (list
                         (car sel-mode:start-position)
                         (cdr sel-mode:start-position)
                         (- (car mp) (car sel-mode:start-position))
                         (- (cdr mp) (cdr sel-mode:start-position))))
              (mirror-ids (map car (filter (→1 (rect-collision?
                                                (pts->rect (cadr x) (caddr x))
                                                sel-rect))
                                           *mirrors*)))
              (prism-ids (map car (filter (→1 (rect-collision?
                                               (apply triangle->rect (prism->ptlist x))
                                               sel-rect))
                                          *prisms*)))
              (lens-ids (map car (filter (→1 (rect-collision? (lens->rect x) sel-rect))
                                         *lenss*)))
              (source-ids (filter (→1 (rect-collision? sel-rect (src->rect (car (list-ref *sources* x))))) (⍳ 0 1 (length *sources*)))))
         (set! sel-mode:selected-source-ids source-ids)
         (set! sel-mode:selected-bounceable-ids (append mirror-ids prism-ids lens-ids))
         (start-selected-mode)))))

;; TODO: sel custom
;; stary sposób był (chyba) trochę szybszy chociaż pewności nie mam.
;; teraz po prostu co klatkę, jeśli coś się zmieniło robię set-rzecz! bez zatrzymywania symulacji
(define (start-selected-mode)
  (set! *current-mode* 'selected)
  (if (> (+ (length sel-mode:selected-bounceable-ids)
            (length sel-mode:selected-source-ids))
            0)
      (let* ((mp (get-mouse-position))
             (sel-mode:menu-open #f)
             (rects (map normalize-rectangle (append (map thing->rect (map get-bounceable sel-mode:selected-bounceable-ids))
                                                     (map (→1 (src->rect (car (list-ref *sources* x)))) sel-mode:selected-source-ids))))
             (minx (minl (map car rects)))
             (miny (minl (map cadr rects)))
             (maxx (maxl (map (→1 (+ (car x) (caddr x))) rects)))
             (maxy (maxl (map (→1 (+ (cadr x) (cadddr x))) rects)))
             (∆x 0)
             (∆y 0)
             (∆mouse nil)
             (bounding-rect nil)
             (update-bounding-rect
              (→ (set! bounding-rect (list (+ ∆x minx)
                                           (+ ∆y miny)
                                           (- maxx minx)
                                           (- maxy miny)))))
             (_ (update-bounding-rect))
             (b-rect-id (add-hook 'frame (→ (gui/rect bounding-rect (aq 'red *colorscheme*)))))
             (cursor-handler-id
              (add-hook
               'frame
               (→ (when (and (not sel-mode:menu-open) (not sel-mode:wait-a-sec))
                    (if (point-in-rect? (get-mouse-position) bounding-rect)
                        (set-cursor MOUSE-CURSOR-RESIZE-ALL)
                        (set-cursor MOUSE-CURSOR-ARROW))))))
             (external-close-handler-id
              (add-hook
               'frame
               (→ (when sel-mode:should-end-selected-mode
                    (end-selected-mode)))))
             (move-handler-id
              (add-hook
               'click
               (lambda (first l r)
                 (let ((mp (get-mouse-position)))
                   (when (and (not sel-mode:menu-open) (not sel-mode:wait-a-sec))
                     (when l
                       (when first
                         (set! ∆mouse (cons (- (car mp) minx ∆x)
                                            (- (cdr mp) miny ∆y))))
                       (let ((∆last (cons ∆x ∆y)))
                         (set! ∆x (- (car mp) minx (car ∆mouse)))
                         (set! ∆y (- (cdr mp) miny (cdr ∆mouse)))
                         (for-each
                          (→1 (reposition-source-by-delta x (cons (- ∆x (car ∆last))
                                                                  (- ∆y (cdr ∆last)))))
                          sel-mode:selected-source-ids)
                         (for-each
                          (→1 (reposition-bounceable-by-delta x (cons (- ∆x (car ∆last))
                                                                      (- ∆y (cdr ∆last)))))
                          sel-mode:selected-bounceable-ids))
                       (update-bounding-rect)))))))
             (menu-handler-id
              (add-hook
               'click
               (lambda (first l r)
                 (when (and first r (not sel-mode:menu-open) (not sel-mode:wait-a-sec))
                   (set! sel-mode:menu-open #t)
                   (set! *gui/option-menu-force-can-be-handled* #t)
                   (gui/option-menu
                    (get-mouse-position)
                    (sel-mode:get-menu)
                    (→ (set! sel-mode:menu-open #f)))))))
             (end-selected-mode
              (→ (delete-hook 'frame b-rect-id)
                 (delete-hook 'frame external-close-handler-id)
                 (delete-hook 'frame cursor-handler-id)
                 (delete-hook 'click move-handler-id)
                 (delete-hook 'click menu-handler-id)
                 (delete-hook 'click close-handler-id)
                 (really-end-selected-mode)))
             (close-handler-id
              (add-hook
               'click
               (lambda (first l r)
                 (when (and (not sel-mode:menu-open) (not sel-mode:wait-a-sec))
                   (when (and first l (not (point-in-rect? (get-mouse-position) bounding-rect)))
                     (end-selected-mode))))))))
      (really-end-selected-mode)))

(define (really-end-selected-mode)
  (set! sel-mode:menu-open #f)
  (set! sel-mode:should-end-selected-mode #f)
  (set! sel-mode:wait-a-sec #f)
  (set! *gui/option-menu-force-can-be-handled* #f)
  (set! sel-mode:last-time
        (if (> (+ (length sel-mode:selected-bounceable-ids) (length sel-mode:selected-source-ids)) 0)
            (time)
            0))
  (set! *click-can-be-handled* #t)
  (set! *current-mode* nil))
