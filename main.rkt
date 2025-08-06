#lang racket

(require racket/gui)

(provide columns-pane%)

(define columns-pane%
  (class vertical-pane%
    (super-new)

    ;; Internal data
    (define hpanes #f)
    (define creating-row #f)
    (define current-col 0)
    (define current-row 0)

    (define/private (mk-hpane*)
      (set! creating-row #t)
      (let ((new-pane (new horizontal-pane% [parent this]
                           [spacing spacing] [horiz-margin horiz-margin] [vert-margin (round (/ spacing 2))]
                           [stretchable-height #f])))
        (letrec ((adder (lambda (col)
                          (when (< col columns)
                            (new horizontal-pane% [parent new-pane] [stretchable-width #t]
                                 [alignment (list (column-alignment* col) 'center)]))
                          (adder (+ col 1)))))
          (adder 0))
        (set! creating-row #f)
        new-pane)
      )

    (define/private (column-alignment* c)
      (vector-ref column-aligns c))
    
    (define/private (get-row* i)
      (when (eq? hpanes #f)
        (set! hpanes (make-vector (+ i 1) #f)))
      (when (>= i (vector-length hpanes))
        (let ((v hpanes))
          (set! hpanes (make-vector (+ i 1) #f))
          (vector-copy! hpanes 0 v)))
      (let ((p (vector-ref hpanes i)))
        (when (eq? p #f)
          (set! p (mk-hpane*))
          (vector-set! hpanes i p))
        p))

    (define/private (cell* c r)
      (let* ((row (get-row* r))
             (cells (send row get-children)))
        (list-ref cells c)))
    
    (define/private (child* c r)
      (let* ((cell (cell* c r))
             (children (send cell get-children))
             (child (if (null? children) #f (car children)))
             )
        child))
        
    (define/private (max-width* c r w)
      (if (< r (vector-length hpanes))
          (let* ((child (child* c r))
                 (child-width (if (eq? child #f) 0 (send child get-width))))
            (max-width* c (+ r 1) (if (> child-width w) child-width w)))
          w))

    (define/private (set-min-width* c r w)
      (when (< r (vector-length hpanes))
        (let ((child (child* c r)))
          (when (not (eq? child #f))
            (send child min-width w))
          (set-min-width* c (+ r 1) w))))
    
    (define/private (arrange-col* c)
      (let* ((w (max-width* c 0 -1))
             (col-min-width (vector-ref column-min-widths c))
             (w* (if (> col-min-width w) col-min-width w))
             )
        (set-min-width* c 0 w*)))
        
    (define/private (arrange*)
      (letrec ((cols (length (send (vector-ref hpanes 0) get-children)))
               (f (lambda (c)
                    (when (< c cols)
                      (arrange-col* c)
                      (f (+ c 1)))))
               )
        (f 0)))

    ;; Fields that can be given
    (init-field [vert-margin 5] [horiz-margin 5] [spacing 5] [columns 1])

    ;; Internal data
    (define column-min-widths (make-vector columns 0))
    (define column-aligns (make-vector columns 'left))

    ;; Public methods
    (define/public (column-min-width c . w)
      (unless (null? w)
        (vector-set! column-min-widths c (car w)))
      (vector-ref column-min-widths c))

    (define/public (column-align c . a)
      (unless (null? a)
        (vector-set! column-aligns c (car a)))
      (vector-reff column-aligns c))

    ;; Overridden methods
    (define/override (after-new-child child)
      (super after-new-child child)
      (when (eq? creating-row #f)
        (let ((cell (get-cell* current-col current-row)))
          (send child reparent cell))
        (set! current-col (+ current-col 1))
        (when (>= current-col columns)
          (set! current-row (+ current-row 1))
          (set! current-col 0))
        ))

    (define/override (place-children info width height)
      (let ((r (super place-children info width height)))
        (arrange*)
        r
        ))
    
    ))


;;; GUI Testing

;(define win (new frame% [label "Hi there!"]))
;(define grid-group (new group-box-panel% [label "My group"] [parent win] [horiz-margin 10] [vert-margin 10]))

;(define g (new columns-pane% [parent grid-group] [columns 3]))
;(send g column-min-width 1 500)

;(define btn1 (new button% [parent g] [label "Button 1"]))
;(define g1 (new gauge%  [parent g] [stretchable-width #t] [label "gauge 1"] [range 100]))
;(define lbl1 (new message% [parent g] [label "This is lbl 1"]))

;(define btn2 (new button% [parent g] [label "Btn 2"]))
;(define lbl2 (new message% [parent g] [label "This is lbl 2"]))

;(send win show #t)

