#lang racket

(require
  2htdp/batch-io
  2htdp/image
  2htdp/universe
  unstable/custom-write)


;                                           ;                          ;           
;                                           ;                          ;           
;                                           ;                          ;           
;                                           ;;                         ;;          
;                                           ;;;                        ;;;         
;                       ;                 ;;;;             ;         ;;;;          
;     ;;;;       ;;;;   ;   ;;     ;;;;     ;;      ;;;    ;   ;;      ;;     ;;;; 
;    ;   ;;    ;;    ;  ;   ; ;   ;;        ;;    ;;   ;   ;   ; ;     ;;    ;;    
;    ;    ;   ;;     ;  ;  ;  ;  ;          ;;         ;   ;  ;  ;     ;;   ;      
;   ;        ;;      ;  ; ;   ;  ;           ;         ;   ; ;   ;      ;   ;      
;   ;        ;       ;   ;;   ;   ;;         ;      ;; ;    ;;   ;      ;    ;;    
;   ;        ;       ;   ;    ;     ;;       ;    ;;  ;;    ;    ;      ;      ;;  
;   ;     ;  ;      ;    ;    ;       ;      ;    ;   ;;    ;    ;      ;        ; 
;    ;   ;   ;      ;    ;    ;       ;      ;    ;  ; ;    ;    ;      ;        ; 
;     ;;;     ;    ;     ;    ;  ;;  ;;      ;    ;;;  ;    ;    ;      ;   ;;  ;; 
;              ;;;;               ;;;;                                       ;;;;  
;
(define WIDTH 800)
(define HEIGHT WIDTH)
(define SQUARE-DIM (/ WIDTH 8))
(define OFFSET (/ SQUARE-DIM 2))
(define WHITE-SQUARE-COLOR (make-color 240 255 240))
(define WHITE-PIECE-COLOR (make-color 245 245 220))
(define BLACK-SQUARE-COLOR (make-color 34 139 34))
(define BLACK-PIECE-COLOR "black")
(define PIECE-SIZE (* SQUARE-DIM (/ 3 4)))
(define turn "white")
(define turn-count 0)

(define change-turn
  (λ ()
    (if (eqv? turn "white")
        (set! turn "black")
        (set! turn "white"))))




;                                                              
;               ;                                  ;           
;               ;                                  ;           
;               ;                                  ;           
;     ;;;;;     ;;                                 ;;          
;    ;;         ;;;                                ;;;         
;    ;        ;;;;                               ;;;;          
;   ;           ;;    ;      ;     ;     ;;;;      ;;     ;;;; 
;   ;           ;;    ;   ;; ;     ;    ;   ;;     ;;    ;;    
;    ;;         ;;    ;  ;   ;     ;    ;    ;     ;;   ;      
;     ;;;;       ;    ; ;;   ;     ;   ;            ;   ;      
;        ;;      ;    ;;;    ;    ;;   ;            ;    ;;    
;   ;     ;      ;    ;;     ;    ;;   ;            ;      ;;  
;   ;     ;      ;     ;     ;   ;;;   ;     ;      ;        ; 
;   ;    ;       ;     ;     ;  ;  ;    ;   ;       ;        ; 
;    ;;;;        ;     ;     ;  ;  ;     ;;;        ;   ;;  ;; 
;                             ;;   ;                     ;;;;  
;                                                              
(struct piece
  (name
   symbol
   side
   location
   class
   exist-pct
   pawn-info)
  #:property prop:auto-custom-write 'constructor)

(struct pawns
  (moved?
   double-move-last-turn?
   turn-of-double)
  #:property prop:auto-custom-write 'constructor)

(struct move-type
  (pawn?
   perpendicular?
   diagonal?
   knight?
   range)
  #:property prop:auto-custom-write 'constructor)

(struct posn
  (x
   y)
  #:property prop:auto-custom-write 'constructor)

                                                                                                  
;                                                                                                                   
;                                                                            ;                                      
;                                                                            ;                                      
;                                                        ;             ;     ;                                      
;                                                                            ;;                                     
;                                                                            ;;;                                    
;                                                            ;             ;;;;                                     
;       ;;  ;;        ;;;;   ;        ;;;;               ;   ;   ;;    ;     ;;     ;;;;                            
;     ;; ;  ; ;     ;;    ;  ;   ;;  ;;  ;               ;   ;   ; ;   ;     ;;    ;;                               
;     ;; ; ;  ;    ;;     ;  ;  ;    ;    ;              ;   ;  ;  ;   ;     ;;   ;                                 
;     ;   ;;  ;   ;;      ;  ; ;;   ;    ;;              ;   ; ;   ;   ;      ;   ;                                 
;     ;   ;;  ;   ;       ;  ;;;    ;;;;;                ;    ;;   ;   ;      ;    ;;                               
;     ;   ;   ;   ;       ;  ;;     ;                    ;    ;    ;   ;      ;      ;;      ;        ;        ;    
;     ;   ;   ;   ;      ;    ;     ;      ;             ;    ;    ;   ;      ;        ;    ;;;      ;;;      ;;;   
;     ;   ;   ;   ;      ;    ;      ;   ;;              ;    ;    ;   ;      ;        ;   ;;;;     ;;;;     ;;;;   
;     ;   ;   ;    ;    ;     ;       ;;;;               ;    ;    ;   ;      ;   ;;  ;;   ;; ;     ;; ;     ;; ;   
;                   ;;;;                                                           ;;;;    ;;;      ;;;      ;;;    
;                                                                                                                   
;                                                                                                                   
(define PIECE-HIGHLIGHT-COLOR (make-color 255 255 0 100))
(define MOVE-HIGHLIGHT-COLOR (make-color 255 255 0 50))
(define highlight #f)
(define highlight-posn (posn -1 -1))
(define highlight-move-posns '())


(define knight-moves
  (move-type
   #f
   #f
   #f
   #t
   1))
(define bishop-moves
  (move-type
   #f
   #f
   #t
   #f
   -1))
(define rook-moves
  (move-type
   #f
   #t
   #f
   #f
   -1))
(define king-moves
  (move-type
   #f
   #t
   #t
   #f
   1))
(define queen-moves
  (move-type
   #f
   #t
   #t
   #f
   -1))
(define pawn-moves
  (move-type
   #t
   #f
   #f
   #f
   0))

                                  
;                                    
;                        ;           
;                        ;           
;    ;             ;     ;           
;                        ;;          
;                        ;;;         
;        ;             ;;;;          
;    ;   ;   ;;    ;     ;;     ;;;; 
;    ;   ;   ; ;   ;     ;;    ;;    
;    ;   ;  ;  ;   ;     ;;   ;      
;    ;   ; ;   ;   ;      ;   ;      
;    ;    ;;   ;   ;      ;    ;;    
;    ;    ;    ;   ;      ;      ;;  
;    ;    ;    ;   ;      ;        ; 
;    ;    ;    ;   ;      ;        ; 
;    ;    ;    ;   ;      ;   ;;  ;; 
;                              ;;;;  
;
(define knight-moves-ls
  (list (posn -2 1)
        (posn -2 -1)
        (posn -1 2)
        (posn -1 -2)
        (posn 1 2)
        (posn 1 -2)
        (posn 2 1)
        (posn 2 -1)))

(define diagonal-moves-ls
  (list (posn -1 -1)
        (posn -1 1)
        (posn 1 -1)
        (posn 1 1)))

(define perpendicular-moves-ls
  (list (posn 1 0)
        (posn -1 0)
        (posn 0 1)
        (posn 0 -1)))

(define black-pieces
  (list
   (piece "pawn"
          "♟"
          "black"
          (posn 0 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 1 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 2 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 3 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 4 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 5 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 6 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "black"
          (posn 7 1)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "rook"
          "♜"
          "black"
          (posn 0 0)
          rook-moves
          100
          #f)
   (piece "knight"
          "♞"
          "black"
          (posn 1 0)
          knight-moves
          100
          #f)
   (piece "bishop"
          "♝"
          "black"
          (posn 2 0)
          bishop-moves
          100
          #f)
   (piece "queen"
          "♛"
          "black"
          (posn 3 0)
          queen-moves
          100
          #f)
   (piece "king"
          "♚"
          "black"
          (posn 4 0)
          king-moves
          100
          #f)
   (piece "bishop"
          "♝"
          "black"
          (posn 5 0)
          bishop-moves
          100
          #f)
   (piece "knight"
          "♞"
          "black"
          (posn 6 0)
          knight-moves
          100
          #f)
   (piece "rook"
          "♜"
          "black"
          (posn 7 0)
          rook-moves
          100
          #f)))

(define white-pieces
  (list
   (piece "pawn"
          "♟"
          "white"
          (posn 0 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 1 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 2 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 3 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 4 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 5 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 6 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "pawn"
          "♟"
          "white"
          (posn 7 6)
          pawn-moves
          100
          (pawns
           #f
           #f
           -1))
   (piece "rook"
          "♜"
          "white"
          (posn 0 7)
          rook-moves
          100
          #f)
   (piece "knight"
          "♞"
          "white"
          (posn 1 7)
          knight-moves
          100
          #f)
   (piece "bishop"
          "♝"
          "white"
          (posn 2 7)
          bishop-moves
          100
          #f)
   (piece "queen"
          "♛"
          "white"
          (posn 3 7)
          queen-moves
          100
          #f)
   (piece "king"
          "♚"
          "white"
          (posn 4 7)
          king-moves
          100
          #f)   
   (piece "bishop"
          "♝"
          "white"
          (posn 5 7)
          bishop-moves
          100
          #f)
   (piece "knight"
          "♞"
          "white"
          (posn 6 7)
          knight-moves
          100
          #f)
   (piece "rook"
          "♜"
          "white"
          (posn 7 7)
          rook-moves
          100
          #f)))
(define board-init
  (append white-pieces black-pieces))

                                                      
;                                                              
;                       ;                                      
;                       ;                                      
;                       ;                                      
;   ;                   ;                                      
;   ;                   ;                                      
;   ;                   ;                                      
;   ;                   ;                                      
;    ;  ;;     ;;;;     ;    ;;;;;;     ;;;;     ;        ;;;; 
;    ; ;; ;   ;;  ;     ;    ;;    ;   ;;  ;     ;   ;;  ;;    
;    ;;;  ;   ;    ;    ;   ;;     ;   ;    ;    ;  ;   ;      
;    ;;   ;  ;    ;;    ;    ;     ;  ;    ;;    ; ;;   ;      
;    ;;   ;  ;;;;;      ;    ;    ;   ;;;;;      ;;;     ;;    
;    ;    ;  ;          ;    ;    ;   ;          ;;        ;;  
;    ;    ;  ;      ;   ;    ;   ;    ;      ;    ;          ; 
;    ;    ;   ;   ;;    ;    ;;;;      ;   ;;     ;          ; 
;    ;    ;    ;;;;          ;          ;;;;      ;     ;;  ;; 
;                             ;                          ;;;;  
;                             ;                                
;                             ;                                
;                             ;
(define incr-turn-count
  (λ ()
    (set! turn-count (add1 turn-count))))

(define pawn?
  (λ (p)
    (eqv? (piece-name p) "pawn")))

(define piece-at-xy
  (λ (x y pieces)
    (cond
      [(null? pieces) #f]
      [else (let* ([p (car pieces)]
                   [p-posn (piece-location p)]
                   [p-x (posn-x p-posn)]
                   [p-y (posn-y p-posn)])
              (if (and (eqv? x p-x)
                       (eqv? y p-y))
                  p
                  (piece-at-xy x y (cdr pieces))))])))

(define actually-there?
  (λ (p)
    (eqv? (piece-exist-pct p) 100)))

(define piece-at-posn
  (λ (p pieces)
    (piece-at-xy (posn-x p) (posn-y p) pieces)))

(define pawn-double-move-last?
  (λ (p)
    (if (pawn? p)
        (let ([info (piece-pawn-info p)])
          (and
           (pawns-double-move-last-turn? info)
           (eqv? (pawns-turn-of-double info) turn-count)))
        #f)))

(define same-side
  (λ (p1 p2)
    (eqv? (piece-side p1)
          (piece-side p2))))

(define generate-possible-moves-with-incr
  (λ (p incr-x incr-y pieces)
    (letrec ([knight? (move-type-knight? (piece-class p))] ; possible bug here
             [helper
              (λ (x y range-left)
                (let* ([next-x (+ x incr-x)]
                       [next-y (+ y incr-y)]
                       [next-posn (posn next-x next-y)]
                       [piece-at-next (piece-at-posn next-posn pieces)]
                       [can-move-next (if piece-at-next
                                          (not (same-side p piece-at-next))
                                          #t)])
                  (cond
                    [(or (< x 0) (> x 7) (< y 0) (> y 7) (eqv? range-left 0)) '()]
                    [knight? (if can-move-next
                                 (cons next-posn
                                       (helper next-x next-y (sub1 range-left)))
                                 '())]
                    [else (if piece-at-next
                              (if can-move-next
                                  (list next-posn)
                                  '())
                              (cons next-posn
                                    (helper next-x next-y (sub1 range-left))))])))])
      (helper (posn-x (piece-location p)) (posn-y (piece-location p)) (move-type-range (piece-class p))))))

(define generate-possible-moves-with-incr-posn-ls
  (λ (p posn-ls pieces)
    (cond
      [(null? posn-ls) '()]
      [else (append (generate-possible-moves-with-incr p (posn-x (car posn-ls)) (posn-y (car posn-ls)) pieces)
                    (generate-possible-moves-with-incr-posn-ls p (cdr posn-ls) pieces))])))

(define get-all-possible-moves
  (λ (p pieces)
    (let ([p-x (posn-x (piece-location p))]
          [p-y (posn-y (piece-location p))]
          [p-side (piece-side p)]
          [p-pct (piece-exist-pct p)]
          [p-pawn? (move-type-pawn? (piece-class p))]
          [p-perpendicular? (move-type-perpendicular? (piece-class p))]
          [p-diagonal? (move-type-diagonal? (piece-class p))]
          [p-knight? (move-type-knight? (piece-class p))]
          [p-range (move-type-range (piece-class p))])
      (letrec ([helper
                (λ ()
                  (cond
                    [p-pawn?
                     (let* ([advance-direction (let* ([move1 (if (eqv? p-side "white")
                                                                 (posn p-x (sub1 p-y))
                                                                 (posn p-x (add1 p-y)))]
                                                      [piece-at-move1 (piece-at-posn move1 pieces)])
                                                 (if (and piece-at-move1 (actually-there? piece-at-move1))
                                                     '()
                                                     (cons move1
                                                           (if (not (pawns-moved? (piece-pawn-info p)))
                                                               (let* ([move2 (if (eqv? p-side "white")
                                                                                 (posn p-x (sub1 (sub1 p-y)))
                                                                                 (posn p-x (add1 (add1 p-y))))]
                                                                      [piece-at-move2 (piece-at-posn move2 pieces)])
                                                                 (if (and piece-at-move2 (actually-there? piece-at-move2))
                                                                     '()
                                                                     (list move2)))
                                                               '()))))]
                            [capture-move1 (if (eqv? p-side "white")
                                               (posn (sub1 p-x) (sub1 p-y))
                                               (posn (sub1 p-x) (add1 p-y)))]
                            [capture-move2 (if (eqv? p-side "white")
                                               (posn (add1 p-x) (sub1 p-y))
                                               (posn (add1 p-x) (add1 p-y)))]
                            [piece-at-capture-move1 (piece-at-posn capture-move1 pieces)]
                            [piece-at-capture-move2 (piece-at-posn capture-move2 pieces)]
                            [capture-direction1 (if (and piece-at-capture-move1 (not (same-side p piece-at-capture-move1)))
                                                    (list capture-move1)
                                                    '())]
                            [capture-direction2 (if (and piece-at-capture-move2 (not (same-side p piece-at-capture-move2)))
                                                    (list capture-move2)
                                                    '())]
                            [capture-direction (append capture-direction1 capture-direction2)]
                            [piece-adjacent-left (piece-at-xy (sub1 p-x) p-y pieces)]
                            [piece-adjacent-right (piece-at-xy (add1 p-x) p-y pieces)]
                            [left-en-passant (if (and piece-adjacent-left
                                                      (not (same-side p piece-adjacent-left))
                                                      (pawn? piece-adjacent-left)
                                                      (pawn-double-move-last? piece-adjacent-left))
                                                 (list (posn (sub1 p-x)
                                                             (if (eqv? p-side "white")
                                                                 (sub1 p-y)
                                                                 (add1 p-y))))
                                                 '())]
                            [right-en-passant (if (and piece-adjacent-right
                                                       (not (same-side p piece-adjacent-right))
                                                       (pawn? piece-adjacent-right)
                                                       (pawn-double-move-last? piece-adjacent-right))
                                                  (list (posn (add1 p-x)
                                                              (if (eqv? p-side "white")
                                                                  (sub1 p-y)
                                                                  (add1 p-y))))
                                                  '())]
                            [en-passant-direction (append left-en-passant right-en-passant)])
                       (append advance-direction capture-direction en-passant-direction))]
                    [else
                     (let* ([perpendicular-moves (if p-perpendicular?
                                                     (generate-possible-moves-with-incr-posn-ls p perpendicular-moves-ls pieces)
                                                     '())]
                            [diagonal-moves (if p-diagonal?
                                                (generate-possible-moves-with-incr-posn-ls p diagonal-moves-ls pieces)
                                                '())]
                            [knight-moves (if p-knight?
                                              (generate-possible-moves-with-incr-posn-ls p knight-moves-ls pieces)
                                              '())])
                       (append perpendicular-moves diagonal-moves knight-moves))]))])
        (helper)))))

(define highlight-moves
  (λ (x y pieces)
    (let ([p (piece-at-xy x y pieces)])
      (if p
          (set! highlight-move-posns (get-all-possible-moves p pieces))
          #f))))

(define xy-member-posn-ls?
  (λ (x y posn-ls)
    (cond
      [(null? posn-ls) #f]
      [(and (eqv? x (posn-x (car posn-ls)))
            (eqv? y (posn-y (car posn-ls)))) #t]
      [else (xy-member-posn-ls? x y (cdr posn-ls))])))

(define member-posn-ls?
  (λ (psn posn-ls)
    (xy-member-posn-ls? (posn-x psn) (posn-y psn) posn-ls)))

(define remove-piece
  (λ (p pieces)
    (let ([p-x (posn-x (piece-location p))]
          [p-y (posn-y (piece-location p))])
      (letrec ([helper
                (λ (pieces)
                  (cond
                    [(null? pieces) '()]
                    [(and (eqv? p-x (posn-x (piece-location (car pieces))))
                          (eqv? p-y (posn-y (piece-location (car pieces)))))
                     (cdr pieces)]
                    [else
                     (cons (car pieces) (helper (cdr pieces)))]))])
        (helper pieces)))))

(define remove-all-pieces
  (λ (ls-p pieces)
    (cond
      [(null? ls-p) pieces]
      [else (remove-all-pieces (cdr ls-p) (remove-piece (car ls-p) pieces))])))

(define piece-copy-move
  (λ (x y p)
    (piece
     (piece-name p)
     (piece-symbol p)
     (piece-side p)
     (posn x y)
     (piece-class p)
     (piece-exist-pct p)
     (if (pawn? p)
         (let ([info (piece-pawn-info p)])
           (pawns
            #t
            (> (abs (- y (posn-y (piece-location p)))) 1)
            turn-count))
         #f))))

; any piece in path of straight line from (p1,p2].
(define pieces-in-range-posns
  (λ (p pos pieces)
    (letrec ([jump? (move-type-knight? (piece-class p))]
             [p1 (piece-location p)]
             [p2 pos]
             [p1-x (posn-x p1)]
             [p1-y (posn-y p1)]
             [p2-x (posn-x p2)]
             [p2-y (posn-y p2)]
             [incr-posn (let* ([dx (- p2-x p1-x)]
                               [dy (- p2-y p1-y)]
                               [same-abs? (eqv? (abs dx) (abs dy))]
                               [one-zero? (if (or (eqv? dx 0)
                                                  (eqv? dy 0))
                                              #t
                                              #f)]
                               [x-dir (cond
                                        [(< dx 0) -1]
                                        [(eqv? dx 0) 0]
                                        [(> dx 0) 1])]
                               [y-dir (cond
                                        [(< dy 0) -1]
                                        [(eqv? dy 0) 0]
                                        [(> dy 0) 1])])
                          (if (or same-abs? one-zero?)
                              (posn x-dir y-dir)
                              (if (> (abs dx) (abs dy))
                                  (posn (+ x-dir x-dir) y-dir)
                                  (posn x-dir (+ y-dir y-dir)))))]
             [incr-x (posn-x incr-posn)]
             [incr-y (posn-y incr-posn)]
             [helper-pawn
              (λ (x y)
                '())]
             [helper-jump
              (λ (x y)
                '())]
             [helper-nonjump
              (λ (x y)
                (let ([piece-at-cur-posn (piece-at-xy x y pieces)])
                  (cond
                    [(or (< x 0) (> x 7) (< y 0) (> x 7)) '()]
                    [(and (not piece-at-cur-posn)
                          (eqv? x p2-x) (eqv? y p2-y))
                     '()]
                    [(and piece-at-cur-posn
                          (eqv? x p2-x) (eqv? y p2-y))
                     (list piece-at-cur-posn)]
                    [piece-at-cur-posn
                     (cons piece-at-cur-posn (helper-nonjump (+ x incr-x) (+ y incr-y)))]
                    [else (helper-nonjump (+ x incr-x) (+ y incr-y))])))])
      (if (pawn? p)
          (helper-pawn (posn-x p1) (posn-y p1))
          (if jump?
              (helper-jump (posn-x p1) (posn-y p1))
              (helper-nonjump (+ (posn-x p1) incr-x) (+ (posn-y p1) incr-y)))))))

(define move-piece
  (λ (x y pieces)
    (let* ([p-posn highlight-posn]
           [p (piece-at-xy (posn-x p-posn) (posn-y p-posn) pieces)])
      ; are we moving a valid piece, and are we going to a place we previously highlighted
      (if (and p (xy-member-posn-ls? x y highlight-move-posns))
          (let ([pieces-along-path (pieces-in-range-posns p (posn x y) pieces)])
            (incr-turn-count)
            (println pieces-along-path)
            (if (null? pieces-along-path)
                (begin
                  (println 'case1)
                  (cons (piece-copy-move x y p) (remove-piece p pieces)))
                (begin
                  (println 'case2)
                  (cons (piece-copy-move x y p) (remove-all-pieces pieces-along-path (remove-piece p pieces))))))
          pieces))))



;           ;                             
;           ;                             
;           ;                             
;           ;                             
;           ;                             
;           ;                             
;           ;                             
;      ;;;  ;   ;         ;;;   ;        ;
;     ;  ;; ;   ;   ;;  ;;   ;  ;       ; 
;    ;     ;;   ;  ;         ;  ;       ; 
;   ;       ;   ; ;;         ;  ;       ; 
;   ;       ;   ;;;       ;; ;   ;  ;   ; 
;   ;      ;;   ;;      ;;  ;;   ; ; ; ;  
;   ;     ;;;    ;      ;   ;;   ; ;  ;;  
;   ;;   ;; ;    ;      ;  ; ;   ;;   ;;  
;     ;;;   ;    ;      ;;;  ;    ;    ;  
;           ;
(define get-square-color
  (λ (x y)
    (if (even? (+ x y))
        WHITE-SQUARE-COLOR
        BLACK-SQUARE-COLOR)))

(define draw-square
  (λ (x y color board)
    (place-image
     (square (/ WIDTH 8)
             "solid"
             color)
     (+ (* x SQUARE-DIM) OFFSET)
     (+ (* y SQUARE-DIM) OFFSET)
     board)))

(define draw-empty-board
  (λ ()
    (letrec ([helper (λ (x y)
                       (cond
                         [(> y 7) (empty-scene WIDTH HEIGHT)]
                         [(> x 7) (helper 0 (add1 y))]
                         [(and highlight
                               (eqv? x (posn-x highlight-posn))
                               (eqv? y (posn-y highlight-posn))
                               (draw-square x
                                            y
                                            PIECE-HIGHLIGHT-COLOR
                                            (draw-square x y (get-square-color x y) (helper (add1 x) y))))]
                         [(and highlight
                               (xy-member-posn-ls? x y highlight-move-posns))
                          (draw-square x
                                       y
                                       MOVE-HIGHLIGHT-COLOR
                                       (draw-square x y (get-square-color x y) (helper (add1 x) y)))]
                         [else (draw-square x y (get-square-color x y) (helper (add1 x) y))]))])
      (helper 0 0))))


(define draw-board
  (λ (pieces)
    (cond
      [(null? pieces) (draw-empty-board)]
      [else
       (let* ([p (car pieces)]
              [p-loc (piece-location p)]
              [p-x (posn-x p-loc)]
              [p-y (posn-y p-loc)]
              [p-side (piece-side p)]
              [p-color (if (eqv? p-side "white")
                           WHITE-PIECE-COLOR
                           BLACK-PIECE-COLOR)]
              [p-symbol (piece-symbol p)])
         (place-image
          (text
           p-symbol
           PIECE-SIZE
           p-color)
          (+ (* p-x SQUARE-DIM) OFFSET)
          (+ (* p-y SQUARE-DIM) OFFSET)
          (draw-board (cdr pieces))))])))

   
;                                                      
;       ;;  ;;        ;;;;  ;     ;     ;;;;    ;;;;   
;     ;; ;  ; ;     ;;    ; ;     ;    ;;      ;;  ;   
;     ;; ; ;  ;    ;;     ; ;     ;   ;        ;    ;  
;     ;   ;;  ;   ;;      ; ;     ;   ;       ;    ;;  
;     ;   ;;  ;   ;       ; ;    ;;    ;;     ;;;;;    
;     ;   ;   ;   ;       ; ;    ;;      ;;   ;        
;     ;   ;   ;   ;      ;  ;   ;;;        ;  ;      ; 
;     ;   ;   ;   ;      ;  ;  ;  ;        ;   ;   ;;  
;     ;   ;   ;    ;    ;   ;  ;  ;   ;;  ;;    ;;;;   
;                   ;;;;     ;;   ;    ;;;;            
;

; left click: deterministic moves
; right click: quantum moves
; click on a non-playable piece: do nothing
; click on a piece, then display all possible moves
; if: click on a possible move -> execute
; else: display all possible moves for the other piece clicked
(define mouse-controls
  (λ (pieces x y mouse-event)
    (if (eqv? mouse-event "button-down")
        (let ([x (exact-floor (/ x SQUARE-DIM))]
              [y (exact-floor (/ y SQUARE-DIM))])
          (if (and (< x 8) (> x -1) (< y 8) (> y -1))
              (if highlight
                  (begin
                    (set! highlight #f)
                    (move-piece x y pieces))
                  (begin
                    (if (highlight-moves x y pieces)
                        (begin
                          (set! highlight #t)
                          (set! highlight-posn (posn x y)))
                        (void))
                    pieces))
              pieces))
        pieces)))

       
;                                                                                  
;     ;;;;;    ;                              ;;;;;                                
;    ;;   ;                                  ;;   ;                                
;     ;   ;                                   ;   ;                                
;     ;  ;            ;;;;                    ;  ;              ;            ;;;;  
;     ;  ;     ;    ;;    ;                   ;  ;       ;;;    ;   ;;     ;;    ; 
;     ; ;;;    ;   ;;     ;                   ; ;;;    ;;   ;   ;   ; ;   ;;     ; 
;     ;;   ;   ;   ;      ;   ;;              ;;   ;        ;   ;  ;  ;   ;      ; 
;     ;    ;   ;  ;      ;;   ;;;;;;;;;;;     ;    ;        ;   ; ;   ;  ;      ;; 
;     ;    ;   ;  ;      ;;                   ;    ;     ;; ;    ;;   ;  ;      ;; 
;      ;   ;   ;  ;     ; ;                    ;   ;   ;;  ;;    ;    ;  ;     ; ; 
;      ;  ;;   ;  ;;   ;  ;                    ;  ;;   ;   ;;    ;    ;  ;;   ;  ; 
;      ;  ;    ;    ;;;   ;                    ;  ;    ;  ; ;    ;    ;    ;;;   ; 
;      ;;;     ;          ;                    ;;;     ;;;  ;    ;    ;          ; 
;      ;             ;    ;                    ;                            ;    ; 
;                    ;    ;                                                 ;    ; 
;                     ;;  ;                                                  ;;  ; 
;                       ;;;                                                    ;;; 
(big-bang board-init
          (to-draw draw-board)
          (on-mouse mouse-controls))


                                      
;                                            
;      ;                         ;           
;      ;                         ;           
;      ;                         ;           
;      ;;                        ;;          
;      ;;;                       ;;;         
;    ;;;;                      ;;;;          
;      ;;     ;;;;      ;;;;     ;;     ;;;; 
;      ;;    ;;  ;     ;;        ;;    ;;    
;      ;;    ;    ;   ;          ;;   ;      
;       ;   ;    ;;   ;           ;   ;      
;       ;   ;;;;;      ;;         ;    ;;    
;       ;   ;            ;;       ;      ;;  
;       ;   ;      ;       ;      ;        ; 
;       ;    ;   ;;        ;      ;        ; 
;       ;     ;;;;    ;;  ;;      ;   ;;  ;; 
;                      ;;;;            ;;;;  
;
(module+ test
  (require rackunit)
  
  (define knight1
    (piece "knight"
          "♞"
          "black"
          (posn 3 3)
          knight-moves
          100
          #f))
)