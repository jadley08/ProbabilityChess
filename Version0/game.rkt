#lang racket

(require
  2htdp/batch-io
  2htdp/image
  2htdp/universe)


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
   moved?))

(struct move-type
  (pawn?
   perpendicular?
   diagonal?
   knight?
   range))

(struct posn
  (x
   y))

                                                                                                  
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
(define white-pieces
  (list
   (piece "pawn"
          "♟"
          "black"
          (posn 0 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 1 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 2 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 3 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 4 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 5 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 6 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 7 1)
          pawn-moves
          100
          #f)
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

(define black-pieces
  (list
   (piece "pawn"
          "♟"
          "white"
          (posn 0 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 1 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 2 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 3 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 4 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 5 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 6 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 7 6)
          pawn-moves
          100
          #f)
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
(define piece-at
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
                  (piece-at x y (cdr pieces))))])))

(define get-all-possible-moves
  (λ (p)
    (let ([p-x (posn-x (piece-location p))]
          [p-y (posn-y (piece-location p))]
          [p-side (piece-side p)]
          [p-pct (piece-exist-pct p)]
          [p-moved? (piece-moved? p)]
          [p-pawn? (move-type-pawn? (piece-class p))]
          [p-perpendicular? (move-type-perpendicular? (piece-class p))]
          [p-diagonal? (move-type-diagonal? (piece-class p))]
          [p-knight? (move-type-knight? (piece-class p))]
          [p-range (move-type-range (piece-class p))])
      (letrec ([helper
                (λ ()
                  (cond
                    [p-pawn?
                     (cons (posn p-x (if (eqv? p-side "white")
                                         (sub1 p-y)
                                         (add1 p-y)))
                           (if (not p-moved?)
                               (list (posn p-x (if (eqv? p-side "white")
                                                   (sub1 (sub1 p-y))
                                                   (add1 (add1 p-y)))))
                               '()))]
                    [else 'todo]))])
        (helper)))))

(define highlight-moves
  (λ (x y pieces)
    (let ([p (piece-at x y pieces)])
      (if p
          (set! highlight-move-posns (get-all-possible-moves p))
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

(define piece-copy-move
  (λ (x y p)
    (piece
     (piece-name p)
     (piece-symbol p)
     (piece-side p)
     (posn x y)
     (piece-class p)
     (piece-exist-pct p)
     #t)))

(define move-piece
  (λ (x y pieces)
    (let* ([p-posn highlight-posn]
           [p (piece-at (posn-x p-posn) (posn-y p-posn) pieces)])
      (if (and p (xy-member-posn-ls? x y highlight-move-posns))
          (begin
            (cons (piece-copy-move x y p) (remove-piece p pieces)))
          (begin
            pieces)))))



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
                    (set! highlight #t)
                    (set! highlight-posn (posn x y))
                    (highlight-moves x y pieces)
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
)