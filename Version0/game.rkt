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
(define PIECE-HIGHLIGHT-COLOR (make-color 255 255 0 50))
(define MOVE-HIGHLIGHT-COLOR (make-color 255 255 0 25))

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

(struct move
  (x
   y))

(struct posn
  (x
   y))


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
          "white"
          (posn 0 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 1 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 2 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 3 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 4 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 5 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 6 1)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "white"
          (posn 7 1)
          pawn-moves
          100
          #f)
   (piece "rook"
          "♜"
          "white"
          (posn 0 0)
          rook-moves
          100
          #f)
   (piece "knight"
          "♞"
          "white"
          (posn 1 0)
          knight-moves
          100
          #f)
   (piece "bishop"
          "♝"
          "white"
          (posn 2 0)
          bishop-moves
          100
          #f)
   (piece "queen"
          "♛"
          "white"
          (posn 3 0)
          queen-moves
          100
          #f)
   (piece "king"
          "♚"
          "white"
          (posn 4 0)
          king-moves
          100
          #f)
   (piece "bishop"
          "♝"
          "white"
          (posn 5 0)
          bishop-moves
          100
          #f)
   (piece "knight"
          "♞"
          "white"
          (posn 6 0)
          knight-moves
          100
          #f)
   (piece "rook"
          "♜"
          "white"
          (posn 7 0)
          rook-moves
          100
          #f)))

(define black-pieces
  (list
   (piece "pawn"
          "♟"
          "black"
          (posn 0 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 1 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 2 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 3 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 4 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 5 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 6 6)
          pawn-moves
          100
          #f)
   (piece "pawn"
          "♟"
          "black"
          (posn 7 6)
          pawn-moves
          100
          #f)
   (piece "rook"
          "♜"
          "black"
          (posn 0 7)
          rook-moves
          100
          #f)
   (piece "knight"
          "♞"
          "black"
          (posn 1 7)
          knight-moves
          100
          #f)
   (piece "bishop"
          "♝"
          "black"
          (posn 2 7)
          bishop-moves
          100
          #f)
   (piece "queen"
          "♛"
          "black"
          (posn 3 7)
          queen-moves
          100
          #f)
   (piece "king"
          "♚"
          "black"
          (posn 4 7)
          king-moves
          100
          #f)   
   (piece "bishop"
          "♝"
          "black"
          (posn 5 7)
          bishop-moves
          100
          #f)
   (piece "knight"
          "♞"
          "black"
          (posn 6 7)
          knight-moves
          100
          #f)
   (piece "rook"
          "♜"
          "black"
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
                         [else (draw-square x y (get-square-color x y) (helper (add1 x) y))]))])
      (helper 0 0))))


(define draw-board
  (λ (pieces)
    (cond
      [(null? pieces) (draw-empty-board)]
      [else
       (println "drew")
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
(define highlight-square
  (λ (x y pieces)
    (println pieces)
    (draw-square
     x
     y
     PIECE-HIGHLIGHT-COLOR
     (draw-square
      x
      y
      (get-square-color x y)
      (draw-board pieces)))))

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
              (highlight-square x y pieces)
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