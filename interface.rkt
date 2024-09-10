#lang racket
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define z (open-viewport "TicTacToe" 660 660))

(define gris (make-rgb 0.40 0.40 0.40))
(define blanco (make-rgb 1 1 1))  ;; Definimos el color blanco

(define u 0)  ;; Variable para alternar las filas

;; Dibujo del tablero
(for ([v (in-range 11 651 80)]) 
  (if (= u 0)
      (begin
        (for ([h (in-range 11 651 160)]) 
          ((draw-solid-rectangle z) (make-posn h v) 79 79 gris))
        (for ([h (in-range 91 651 160)])  
          ((draw-solid-rectangle z) (make-posn h v) 79 79 blanco))
        (set! u 1)) 
      (begin
        (for ([h (in-range 11 651 160)])  
          ((draw-solid-rectangle z) (make-posn h v) 79 79 blanco))
        (for ([h (in-range 91 651 160)]) 
          ((draw-solid-rectangle z) (make-posn h v) 79 79 gris))
        (set! u 0)))) 
