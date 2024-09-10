#lang racket

(define (TTT m n)
  (cond ((< m 3)(display "El minimo de la matriz es 3x3"))
        ((< n 3)(display "El minimo de la matriz es 3x3"))
        ((> n 10)(display "El maximo de la matriz es 10x10"))
        ((> m 10)(display "El maximo de la matriz es 10x10"))
        (else (playerTurn (createMat m n)))))

(define (createMat m n)
    (if (= m 0)
        '()
        (cons (createRow n) (createMat (- m 1) n))))

(define (createRow n)
    (if (= n 0)
        '()
        (cons 0 (createRow (- n 1)))))

(define (playerTurn mat)
  (display "Turno del jugador:\n")
  (display "Ingrese la fila:\n")
  (let ((i (read)))  
  (display "Ingrese la columna:\n")
  (let ((j (read)))  
  (markPositioni mat i j))))


(define (markPositioni mat i j)
 (markPositionj mat (list-ref mat i) i j))

(define (markPositionj mat list i j)
  (playerTurn (list-set mat i (list-set list j 1)) ))


(define (list-set lst index value)
  (cond
    ((< index 0) (error "Index fuera de rango"))
    ((zero? index) (cons value (cdr lst)))
    (else (cons (car lst) (list-set (cdr lst) (- index 1) value)))))



(TTT 6 5)
  