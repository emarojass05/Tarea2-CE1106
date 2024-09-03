#lang racket

(define (TTT m n)
  (cond ((< m 3)(display "El minimo de la matriz es 3x3"))
        ((< n 3)(display "El minimo de la matriz es 3x3"))
        ((> n 10)(display "El maximo de la matriz es 10x10"))
        ((> m 10)(display "El maximo de la matriz es 10x10"))
        (else (createMat m n))))

(define (createMat m n)
    (if (= m 0)
        '()
        (cons (createRow n) (createMat (- m 1) n))))

(define (createRow n)
    (if (= n 0)
        '()
        (cons 0 (createRow (- n 1)))))