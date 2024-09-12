#lang racket

(provide TTT)

(define (TTT m n)
  (cond ((< m 3) (display "El mínimo de la matriz es 3x3"))
        ((< n 3) (display "El mínimo de la matriz es 3x3"))
        ((> n 10) (display "El máximo de la matriz es 10x10"))
        ((> m 10) (display "El máximo de la matriz es 10x10"))
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
  (display "Ingrese la fila (1-indexado):\n")
  (let ((i (sub1 (read))))  ; Convertir de 1-indexado a 0-indexado
    (display "Ingrese la columna (1-indexado):\n")
    (let ((j (sub1 (read))))  ; Convertir de 1-indexado a 0-indexado
      (let ((new-mat (markPosition mat i j 1)))  ; Marcamos con '1' para el jugador
        (display "\nTablero después del turno del jugador:\n")
        (printMat new-mat)
        (if (lineComplete new-mat)
            (begin
              (display "¡Línea completa! Reiniciando la matriz...\n")
              (playerTurn (createMat (length new-mat) (length (list-ref new-mat 0)))))  ; Reinicia la matriz
            (machineTurn new-mat))))))


(define (machineTurn mat)
  (display "Turno de la máquina:\n")
  (let ((new-mat (greedyMove mat)))  ; Usamos el algoritmo codicioso para el turno de la máquina
    (display "\nTablero después del turno de la máquina:\n")
    (printMat new-mat)
    (if (lineComplete new-mat)
        (begin
          (display "¡Línea completa! Reiniciando la matriz...\n")
          (playerTurn (createMat (length new-mat) (length (list-ref new-mat 0)))))  ; Reinicia la matriz
        (playerTurn new-mat))))


;; Simplificación de `markPosition`
(define (markPosition mat i j value)
  (if (and (>= i 0) (< i (length mat)) (>= j 0) (< j (length (list-ref mat 0))))
      (let* ((row (list-ref mat i))
             (new-row (list-set row j value)))
        (list-set mat i new-row))
      mat))  ; Devuelve la matriz original si la posición está fuera de los límites


(define (list-set lst index value)
  (cond
    ((< index 0) (error "Index fuera de rango"))
    ((zero? index) (cons value (cdr lst)))
    (else (cons (car lst) (list-set (cdr lst) (- index 1) value)))))


(define (printMat mat)
  (for-each (lambda (row) (display row) (newline)) mat))


;; Función para movimiento codicioso de la máquina
(define (greedyMove mat)
  (or (find-winning-move mat 2)  ; Busca una jugada ganadora para la máquina ('2')
      (find-blocking-move mat 1)  ; Si no hay jugada ganadora, busca bloquear al jugador ('1')
      (make-best-move mat)))  ; Si no hay jugadas críticas, hace el mejor movimiento disponible


;; Busca una jugada ganadora para el símbolo dado (puede ser '2' para la máquina o '1' para bloquear)
(define (find-winning-move mat symbol)
  (or (check-horizontal-alignment mat symbol)
      (check-vertical-alignment mat symbol)
      (check-diagonal-main-alignment mat symbol)
      (check-diagonal-secondary-alignment mat symbol)))


;; Busca un movimiento para bloquear al jugador (símbolo 1)
(define (find-blocking-move mat symbol)
  (or (check-horizontal-block mat symbol)
      (check-vertical-block mat symbol)
      (check-diagonal-main-block mat symbol)
      (check-diagonal-secondary-block mat symbol)))


;; Verifica una alineación horizontal para ganar o bloquear
(define (check-horizontal-alignment mat symbol)
  (let loop ((row 0))
    (cond ((>= row (length mat)) #f)
          ((check-horizontal-line mat row symbol) mat)
          (else (loop (+ row 1))))))

(define (check-horizontal-line mat row symbol)
  (let loop ((col 0))
    (cond ((>= col (- (length (list-ref mat 0)) 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) symbol)
                (= (list-ref (list-ref mat row) (+ col 1)) symbol)
                (= (list-ref (list-ref mat row) (+ col 2)) 0))
           (markPosition mat row (+ col 2) 2))
          (else (loop (+ col 1))))))


;; Verifica una alineación vertical para ganar o bloquear
(define (check-vertical-alignment mat symbol)
  (let loop ((col 0))
    (cond ((>= col (length (list-ref mat 0))) #f)
          ((check-vertical-line mat col symbol) mat)
          (else (loop (+ col 1))))))

(define (check-vertical-line mat col symbol)
  (let loop ((row 0))
    (cond ((>= row (- (length mat) 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) symbol)
                (= (list-ref (list-ref mat (+ row 1)) col) symbol)
                (= (list-ref (list-ref mat (+ row 2)) col) 0))
           (markPosition mat (+ row 2) col 2))
          (else (loop (+ row 1))))))


;; Verifica una alineación diagonal principal para ganar o bloquear
(define (check-diagonal-main-alignment mat symbol)
  (let loop ((i 0))
    (cond ((>= i (- (length mat) 2)) #f)
          ((and (= (list-ref (list-ref mat i) i) symbol)
                (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) symbol)
                (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 0))
           (markPosition mat (+ i 2) (+ i 2) 2))
          (else (loop (+ i 1))))))


;; Verifica una alineación diagonal secundaria para ganar o bloquear
(define (check-diagonal-secondary-alignment mat symbol)
  (let loop ((i 0))
    (let ((max (- (length mat) 1)))  ; Calcula el índice máximo
      (cond ((>= i (- (length mat) 2)) #f)
            ((and (= (list-ref (list-ref mat i) (- max i)) symbol)
                  (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) symbol)
                  (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 0))
             (markPosition mat (+ i 2) (- max (+ i 2)) 2))
            (else (loop (+ i 1)))))))


;; Define las funciones de verificación de bloqueos
(define (check-horizontal-block mat symbol)
  (let loop ((row 0))
    (cond ((>= row (length mat)) #f)
          ((check-horizontal-line-block mat row symbol) mat)
          (else (loop (+ row 1))))))

(define (check-horizontal-line-block mat row symbol)
  (let loop ((col 0))
    (cond ((>= col (- (length (list-ref mat 0)) 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) symbol)
                (= (list-ref (list-ref mat row) (+ col 1)) symbol)
                (= (list-ref (list-ref mat row) (+ col 2)) 0))
           (markPosition mat row (+ col 2) 2))
          (else (loop (+ col 1))))))


(define (check-vertical-block mat symbol)
  (let loop ((col 0))
    (cond ((>= col (length (list-ref mat 0))) #f)
          ((check-vertical-line-block mat col symbol) mat)
          (else (loop (+ col 1))))))

(define (check-vertical-line-block mat col symbol)
  (let loop ((row 0))
    (cond ((>= row (- (length mat) 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) symbol)
                (= (list-ref (list-ref mat (+ row 1)) col) symbol)
                (= (list-ref (list-ref mat (+ row 2)) col) 0))
           (markPosition mat (+ row 2) col 2))
          (else (loop (+ row 1))))))


(define (check-diagonal-main-block mat symbol)
  (let loop ((i 0))
    (cond ((>= i (- (length mat) 2)) #f)
          ((and (= (list-ref (list-ref mat i) i) symbol)
                (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) symbol)
                (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 0))
           (markPosition mat (+ i 2) (+ i 2) 2))
          (else (loop (+ i 1))))))


(define (check-diagonal-secondary-block mat symbol)
  (let loop ((i 0))
    (let ((max (- (length mat) 1)))  ; Calcula el índice máximo
      (cond ((>= i (- (length mat) 2)) #f)
            ((and (= (list-ref (list-ref mat i) (- max i)) symbol)
                  (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) symbol)
                  (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 0))
             (markPosition mat (+ i 2) (- max (+ i 2)) 2))
            (else (loop (+ i 1)))))))


;; Verifica si hay una línea completa de tres
(define (lineComplete mat)
  (or (check-line-complete-horizontal mat)
      (check-line-complete-vertical mat)
      (check-line-complete-diagonal-main mat)
      (check-line-complete-diagonal-secondary mat)))


;; Verifica si hay una línea completa horizontal
(define (check-line-complete-horizontal mat)
  (let loop ((row 0))
    (cond ((>= row (length mat)) #f)
          ((check-horizontal-line-complete mat row) #t)
          (else (loop (+ row 1))))))

(define (check-horizontal-line-complete mat row)
  (let loop ((col 0))
    (cond ((>= col (- (length (list-ref mat 0)) 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) 1)
                (= (list-ref (list-ref mat row) (+ col 1)) 1)
                (= (list-ref (list-ref mat row) (+ col 2)) 1))
           #t)
          (else (loop (+ col 1))))))


;; Verifica si hay una línea completa vertical
(define (check-line-complete-vertical mat)
  (let loop ((col 0))
    (cond ((>= col (length (list-ref mat 0))) #f)
          ((check-vertical-line-complete mat col) #t)
          (else (loop (+ col 1))))))

(define (check-vertical-line-complete mat col)
  (let loop ((row 0))
    (cond ((>= row (- (length mat) 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) 1)
                (= (list-ref (list-ref mat (+ row 1)) col) 1)
                (= (list-ref (list-ref mat (+ row 2)) col) 1))
           #t)
          (else (loop (+ row 1))))))


;; Verifica si hay una línea completa diagonal principal
(define (check-line-complete-diagonal-main mat)
  (let loop ((i 0))
    (cond ((>= i (- (length mat) 2)) #f)
          ((and (= (list-ref (list-ref mat i) i) 1)
                (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) 1)
                (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 1))
           #t)
          (else (loop (+ i 1))))))


;; Verifica si hay una línea completa diagonal secundaria
(define (check-line-complete-diagonal-secondary mat)
  (let loop ((i 0))
    (let ((max (- (length mat) 1)))  ; Calcula el índice máximo
      (cond ((>= i (- (length mat) 2)) #f)
            ((and (= (list-ref (list-ref mat i) (- max i)) 1)
                  (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) 1)
                  (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 1))
             #t)
            (else (loop (+ i 1)))))))


;; Hace el mejor movimiento disponible (por simplicidad, el primero vacío encontrado)
(define (make-best-move mat)
  (let loop ((i 0) (j 0))
    (cond ((>= i (length mat)) mat)
          ((>= j (length (list-ref mat 0))) (loop (+ i 1) 0))
          ((= (list-ref (list-ref mat i) j) 0)
           (markPosition mat i j 2))  ; Marca '2' para la máquina
          (else (loop i (+ j 1))))))

