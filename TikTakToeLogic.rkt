#lang racket

;; =======================
;; 1. Funciones Principales
;; =======================

(provide TTT playerTurn createMat)


(define (TTT m n)
       (createMat m n))


;; =======================
;; 2. Creación de la Matriz
;; =======================

(define (createMat m n)
  (if (= m 0)
      '()
      (cons (createRow n) (createMat (- m 1) n))))

(define (createRow n)
  (if (= n 0)
      '()
      (cons 0 (createRow (- n 1)))))


;; ==========================
;; 3. Turnos del Jugador y Máquina
;; ==========================

(define (playerTurn mat i j)
  (display "\nTablero después del turno del jugador:\n")
  (printMat (markPosition mat i j 1)) ; Llamada directa a la función para obtener new-mat
  (if (lineComplete (markPosition mat i j 1)) ; Llamada directa a la función para evaluar new-mat
      (begin
        (display "¡Línea completa! Reiniciando la matriz...\n")
        (playerTurn (createMat (length mat) (length (list-ref mat 0))) i j)) ; Reinicia la matriz y pasa las coordenadas
      (machineTurn (markPosition mat i j 1)))) ; Pasa el nuevo tablero a machineTurn
(define (machineTurn mat)
  (display "Turno de la máquina:\n")
  (printMat (greedyMove mat)) ; Llamada directa a greedyMove
  (if (lineComplete (greedyMove mat)) ; Llamada directa a greedyMove para evaluar new-mat
      (begin
        (display "¡Línea completa! Reiniciando la matriz...\n")
        (playerTurn (createMat (length mat) (length (list-ref mat 0))))) ; Reinicia la matriz
     mat)) ; No hace nada si no hay línea completa


;; ============================
;; 4. Funciones de Marcado y Impresión
;; ============================

(define (markPosition mat row col symbol)
  (let ((new-row (list-set (list-ref mat row) col symbol)))
    (list-set mat row new-row)))
; Devuelve la matriz original si la posición está fuera de los límites

(define (list-set lst index value)
  (cond
    ((< index 0) (error "Index fuera de rango"))
    ((zero? index) (cons value (cdr lst)))
    (else (cons (car lst) (list-set (cdr lst) (- index 1) value)))))


(define (printMat mat)
  (for-each (lambda (row) (display row) (newline)) mat))


;; =============================
;; 5. Movimiento Codicioso de la Máquina
;; =============================

(define (greedyMove mat)
  (or (find-winning-move mat 2)  ; Busca una jugada ganadora para la máquina ('2')
      (find-blocking-move mat 1)  ; Si no hay jugada ganadora, busca bloquear al jugador ('1')
      (make-best-move mat)))  ; Si no hay jugadas críticas, hace el mejor movimiento disponible


;; =============================
;; 6. Verificación de Movimiento Ganador o de Bloqueo
;; =============================

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


;; ===============================
;; 7. Verificación de Alineaciones
;; ===============================

;; Verifica una alineación horizontal para ganar o bloquear
;; Verifica una alineación horizontal para ganar
(define (check-horizontal-alignment mat symbol)
  (let loop ((row 0))
    (cond ((>= row (length mat)) #f)
          ((let ((result (check-horizontal-line mat row symbol)))
             (if result
                 (begin
                   (displayln "WIN")
                   result)
                 (loop (+ row 1)))))
          (else #f))))


(define (check-horizontal-line mat row symbol)
  (let ((n (length (list-ref mat 0))))
    (let loop ((col 0))
      (cond ((>= col (- n 2)) #f)
            ((and (= (list-ref (list-ref mat row) col) symbol)
                  (= (list-ref (list-ref mat row) (+ col 1)) symbol)
                  (= (list-ref (list-ref mat row) (+ col 2)) 0))
             (markPosition mat row (+ col 2) 2))
            (else (loop (+ col 1)))))))

;; Verifica una alineación vertical para ganar o bloquear
(define (check-vertical-alignment mat symbol)
  (let loop ((col 0))
    (cond ((>= col (length (list-ref mat 0))) #f)
          ((check-vertical-line mat col symbol)
           (begin (display "WIN") (newline) mat))  ; Imprime "WIN" y luego retorna la matriz
          (else (loop (+ col 1))))))

(define (check-vertical-line mat col symbol)
  (let ((n (length mat)))
    (let loop ((row 0))
      (cond ((>= row (- n 2)) #f)
            ((and (= (list-ref (list-ref mat row) col) symbol)
                  (= (list-ref (list-ref mat (+ row 1)) col) symbol)
                  (= (list-ref (list-ref mat (+ row 2)) col) 0))
             (markPosition mat (+ row 2) col 2)
             #t)  ; Indica que se encontró una alineación
            (else (loop (+ row 1)))))))


;; Verifica una alineación diagonal principal para ganar o bloquear
(define (check-diagonal-main-alignment mat symbol)
  (let ((n (length mat)))
    (let loop ((i 0))
      (cond ((>= i (- n 2)) #f)
            ((and (= (list-ref (list-ref mat i) i) symbol)
                  (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) symbol)
                  (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 0))
             (markPosition mat (+ i 2) (+ i 2) 2))
            (else (loop (+ i 1)))))))

;; Verifica una alineación diagonal secundaria para ganar o bloquear
(define (check-diagonal-secondary-alignment mat symbol)
  (let ((n (length mat))
        (max (- (length (list-ref mat 0)) 1)))  ; Calcula el índice máximo
    (let loop ((i 0))
      (cond ((>= i (- n 2)) #f)
            ((and (= (list-ref (list-ref mat i) (- max i)) symbol)
                  (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) symbol)
                  (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 0))
             (markPosition mat (+ i 2) (- max (+ i 2)) 2))
            (else (loop (+ i 1)))))))


;; ===============================
;; 8. Verificación de Bloqueos
;; ===============================

;; Define las funciones de verificación de bloqueos
;; Define las funciones de verificación de bloqueos
(define (check-horizontal-block mat symbol)
  (let loop ((row 0))
    (cond ((>= row (length mat)) #f)
          ((check-horizontal-line-block mat row symbol))  ; Devuelve la matriz actualizada
          (else (loop (+ row 1))))))

(define (check-horizontal-line-block mat row symbol)
  (let ((n (length (list-ref mat 0))))
    (let loop ((col 0))
      (cond ((>= col (- n 2)) #f)
            ((and (= (list-ref (list-ref mat row) col) symbol)
                  (= (list-ref (list-ref mat row) (+ col 1)) symbol)
                  (= (list-ref (list-ref mat row) (+ col 2)) 0))
             (display "BLOQUEO HORIZONTAL\n")  ; Imprimir "BLOQUEO HORIZONTAL"
             (printMat mat)  ; Imprimir la matriz actualizada
             (markPosition mat row (+ col 2) 2))
            (else (loop (+ col 1)))))))



(define (check-vertical-block mat symbol)
  (let loop ((col 0))
    (cond ((>= col (length (list-ref mat 0))) #f)  ; Devuelve la matriz original si no se encuentra un bloqueo
          ((check-vertical-line-block mat col symbol))  ; Devuelve la matriz actualizada
          (else (loop (+ col 1))))))

;; Verifica un bloqueo vertical
(define (check-vertical-line-block mat col symbol)
  (let ((n (length mat)))
    (let loop ((row 0))
      (cond ((>= row (- n 2)) #f)
            ((and (= (list-ref (list-ref mat row) col) symbol)
                  (= (list-ref (list-ref mat (+ row 1)) col) symbol)
                  (= (list-ref (list-ref mat (+ row 2)) col) 0))
             (display "BLOQUEO\n")  ; Imprimir "BLOQUEO"
             (markPosition mat (+ row 2) col 2))
            (else (loop (+ row 1)))))))




;; Verifica un bloqueo diagonal principal
(define (check-diagonal-main-block mat symbol)
  (let ((n (length mat)))
    (let loop ((i 0))
      (cond ((>= i (- n 2)) #f)
            ((and (= (list-ref (list-ref mat i) i) symbol)
                  (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) symbol)
                  (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 0))
             (display "BLOQUEO\n")  ; Imprimir "BLOQUEO"
             (markPosition mat (+ i 2) (+ i 2) 2))
            (else (loop (+ i 1)))))))

;; Verifica un bloqueo diagonal secundaria
(define (check-diagonal-secondary-block mat symbol)
  (let ((n (length mat))
        (max (- (length (list-ref mat 0)) 1)))  ; Calcula el índice máximo
    (let loop ((i 0))
      (cond ((>= i (- n 2)) #f)
            ((and (= (list-ref (list-ref mat i) (- max i)) symbol)
                  (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) symbol)
                  (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 0))
             (display "BLOQUEO\n")  ; Imprimir "BLOQUEO"
             (markPosition mat (+ i 2) (- max (+ i 2)) 2))
            (else (loop (+ i 1)))))))


;; =============================
;; 9. Verificación de Línea Completa
;; =============================

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
  (let ((n (length (list-ref mat 0))))
    (let loop ((col 0))
      (cond ((>= col (- n 2)) #f)
            ((and (= (list-ref (list-ref mat row) col) 1)
                  (= (list-ref (list-ref mat row) (+ col 1)) 1)
                  (= (list-ref (list-ref mat row) (+ col 2)) 1))
             #t)
            (else (loop (+ col 1)))))))

;; Verifica si hay una línea completa vertical
(define (check-line-complete-vertical mat)
  (let loop ((col 0))
    (cond ((>= col (length (list-ref mat 0))) #f)
          ((check-vertical-line-complete mat col) #t)
          (else (loop (+ col 1))))))

(define (check-vertical-line-complete mat col)
  (let ((n (length mat)))
    (let loop ((row 0))
      (cond ((>= row (- n 2)) #f)
            ((and (= (list-ref (list-ref mat row) col) 1)
                  (= (list-ref (list-ref mat (+ row 1)) col) 1)
                  (= (list-ref (list-ref mat (+ row 2)) col) 1))
             #t)
            (else (loop (+ row 1)))))))

;; Verifica si hay una línea completa diagonal principal
(define (check-line-complete-diagonal-main mat)
  (let ((n (length mat)))
    (let loop ((i 0))
      (cond ((>= i (- n 2)) #f)
            ((and (= (list-ref (list-ref mat i) i) 1)
                  (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) 1)
                  (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 1))
             #t)
            (else (loop (+ i 1)))))))

;; Verifica si hay una línea completa diagonal secundaria
(define (check-line-complete-diagonal-secondary mat)
  (let ((n (length mat))
        (max (- (length (list-ref mat 0)) 1)))  ; Calcula el índice máximo
    (let loop ((i 0))
      (cond ((>= i (- n 2)) #f)
            ((and (= (list-ref (list-ref mat i) (- max i)) 1)
                  (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) 1)
                  (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 1))
             #t)
            (else (loop (+ i 1)))))))


;; =============================
;; 10. Movimiento Mejor
;; =============================

;; Hace el mejor movimiento disponible (por simplicidad, el primero vacío encontrado)
(define (make-best-move mat)
  (let loop ((i 0) (j 0))
    (cond ((>= i (length mat)) mat)
          ((>= j (length (list-ref mat 0))) (loop (+ i 1) 0))
          ((= (list-ref (list-ref mat i) j) 0)
           (markPosition mat i j 2))  ; Marca '2' para la máquina
          (else (loop i (+ j 1))))))

