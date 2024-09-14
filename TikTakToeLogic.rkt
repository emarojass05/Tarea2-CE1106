#lang racket

;; =======================
;; 1. Funciones Principales
;; =======================

(provide TTT playerTurn createMat replace-matrix )


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

;;2.5
(define (track-block-position mat row col)
  (displayln (string-append "Posición del último bloqueo: (" 
                            (number->string row) ", " 
                            (number->string col) ")"))
  (list row col))  ;; Devuelve la matriz sin cambios


;; Función recursiva para rastrear los movimientos de la máquina
(define (trackMove mat)
  ;; Función recursiva auxiliar para recorrer las filas y columnas
  (define (find-moves row col)
    (cond
      [(>= row (length mat)) mat]  ; Caso base: hemos revisado todas las filas
      [(>= col (length (list-ref mat row))) (find-moves (+ row 1) 0)]  ; Recorremos a la siguiente fila
      [(= (list-ref (list-ref mat row) col) 2)  ; Movimiento de la máquina encontrado
       (begin
         (displayln (string-append "Movimiento de la máquina en posición: (" 
                                   (number->string row) ", " 
                                   (number->string col) ")"))
         (find-moves row (+ col 1)))]  ; Recorremos a la siguiente columna
      [else (find-moves row (+ col 1))]))  ; Continúa en la misma fila
    
  (find-moves 0 0))  ; Comienza desde la primera fila y columna

(define (replace-matrix mat row col value)
  (list-set mat row (list-set (list-ref mat row) col value)))

;;=============================

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
  (define new-mat (greedyMove mat)) ; Guarda el resultado de greedyMove
  (define updated-mat (trackMove new-mat)) ; Llama a trackMove para registrar e imprimir el movimiento
  (printMat updated-mat) ; Imprime la matriz después del turno de la máquina
  (if (lineComplete updated-mat) ; Verifica si hay una línea completa
      (begin
        (display "¡Línea completa! Reiniciando la matriz...\n")
        (playerTurn (createMat (length updated-mat) (length (list-ref updated-mat 0))) 0 0)) ; Reinicia la matriz
      updated-mat)) ; Devuelve la matriz actualizada si no hay línea completa




;; ============================
;; 4. Funciones de Marcado y Impresión
;; ============================

(define (markPosition mat row col symbol)
  (list-set mat row (list-set (list-ref mat row) col symbol)))

; Devuelve la matriz original si la posición está fuera de los límites

(define (list-set lst index value)
  (cond
    ((< index 0) (error "Index fuera de rango"))
    ((zero? index) (cons value (cdr lst)))
    (else (cons (car lst) (list-set (cdr lst) (- index 1) value)))))


(define (printMat mat)
  (cond
    [(empty? mat) '()] ; Caso base: cuando la matriz está vacía
    [else
     (display (first mat)) ; Muestra la primera fila
     (newline)
     (printMat (rest mat))])) ; Llama recursivamente para el resto de las filas



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
  (define (recursive-check row mat symbol)
    (cond
      [(>= row (length mat)) #f] ; Caso base: si revisamos todas las filas, devolvemos #f
      [(check-horizontal-line mat row symbol) 
       (displayln "WIN") 
       #t] ; Si encontramos una alineación ganadora, devolvemos #t
      [else (recursive-check (+ row 1) mat symbol)])) ; Recurre para la siguiente fila
  (recursive-check 0 mat symbol)) ; Comienza la recursión desde la fila 0




(define (check-horizontal-line mat row symbol)
  (define (recursive-check col n)
    (cond
      ;; Caso base: Si col está fuera del rango permitido
      ((>= col (- n 2)) #f)
      ;; Caso de éxito: Encontramos una alineación horizontal
      ((and (= (list-ref (list-ref mat row) col) symbol)
            (= (list-ref (list-ref mat row) (+ col 1)) symbol)
            (= (list-ref (list-ref mat row) (+ col 2)) 0))
       (markPosition mat row (+ col 2) 2))  ; Marca la posición y retorna el resultado
      ;; Caso recursivo: Recorremos a la siguiente columna
      (else (recursive-check (+ col 1) n))))
  (recursive-check 0 (length (list-ref mat row))))



;; Verifica una alineación vertical para ganar o bloquear
(define (check-vertical-alignment mat symbol)
  (define (recursive-check col)
    (cond
      ;; Caso base: Si col está fuera del rango permitido
      ((>= col (length (list-ref mat 0))) #f)
      ;; Caso de éxito: Encontramos una alineación vertical
      ((check-vertical-line mat col symbol)
       (display "WIN")
       (newline)
       mat)
      ;; Caso recursivo: Recorremos a la siguiente columna
      (else (recursive-check (+ col 1)))))
  (recursive-check 0))



(define (check-vertical-line mat col symbol)
  (define (recursive-check row n)
    (cond
      ;; Caso base: Si row está fuera del rango permitido
      ((>= row (- n 2)) #f)
      ;; Caso de éxito: Encontramos una alineación vertical
      ((and (= (list-ref (list-ref mat row) col) symbol)
            (= (list-ref (list-ref mat (+ row 1)) col) symbol)
            (= (list-ref (list-ref mat (+ row 2)) col) 0))
       (markPosition mat (+ row 2) col 2)
       #t)
      ;; Caso recursivo: Recorremos a la siguiente fila
      (else (recursive-check (+ row 1) n))))
  (recursive-check 0 (length mat)))




;; Verifica una alineación diagonal principal para ganar o bloquear
(define (check-diagonal-main-alignment mat symbol)
  (define (recursive-check i)
    (cond
      ;; Caso base: Si i está fuera del rango permitido
      ((>= i (- (length mat) 2)) #f)
      ;; Caso de éxito: Encontramos una alineación diagonal principal
      ((and (= (list-ref (list-ref mat i) i) symbol)
            (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) symbol)
            (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 0))
       (markPosition mat (+ i 2) (+ i 2) 2)
       #t)
      ;; Caso recursivo: Recorremos al siguiente índice
      (else (recursive-check (+ i 1)))))
  (recursive-check 0))




;; Verifica una alineación diagonal secundaria para ganar o bloquear
(define (check-diagonal-secondary-alignment mat symbol)
  (define (recursive-check i max)
    (cond
      ;; Caso base: Si i está fuera del rango permitido
      ((>= i (- (length mat) 2)) #f)
      ;; Caso de éxito: Encontramos una alineación diagonal secundaria
      ((and (= (list-ref (list-ref mat i) (- max i)) symbol)
            (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) symbol)
            (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 0))
       (markPosition mat (+ i 2) (- max (+ i 2)) 2)
       #t)
      ;; Caso recursivo: Recorremos al siguiente índice
      (else (recursive-check (+ i 1) max))))
  (recursive-check 0 (- (length (list-ref mat 0)) 1)))




;; ===============================
;; 8. Verificación de Bloqueos
;; ===============================


(define (check-horizontal-block mat symbol)
  (define (recursive-check row)
    (cond
      ;; Caso base: Si hemos revisado todas las filas
      ((>= row (length mat)) #f)
      ;; Caso de éxito: Encontramos un bloqueo horizontal
      ((check-horizontal-line-block mat row symbol))
      ;; Caso recursivo: Recorremos la siguiente fila
      (else (recursive-check (+ row 1)))))
  (recursive-check 0))



(define (check-horizontal-line-block mat row symbol)
  (define (check col n)
    (cond ((>= col (- n 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) symbol)
                (= (list-ref (list-ref mat row) (+ col 1)) symbol)
                (= (list-ref (list-ref mat row) (+ col 2)) 0))
           (display "BLOQUEO HORIZONTAL\n")
           (track-block-position mat row (+ col 2))  ; Almacena la posición del bloqueo
           (markPosition mat row (+ col 2) 2))  ; Marca el bloqueo en la matriz
          (else (check (+ col 1) n))))
  (check 0 (length (list-ref mat 0))))



(define (check-vertical-block mat symbol)
  (define (recursive-check col)
    (cond
      ;; Caso base: si hemos revisado todas las columnas
      ((>= col (length (list-ref mat 0))) #f)
      ;; Caso de éxito: si encontramos un bloqueo vertical
      ((check-vertical-line-block mat col symbol))  ; Devuelve #t si se encontró un bloqueo
      ;; Caso recursivo: revisamos la siguiente columna
      (else (recursive-check (+ col 1)))))
  (recursive-check 0))



(define (check-vertical-line-block mat col symbol)
  (define (check row n)
    (cond ((>= row (- n 2)) #f)
          ((and (= (list-ref (list-ref mat row) col) symbol)
                (= (list-ref (list-ref mat (+ row 1)) col) symbol)
                (= (list-ref (list-ref mat (+ row 2)) col) 0))
           (display "BLOQUEO VERTICAL\n")
           (track-block-position mat (+ row 2) col)  ; Almacena la posición del bloqueo
           (markPosition mat (+ row 2) col 2))
          (else (check (+ row 1) n))))
  (check 0 (length mat)))





(define (check-diagonal-main-block mat symbol)
  (define (check i n)
    (cond ((>= i (- n 2)) #f)
          ((and (= (list-ref (list-ref mat i) i) symbol)
                (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) symbol)
                (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 0))
           (display "BLOQUEO DIAGONAL PRINCIPAL\n")
           (track-block-position mat (+ i 2) (+ i 2))  ; Almacena la posición del bloqueo
           (markPosition mat (+ i 2) (+ i 2) 2))
          (else (check (+ i 1) n))))
  (check 0 (length mat)))




(define (check-diagonal-secondary-block mat symbol)
  (define (check i n max)
    (cond ((>= i (- n 2)) #f)
          ((and (= (list-ref (list-ref mat i) (- max i)) symbol)
                (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) symbol)
                (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 0))
           (display "BLOQUEO DIAGONAL SECUNDARIO\n")
           (track-block-position mat (+ i 2) (- max (+ i 2)))  ; Almacena la posición del bloqueo
           (markPosition mat (+ i 2) (- max (+ i 2)) 2))
          (else (check (+ i 1) n max))))
  (check 0 (length mat) (- (length (list-ref mat 0)) 1)))


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
  (define (recursive-check row)
    (cond
      ;; Caso base: hemos revisado todas las filas
      ((>= row (length mat)) #f)
      ;; Caso de éxito: si encontramos una línea completa horizontal
      ((check-horizontal-line-complete mat row) #t)
      ;; Caso recursivo: revisamos la siguiente fila
      (else (recursive-check (+ row 1)))))
  (recursive-check 0))



(define (check-horizontal-line-complete mat row)
  (define (recursive-check col)
    (let ((n (length (list-ref mat 0))))
      (cond
        ;; Caso base: hemos llegado al final de la fila sin encontrar una línea completa
        ((>= col (- n 2)) #f)
        ;; Caso de éxito: encontramos una línea completa horizontal
        ((and (= (list-ref (list-ref mat row) col) 1)
              (= (list-ref (list-ref mat row) (+ col 1)) 1)
              (= (list-ref (list-ref mat row) (+ col 2)) 1))
         #t)
        ;; Caso recursivo: revisamos la siguiente columna
        (else (recursive-check (+ col 1))))))
  (recursive-check 0))



;; Verifica si hay una línea completa vertical
(define (check-line-complete-vertical mat)
  (define (check-column col)
    (if (>= col (length (list-ref mat 0)))
        #f
        (if (check-vertical-line-complete mat col)
            #t
            (check-column (+ col 1)))))
  (check-column 0))



(define (check-vertical-line-complete mat col)
  (define (check-row row)
    (cond
      ((>= row (- (length mat) 2)) #f)
      ((and (= (list-ref (list-ref mat row) col) 1)
            (= (list-ref (list-ref mat (+ row 1)) col) 1)
            (= (list-ref (list-ref mat (+ row 2)) col) 1))
       #t)
      (else (check-row (+ row 1)))))
  (check-row 0))



;; Verifica si hay una línea completa diagonal principal
(define (check-line-complete-diagonal-main mat)
  (define (check i n)
    (cond ((>= i (- n 2)) #f)
          ((and (= (list-ref (list-ref mat i) i) 1)
                (= (list-ref (list-ref mat (+ i 1)) (+ i 1)) 1)
                (= (list-ref (list-ref mat (+ i 2)) (+ i 2)) 1))
           #t)
          (else (check (+ i 1) n))))
  (check 0 (length mat)))


;; Verifica si hay una línea completa diagonal secundaria
(define (check-line-complete-diagonal-secondary mat)
  (define (check i n max)
    (cond ((>= i (- n 2)) #f)
          ((and (= (list-ref (list-ref mat i) (- max i)) 1)
                (= (list-ref (list-ref mat (+ i 1)) (- max (+ i 1))) 1)
                (= (list-ref (list-ref mat (+ i 2)) (- max (+ i 2))) 1))
           #t)
          (else (check (+ i 1) n max))))
  (check 0 (length mat) (- (length (list-ref mat 0)) 1)))



;; =============================
;; 10. Movimiento Mejor
;; =============================

;; Hace el mejor movimiento disponible (por simplicidad, el primero vacío encontrado)
(define (make-best-move mat)
  (define (check i j)
    (cond ((>= i (length mat)) mat)
          ((>= j (length (list-ref mat 0))) (check (+ i 1) 0))
          ((= (list-ref (list-ref mat i) j) 0)
           (markPosition mat i j 2))  ; Marca '2' para la máquina
          (else (check i (+ j 1)))))
  (check 0 0))

