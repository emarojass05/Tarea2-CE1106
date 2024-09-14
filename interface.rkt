#lang racket
(require "TikTakToeLogic.rkt")
(require racket/gui)  ; Importar la librería GUI
(require racket/draw)  ; Importar la librería de dibujo

;; Definir colores
(define gris (make-object color% 102 102 102))  ;; Color gris
(define blanco (make-object color% 255 255 255))

;; Función para generar una matriz MxN para el tablero
(define (crear-tablero m n)
  (for/list ((i (in-range m)))
    (for/list ((j (in-range n)))
      (if (even? (+ i j)) gris blanco))))  ;; Alternar colores (gris y blanco)

;; Función para dibujar el tablero basado en el tamaño MxN
(define (draw-canvas canvas dc tablero ancho-alto)
  (let* ((m (length tablero))  ;; Número de filas
         (n (length (first tablero))))  ;; Número de columnas
    (for ((fila tablero) (v (in-range 0 m)))
      (for ((cuadro fila) (h (in-range 0 n)))
        (define x (* h ancho-alto))  ;; Calcular la posición x
        (define y (* v ancho-alto))  ;; Calcular la posición y
        (send dc set-brush (new brush% (color cuadro)))  ;; Establecer el color del pincel
        (send dc draw-rectangle x y ancho-alto ancho-alto)))))  ;; Dibujar y rellenar el rectángulo

;; Función para dibujar una "X" en una casilla
(define (dibujar-X dc x y size)
  (send dc set-pen "black" 2 'solid)  ;; Establecer el color y grosor de la línea
  (send dc draw-line x y (+ x size) (+ y size))  ;; Línea diagonal de esquina superior izquierda a esquina inferior derecha
  (send dc draw-line x (+ y size) (+ x size) y))  ;; Línea diagonal de esquina inferior izquierda a esquina superior derecha

;; Función para dibujar un círculo en una casilla
(define (dibujar-circulo dc x y size)
  (send dc set-pen "black" 2 'solid)  
  (send dc draw-ellipse x y size size)) 

;; Modificación del mouse-event-handler para dibujar la X
(define (mouse-event-handler event m n ancho-alto dc tablero)
  (let ((x (send event get-x))  ;; Obtener la coordenada x del clic
        (y (send event get-y))) ;; Obtener la coordenada y del clic
    (define fila (quotient y ancho-alto))  ;; Calcular la fila basada en la coordenada y
    (define columna (quotient x ancho-alto))  ;; Calcular la columna basada en la coordenada x
    (when (and (< fila m) (< columna n))  ;; Asegurarse de que el clic esté dentro del tablero
      (printf "Coordenada del cuadro clicado: (~a, ~a)\n" columna fila)
      (dibujar-X dc (* columna ancho-alto) (* fila ancho-alto) ancho-alto)
      (dibujar-circulo dc (* 2 ancho-alto) (* 2 ancho-alto) ancho-alto);; Dibujar una "X" en la casilla clicada
      (replace-matrix (playerTurn my-matrix fila columna) dc (* columna ancho-alto) (* fila ancho-alto) ancho-alto ))))  ;; Actualizar la matriz del juego

;; Crear una subclase de canvas% para manejar eventos de mouse, pasando m, n, el tamaño del cuadro y el tablero
(define my-canvas%
  (class canvas%
    (init-field m n ancho-alto tablero)  ;; Se añade tablero como un campo
    (define/override (on-event event)
      (when (send event button-down?)  ;; Detectar clic izquierdo
        (let ((dc (send this get-dc)))  ;; Obtener el contexto de dibujo
          (mouse-event-handler event m n ancho-alto dc tablero))))  ;; Pasar tablero al manejador de eventos
    (super-new)))

;; Función para obtener el tamaño MxN de la matriz del usuario
(define (get-matrix-size callback)
  (define dlg (new dialog% (label "Tamaño del tablero")))
  (define m-input (new text-field% (label "Filas (3-8): ") (parent dlg)))
  (define n-input (new text-field% (label "Columnas (3-8): ") (parent dlg)))
  
  (define ok-button
    (new button%
         (label "OK")
         (parent dlg)
         (callback
          (lambda (button event)
            (define m (string->number (send m-input get-value)))
            (define n (string->number (send n-input get-value)))
            (if (and (<= 3 m 8) (<= 3 n 8))
                (begin
                  (send dlg show #f)  ;; Cerrar el cuadro de diálogo
                  (callback m n))  ;; Llamar al callback con MxN
                (message-box "Error" "Introduce un valor entre 3 y 8."))))))

  (send dlg show #t))

;; Función principal para iniciar la aplicación
(define (start-app)
  (get-matrix-size 
   (lambda (m n)
     
     ;; Crear el marco principal para la aplicación
     (define frame (new frame% (label "TicTacToe")
                         (width (min 620 (* n 120)))
                         (height (min 620 (* m 120)))))

     ;; Crear la matriz para el tablero
     (define tablero (crear-tablero m n))

     ;; Establecer el callback de pintado para el canvas
     (define canvas (new my-canvas% (parent frame)
                         (m m) (n n) (ancho-alto 80) (tablero tablero)
                         (paint-callback
                          (lambda (canvas dc)
                            (draw-canvas canvas dc tablero 80)))))

     ;; Mostrar el marco
     (send frame show #t)
     (TTT m n))))

(define my-matrix (createMat 9 9))

;; Función para reemplazar la matriz completa
(define (replace-matrix new-matrix  dc x y size)
  (dibujar-circulo dc (* y size) (* x size) size)
  (set! my-matrix new-matrix))

;; Iniciar la aplicación
(start-app)
