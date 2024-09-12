#lang racket
(require "TikTakToeLogic.rkt")

(require racket/gui)  ; Importar la librería GUI
(require racket/draw)  ; Importar la librería de dibujo

;; Definir colores
(define gris (make-object color% 102 102 102))  ;; Color gris
(define blanco (make-object color% 255 255 255))  ;; Color blanco

;; Función para generar una matriz MxN para el tablero
(define (crear-tablero m n)
  (TTT m n)
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

;; Función para manejar clics de mouse y detectar las coordenadas del cuadro clicado
(define (mouse-event-handler event m n ancho-alto)
  (let ((x (send event get-x))
        (y (send event get-y)))
    (define fila (quotient y ancho-alto))
    (define columna (quotient x ancho-alto))
    (when (and (< fila m) (< columna n))
      (printf "Coordenada del cuadro clicado: (~a, ~a)\n" columna fila))))  ;; Ahora imprime (columna, fila)

;; Crear una subclase de canvas% para manejar eventos de mouse, pasando m, n y el tamaño del cuadro
(define my-canvas%
  (class canvas%
    (init-field m n ancho-alto)
    (define/override (on-event event)
      (when (send event button-down?)  ;; Detectar clic izquierdo
        (mouse-event-handler event m n ancho-alto)))
    (super-new)))

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
                         (m m) (n n) (ancho-alto 80)
                         (paint-callback
                          (lambda (canvas dc)
                            (draw-canvas canvas dc tablero 80)))))

     ;; Mostrar el marco
     (send frame show #t))))

;; Iniciar la aplicación
(start-app)

