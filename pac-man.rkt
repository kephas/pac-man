#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;;;;;;;;;;;;;;;
; Données
;;;;;;;;;;;;;;;;

(define-struct monde [angle dir])



;;;;;;;;;;;;;;;;
; Logique du jeu
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
; Dessin
;;;;;;;;;;;;;;;;

(define (mr-pac rayon angle° body back)
  (let* ([angle (degrees->radians angle°)]
         [demi-angle° (/ angle° 2)]
         [x1 (+ rayon (* (cos (/ angle 2)) rayon))]
         [y1 (+ rayon (* (sin (/ angle 2)) rayon))]
         [x2 (+ rayon (* (cos (/ angle 2)) rayon))]
         [y2 (+ rayon (* -1 (sin (/ angle 2)) rayon))]
         [angle-bézier° (- 90 demi-angle°)]
         [pull (* 1.5 (/ 4 3) (tan (/ angle 4)))])
    (add-polygon (add-solid-curve
                  (circle rayon "solid" body)
                  x1 y1 angle-bézier° pull
                  x2 y2 (- 180 angle-bézier°) pull
                  back)
                 (list (make-posn rayon rayon)
                       (make-posn x1 y1)
                       (make-posn x2 y2))
                 "solid" back)))

(define (dessiner m)
  (overlay/xy
   (mr-pac 50 (monde-angle m) "yellow" "black")
   -50 -50
   (square 200 "solid" "black")))

;;;;;;;;;;;;;;;;
; Game loop
;;;;;;;;;;;;;;;;

(define (suivant m)
  (cond
    [(and (= (monde-angle m) 90) (= (monde-dir m) 1))
     (make-monde 89 -1)]
    [(and (= (monde-angle m) 0) (= (monde-dir m) -1))
     (make-monde 1 1)]
    [else 
     (make-monde (+ (monde-angle m) (monde-dir m)) (monde-dir m))]))

(big-bang (make-monde 0 1)
  (on-tick suivant .005)
  (to-draw dessiner))


