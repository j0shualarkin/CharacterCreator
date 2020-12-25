#lang racket

(require "choice_struct.rkt")
(require 2htdp/image)

(provide ARROW draw-choices)

;;;;;;;;;;;;;;;;;;;;;;

(define mode "solid")
(define shift .1)

;; A Direction is one of "up" | "down" | "left" | "right"

;; dir->degrees : String -> Int
(define (dir->degrees dir)
  (match dir
    ["up" 90]
    ["down" -90]
    ["right" 0]
    ["left" 180]
    [else (error "unknown direction ~s" dir)]))

;; An R is a Real on the interval [0..1]
;; ARROW : [R  Nat -> [Direction [List Color] -> Image]]
(define ((ARROW f size) dir colors)
  (rotate (dir->degrees dir)
          (let loop ((i 1)
                     (colors colors))
            (match colors
              ['() empty-image]
              [`(,c . ,d)
               (overlay 
                (loop (add1 i) d)
                (scale (- 1 (* i shift))
                       (overlay
                        (rectangle 5 (* f size) mode c)
                        (beside (square (* f size) mode c)
                                (rotate 30 (triangle size mode c))))))]))))


(define Arr1 (ARROW 0.5 100))

(define Arr2 (ARROW 0.7 100))

(define a1 (Arr1 "left" '("green")))
(define a2 (Arr1 "right" '("green" "blue")))
(define a3 (Arr1 "left" '("green" "blue" "red")))
(define a4 (Arr1 "down" '("lemonchiffon")))

(define a5 (Arr2 "left" '("green")))
(define a6 (Arr2 "right" '("green" "blue")))
(define a7 (Arr2 "left" '("green" "blue" "red")))
(define a8 (Arr2 "down" '("lemonchiffon")))



;; If Choice-Type is "Color", Choice-Value is one of "red" | "orange" | ...
;; If Choice-Type is "Shape", Choice-Value is one of "circle" | "triangle" | ...

;; A Choice is a (choice Choice-Type Choice-Value Phase)
;; (struct choice [type value] #:transparent)

#|
each Choice-Type has its own expected # of arguments

Shape : 1
Color : 0 (constant)
|#
(define CHOICE-FNS
  `(;; shape choices
    ("circle"
     ,(λ (color) (circle 30 "outline" color))
     ,(λ (color) (circle 35 "outline" color)))
    ("triangle"
     ,(λ (color) (triangle 30 "outline" color))
     ,(λ (color) (triangle/sas 30 40 20 "solid" color)))
    ;; color choices 
    ("red" ,(color 255 0 0) ,(color 100 0 0))
    ("green" ,(color 0 255 0) ,(color 100 240 0))
    ("blue" ,(color 0 0 255) ,(color 100 0 100))))

(define choices1-1
  (list
   (choice "Shape" "triangle")
   (choice "Color" "red")))

;; lookup-choice : Choice-Type (List Choice) Boolean -> Choice-Value
(define (lookup-choice c ls q?)
  (let ((access (if q? cadr caddr))
        (v (foldr (λ (x a)
                    (if (equal? c (choice-type x))
                        (choice-value x)
                        a))
                  #f
                  ls)))
    (and v (access (assoc v CHOICE-FNS)))))

;; draw-choices : [(List Choice) -> [Bool -> Image]]
(define ((draw-choices cs) quack?)
  (let ((color (lookup-choice "Color" cs quack?))
        (shape (lookup-choice "Shape" cs quack?)))
    (if (and color shape)
        (shape color)
        empty-image)))


#;
(require 2htdp/universe)
#;
(animate
 (λ (i)
   (overlay
    ((draw-choices choices1-1) (even? i))
    (rectangle 400 400 "solid" "black"))))
