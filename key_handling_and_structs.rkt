#lang racket

(require "choice_struct.rkt")
(require "cycle_functions.rkt")
(require "cc-image.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)

(struct MyImage
  [shape color fill])


(define (move w f)
  (match w
    [(world idx choices phase)
     (world (f idx) 
            choices
            phase)]))

(define (move-up w)
  (move w (λ (idx) (modulo (sub1 idx) Amount-Choices)))
  #;
  (match w
    [(world idx choices phase)
     (world (modulo (sub1 idx) Amount-Choices)
            choices
            phase)]))

(check-equal? (move-up (world 0 ex-choices #f))
              (world 1 ex-choices #f))
(check-equal? (move-up (world 1 ex-choices #f))
              (world 0 ex-choices #f))

(define (move-down w)
  (move w (λ (idx) (modulo (add1 idx) Amount-Choices)))
  #;
  (match w
    [(world idx choices phase)
     (world (modulo (add1 idx) Amount-Choices)
            choices
            phase)]))

(check-equal? (move-down (world 0 ex-choices #f))
              (world 1 ex-choices #f))
(check-equal? (move-down (world 1 ex-choices #f))
              (world 0 ex-choices #f))

;; key-handler : World KeyEvent -> World
(define (key-handler w ke)
  (match ke
    ["w" (move-up w)]
    ["s" (move-down w)]
    ["a" (cycle-left w)]
    ["d" (cycle-right w)]
    [else w]))

(define j1_selected ((ARROW 0.6 50) "left" '("plum" "orange" "yellow")))
(define j2_selected ((ARROW 0.6 50) "right" '("plum" "orange" "yellow")))
(define j1 ((ARROW 0.6 50) "left" '("plum")))
(define j2 ((ARROW 0.6 50) "right" '("plum")))

;; draw-world : [(List Choice) -> [Bool -> Image]]

;; draw-arrows : Choice Image Image Image -> Image 
(define (draw-arrows c LeftArrow RightArrow baseImg)
  (beside LeftArrow baseImg RightArrow))

;; draw-highlighted-arrows : Choice -> Image -> Image
;; ... place highlighted arrows to the left and right (pointing that way too) 
(define (draw-highlighted-arrows c img)
  (draw-arrows c j1_selected j2_selected img))

;; draw-regular-arrows : Choice -> Image -> Image
;; ... place regular arrows to the left and right (pointing that way too)
(define (draw-regular-arrows c img)
  (draw-arrows c j1 j2 img))


;; draw-world : World -> Image
(define (draw-world w)
  (match w
    ((world idx choices phase)
     (car
      (foldr
       (λ (c pr_img_indx)
         (if (= (cdr pr_img_indx) idx)
             (cons (draw-highlighted-arrows c (car pr_img_indx))
                   idx)
             (cons (draw-regular-arrows c (car pr_img_indx))
                   (add1 idx))))
       ;; first draw the choices, then draw the arrows 
       (cons ((draw-choices choices) phase)
             0)
       choices)))))

;; use Arrow and draw-world
(big-bang (world 0 (list choice1 choice2) not-quacking)
          [to-draw draw-world]
          [on-key key-handler])

#|
when editing, have one line of the choices be the one being modified, and when we draw
all the choices, that line will get an extra drawing indicating that it is the choice 
the user is modifying


;; editmode vs interact mode
|#


