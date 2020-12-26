#lang racket

(require "choice_struct.rkt")
(require "cycle_functions.rkt")
(require "cc-image.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)

#;
(struct MyImage
  [shape color fill])

(define (move w f)
  (match w
    [(world idx choices phase)
     (world (f idx) 
            choices
            phase)]))

(define (move-up w)
  (move w (λ (idx) (modulo (sub1 idx) Amount-Choices))))

(check-equal? (move-up (world 0 ex-choices #f))
              (world 1 ex-choices #f))
(check-equal? (move-up (world 1 ex-choices #f))
              (world 0 ex-choices #f))

(define (move-down w)
  (move w (λ (idx) (modulo (add1 idx) Amount-Choices))))

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

(define SelectedLeftArrowImage ((ARROW 0.6 50) "left" '("plum" "orange" "yellow")))
(define SelectedRightArrowImage ((ARROW 0.6 50) "right" '("plum" "orange" "yellow")))
(define LeftArrowImage ((ARROW 0.6 50) "left" '("plum")))
(define RightArrowImage ((ARROW 0.6 50) "right" '("plum")))


(define SpaceBetweenArrows 110)
(define SpaceBetweenTypes 60)

;; make-type-image : Choice -> Image
;; use the constants SpaceBetweenArrows and SpaceBetweenTypes to make a white canvas
;; to draw the text for the given type on
(define (make-type-image c)
  (let ([BaseImageForTypeImage
         (rectangle SpaceBetweenArrows SpaceBetweenTypes "solid" "white")])
    (overlay (text (choice-type c) 24 "Goldenrod")
             BaseImageForTypeImage)))

;; draw-arrows : Choice Image Image -> Image 
(define (draw-arrows c LeftArrow RightArrow phase)
  (beside LeftArrow
          ;; put the type name, e.g. "Color" on a fixed-size rectangle
          (make-type-image c)
          RightArrow))

;; draw-highlighted-arrows : Choice -> Image
;; ... place highlighted arrows to the left and right (pointing that way too) 
(define (draw-highlighted-arrows c phase)
  (draw-arrows c SelectedLeftArrowImage SelectedRightArrowImage phase))

;; draw-regular-arrows : Choice -> Image
;; ... place regular arrows to the left and right (pointing that way too)
(define (draw-regular-arrows c phase)
  (draw-arrows c LeftArrowImage RightArrowImage phase))

;; draw-world : World -> Image
(define (draw-world w)
  (match w
    ((world idx choices phase)
     (beside
      (car
       (foldr
        (λ (c pr_img_indx)
          (if (= (cdr pr_img_indx) idx)
              (cons (above (draw-highlighted-arrows c phase) (car pr_img_indx))
                    idx)
              (cons (above (draw-regular-arrows c phase) (car pr_img_indx))
                    (add1 idx))))
        ;; first draw the choices, then draw the arrows 
        (cons empty-image 0)
        choices))
      ((draw-choices choices) phase)))))


;;  <- Shape ->    |       /\
;;  <- Color ->    |      /  \
;;  ...            |     /    \
;;  .              |     ______
;;  .              |

(define SCNW 300)
(define SCNH 300)
(define MTSCN (empty-scene SCNW SCNH))
#|
(define draw-words
  (let* ([amountOfSections Amount-Choices]
         [boxHeight (/ SCNH amountOfSections)])
    (foldr (λ (i ans) ; String Image -> Image
             (let ([rec ])) (place-image
              (text i (- boxHeight 10) "cornflowerblue")
              ;; x and y
              ans
              )
             (... i ans ...))
           MTSCN
           ;; choice-types is in choice_struct.rkt, it's [List String]
           choice-types)))
|#
(define CANVAS MTSCN)

(define ColumnWidth (/ SCNW 6))

;; add-image-to-base : Image Image -> Image
;; the base is an image SCNW wide and SCNW tall, with arrows drawn in columns on the sides
;; so the middle-img should go between them
;; the columns are (SCNW/6) so we should try to lay an image SCNH tall and SCNW-2x wide
;; where x = (SCNW/6)
#;
(define (add-image-to-base middle-img base-img)
  ...)

;; make-base : Number -> Image
;; number says which pair of arrows to draw as selected
#;
(define (make-base idxOfSelected)
  (... SelectedLeftArrowImage
       SelectedRightArrowImage
       LeftArrowImage
       RightArrowImage))

;; draw one column of left arrows and one column of right arrows
;; the columns should be 1/6 of the SCNW wide and just SCNH tall
;; there should be blank space between thenm so the first column goes from [0..x]
;; where x is (SCNW/6) and the other column goes from [(SCNW-x)..SCNW]

;; x-axis on [0-100] is for text width
;; like "color" "shape", ChoiceType-Color ChoiceType-Shape, choice-types

#;
(check-equal? (draw-world (world 0 (list choice1 choice2) not-quacking))
              (add-image-to-base (circle 100 "solid" "red")
                                 (make-base 0)))

;; Remember. A World is a (world Number [List Choice] Boolean)


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


