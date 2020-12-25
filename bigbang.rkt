#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; Each choice is a type (enum) and maps to a total function on that type 

;; A Color is one of : "red" | "orange" | "yellow" | "green" | "blue" | "purple"
;; A Shape is one of : "circle" | "triangle" | "rectangle"

(define Amount-Choices 2)
(define ChoiceType-Color "Color")
(define ChoiceType-Shape "Shape")

;; A Choice-Type is one of Color | Shape
(define choice-types (list ChoiceType-Color ChoiceType-Shape))

(define-syntax (inverse stx)
  (syntax-rules stx
    ((_ (match-lambda [a b] ...))
     (match-lambda [b a] ...))))

;; update-color : Color -> Color
(define update-color
  (match-lambda
    ["red" "orange"]
    ["orange" "yellow"]
    ["yellow" "green"]
    ["green" "blue"]
    ["blue" "purple"]
    ["purple" "white"]
    ["white" "black"]
    ["black" "red"]
    [x (error 'update-color "unknown color: ~s" x)]))

(define cycle-color-left
  (inverse (match-lambda
    ["red" "orange"]
    ["orange" "yellow"]
    ["yellow" "green"]
    ["green" "blue"]
    ["blue" "purple"]
    ["purple" "white"]
    ["white" "black"]
    ["black" "red"]
    [x (error 'update-color "unknown color: ~s" x)])))

(define cycle-color-right update-color)

(define update-shape
  (match-lambda
    ["triangle" "circle"]
    ["circle" "square"]
    ["square" "triangle"]))

(define cycle-shape-left update-shape)
(define cycle-shape-right
  (inverse
   (match-lambda
     ["triangle" "circle"]
     ["circle" "square"]
     ["square" "triangle"])))

;; A choice has 2 phases (think: not quacking vs. quacking)

;; update-phase : Phase -> Phase
;; flips phase (go from quacking to not, vice versa)
(define update-phase not)

;; A Phase is a Boolean
(define quacking #t)
(define not-quacking #f)

;; A Choice is a (choice Choice-Type Choice-Value Phase)
;; If Choice-Type is "Color", Choice-Value is one of "red" | "orange" | ...
;; If Choice-Type is "Shape", Choice-Value is one of "circle" | "triangle" | ...
(struct choice [type value] #:transparent)

(define choice1 (choice ChoiceType-Color "red"))
(define choice2 (choice ChoiceType-Shape "circle"))

(define ex-choices (list choice1 choice2))

;; An index is an integer mod Amount-Choices

;; A World is a (World Index [List Choice])
(struct world [idx choices phase] ;; world : [List Choice] Phase -> World
  #:transparent)

;; ChoiceMap is a [List [Pair ChoiceType [List [ChoiceValue -> ChoiceValue] [ChoiceValue -> ChoiceValue]]]]
(define ChoiceMap
  (list (cons "Color" (list  cycle-color-left cycle-color-right))
        (cons "Shape" (list  cycle-shape-left cycle-shape-right))))

;; update-choice-value : Choice [ChoiceValue -> ChoiceValue] -> choice
(define (update-choice-value c f)
  (match c
    [(choice type val) (choice type (f val))]))

(check-equal? (update-choice-value (choice "Color" "red") cycle-color-right)
              (choice "Color" "orange"))
(check-equal? (update-choice-value (choice "Color" "red") cycle-color-left)
              (choice "Color" "black"))

;; cycle-choice : Choice -> [ChoiceValue -> ChoiceValue] -> Choice
(define cycle-choice
  (λ (c f)
    (match c
      ((choice type value)
       (match (assv type ChoiceMap)
             [#f (error 'cycle-choice-right "couldn't find choice ~s in map" c)]
             [mpr  (update-choice-value c (f mpr) )])))))


;; cycle-choice-left : Choice -> Choice
(define cycle-choice-left
  (λ (c)
    (cycle-choice c
                  (match-lambda
                    [(cons name (list left-cycle right-cycle)) left-cycle]
                    [else (error 'cycle-choice-left "couldnt match on the mapping for the type in the ChoiceMap: choice was ~s" c)]))))

;; cycle-choice-right : Choice -> Choice
(define cycle-choice-right
  (λ (c)
    (cycle-choice c
                  (match-lambda
                    [(cons name (list left-cycle right-cycle)) right-cycle]
                    [else (error 'cycle-choice-right "couldnt match on the mapping for the type in the ChoiceMap: choice was ~s" c)]))))

;; cycle-left : World -> World
(define (cycle-left w) (cycle w cycle-choice-left))
;; cycle-right : World -> World
(define (cycle-left w) (cycle w cycle-choice-right))

;; cycle : World [Index -> Choice -> Choice] -> World
(define (cycle w f)
  (match w
    ((world idx choices phase)
     (world idx
            (for [(c choices) (idx2 0)]
              (if (= idx2 idx)
                  (f c)
                  c))
            phase)))


(define (move-up w)
  (match w
    [(world idx choices phase)
     (world (mod (sub1 idx) Amount-Choices)
            choices
            phase)]))

(check-equal? (move-up (world 0 ex-choices #f))
              (world 1 ex-choices #f))
(check-equal? (move-up (world 1 ex-choices #f))
              (world 0 ex-choices #f))

(define (move-down w)
  (match w
    [(world idx choices phase)
     (world (mod (add1 idx) Amount-Choices)
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
    ["d" (cycle-right w)]))


(big-bang (world (list choice1 choice2) not-quacking)
 [to-draw draw-world]
 [on-key key-handler])

;; we want Draw choice for each phase
;; we want cycle choice, keep phase 

#|
when editing, have one line of the choices be the one being modified, and when we draw
all the choices, that line will get an extra drawing indicating that it is the choice 
the user is modifying

WASD 
W - move up choices
S - move down choices
A - cycle choice left
D - cycle choice right

|#

;; editmode vs interact mode

