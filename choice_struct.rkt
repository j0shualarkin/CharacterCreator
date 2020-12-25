#lang racket

(provide (all-defined-out))
(require rackunit)

;; Each choice is a type (enum) and maps to a total function on that type 

;; A Color is one of : "red" | "orange" | "yellow" | "green" | "blue" | "purple"
;; A Shape is one of : "circle" | "triangle" | "rectangle"

(define Amount-Choices 2)
(define ChoiceType-Color "Color")
(define ChoiceType-Shape "Shape")

;; A Choice-Type is one of Color | Shape
(define choice-types (list ChoiceType-Color ChoiceType-Shape))

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

;; update-choice-value : Choice [ChoiceValue -> ChoiceValue] -> choice
(define (update-choice-value c f)
  (match c
    [(choice type val) (choice type (f val))]))

(check-equal? (update-choice-value (choice "Color" "red")
                                   (match-lambda ("red" "orange") (x x)))
              (choice "Color" "orange"))
(check-equal? (update-choice-value (choice "Color" "red")
                                   (match-lambda ("red" "black") (x x)))
              (choice "Color" "black"))

