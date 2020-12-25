#lang racket

(require "choice_struct.rkt")
(provide (all-defined-out))
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)


;; cycle-choice : Choice -> [ChoiceValue -> ChoiceValue] -> Choice
(define cycle-choice
  (λ (c f)
    (match c
      ((choice type value)
       (match (assv type ChoiceMap)
         [#f (error 'cycle-choice-right "couldn't find choice ~s in map" c)]
         [mpr  (update-choice-value c (f mpr) )])))))



(define cycle-choice-*
  (λ (*_CYCLE sym)
    (λ (c)
      (cycle-choice
       c
       (match-lambda
         [(cons name (list left-cycle right-cycle)) (*_CYCLE left-cycle right-cycle)]
         [else
          (error sym "couldnt match on the mapping for the type in the ChoiceMap: choice was ~s"
                 c)])
       ))))

;; cycle-choice-left : Choice -> Choice
(define cycle-choice-left (cycle-choice-* (λ (a b) a) 'cycle-choice-left))

;; cycle-choice-right : Choice -> Choice
(define cycle-choice-right (cycle-choice-* (λ (a b) b) 'cycle-choce-right))


;; cycle-left : World -> World
(define (cycle-left w) (cycle w cycle-choice-left))
;; cycle-right : World -> World
(define (cycle-right w) (cycle w cycle-choice-right))

(define (make-cycle-error index choices idx)
  (error 'cycle-inner-loop
         "index ~s shouldn't go beyond boundary of choices ~s;\n we expected to find the choice at index ~s"
         index
         choices
         idx))

;; cycle : World [Index -> Choice -> Choice] -> World
(define (cycle w f)
  (define loop
    (λ (idx index choices)
      (cond
        [(> index (length choices))
         (make-cycle-error index choices idx)]
        [(= idx index)
         ;; apply the update function, don't look any further
         (cons (f (car choices)) (cdr choices))]
        [else (cons (car choices)
                    ;; leave choice as is, loop 
                    (loop idx (add1 index) (cdr choices)))])))
  (match w
    ((world idx choices phase)
     (world idx (loop idx 0 choices) phase))))


;;;
;;;  Cycling functions for the Color and Shape ChoiceTypes
;;;

;; macro that makes an inverse function
(define-syntax inverse
  (syntax-rules (match-lambda)
    ((_ (match-lambda [x err]))
     (match-lambda [x err]))
    ((_ (match-lambda [a b] ...))
     (match-lambda [b a] ...))))

;; cycle-*type-{left,right} : ChoiceType -> ChoiceType
(define cycle-color-left
  (inverse
   (match-lambda
    ["red" "orange"]
    ["orange" "yellow"]
    ["yellow" "green"]
    ["green" "blue"]
    ["blue" "purple"]
    ["purple" "white"]
    ["white" "black"]
    ["black" "red"]
    #|[x (error 'update-color "unknown color: ~s" x)]|#)))

(define cycle-color-right
  (match-lambda
    ["red" "orange"]
    ["orange" "yellow"]
    ["yellow" "green"]
    ["green" "blue"]
    ["blue" "purple"]
    ["purple" "white"]
    ["white" "black"]
    ["black" "red"]
    #|[x (error 'update-color "unknown color: ~s" x)]|#))

(define cycle-shape-left
  (match-lambda
    ["triangle" "circle"]
    ["circle" "square"]
    ["square" "triangle"]))

(define cycle-shape-right
  (inverse
   (match-lambda
     ["triangle" "circle"]
     ["circle" "square"]
     ["square" "triangle"])))

;; ChoiceMap is a [List [Pair ChoiceType [List [ChoiceValue -> ChoiceValue] [ChoiceValue -> ChoiceValue]]]]
(define ChoiceMap
  (list (cons "Color" (list  cycle-color-left cycle-color-right))
        (cons "Shape" (list  cycle-shape-left cycle-shape-right))))


(check-equal? (update-choice-value (choice "Color" "red") cycle-color-right)
              (choice "Color" "orange"))
(check-equal? (update-choice-value (choice "Color" "red") cycle-color-left)
              (choice "Color" "black"))

(check-equal? (cycle-choice-left (choice "Color" "red"))
              (choice "Color" "black"))
(check-equal? (cycle-choice-left (choice "Shape" "circle"))
              (choice "Shape" "square"))

(check-equal? (cycle-choice-right (choice "Shape" "circle"))
              (choice "Shape" "triangle"))
(check-equal? (cycle-choice-right (choice "Color" "red"))
              (choice "Color" "orange"))

(check-equal? (cycle (world 0 ex-choices #f) cycle-choice-left)
              (world 0
                     (list (choice ChoiceType-Color "black")
                           choice2)
                     #f)) 

(check-equal? (cycle (world 0 ex-choices #f) cycle-choice-right)
              (world 0
                     (list (choice ChoiceType-Color "orange")
                           choice2)
                     #f))

(check-equal? (cycle (world 1 ex-choices #f) cycle-choice-left)
              (world 1
                     (list choice1
                           (choice ChoiceType-Shape "square"))
                     #f))

(check-equal? (cycle (world 1 ex-choices #f) cycle-choice-right)
              (world 1
                     (list choice1
                           (choice ChoiceType-Shape "triangle"))
                     #f))

