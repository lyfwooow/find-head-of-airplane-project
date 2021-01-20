#lang racket

(require "dd.rkt" "convert.rkt" "checkers.rkt" "update-valid.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction)
(require rackunit)
(provide (all-defined-out))

; count-list-of-airplane : [Listof Board-POSN] -> [Listof [Listof Board-POSN]]
; return a list of airplane body posns
(define (count-list-of-airplane posns)
  (cond
    [(empty? posns) '()]
    [(and (< (posn-x (first posns)) 5)
          (> (posn-x (first posns)) 1))  
     (cond
       [(and (< (posn-y (first posns)) 4)
             (>= (posn-y (first posns)) 0))
        (cons (construct-up-airplane (first posns))
              (count-list-of-airplane (rest posns)))]
       [(and (<= (posn-y (first posns)) 6)
             (> (posn-y (first posns)) 2))
        (cons (construct-down-airplane (first posns))
              (count-list-of-airplane (rest posns)))])]

    [(and (< (posn-y (first posns)) 5)
          (> (posn-y (first posns)) 1))
     (cond
       [(and (< (posn-x (first posns)) 4)
             (>= (posn-x (first posns)) 0))
        (cons (construct-left-airplane (first posns))
              (count-list-of-airplane (rest posns)))]
       [(and (<= (posn-x (first posns)) 6)
             (> (posn-x (first posns)) 2))
        (cons (construct-right-airplane (first posns))
              (count-list-of-airplane (rest posns)))])] 

    [else
     (count-list-of-airplane (rest posns))]))




;; construct-up-airplane : head -> [Listof Board-Posn]
;; to construct a list of body of an UP airplane
(define (construct-up-airplane head)
  (list (make-posn (- (posn-x head) 2) (+ (posn-y head) 1))
        (make-posn (- (posn-x head) 1) (+ (posn-y head) 1))
        (make-posn (posn-x head) (+ (posn-y head) 1))
        (make-posn (+ (posn-x head) 1) (+ (posn-y head) 1))
        (make-posn (+ (posn-x head) 2) (+ (posn-y head) 1))  ;;wing
        (make-posn (posn-x head) (+ (posn-y head) 2))        ;;body
        (make-posn (- (posn-x head) 1) (+ (posn-y head) 3))
        (make-posn (posn-x head) (+ (posn-y head) 3))
        (make-posn (+ (posn-x head) 1) (+ (posn-y head) 3))));;tail

;; construct-down-airplane : head -> [Listof Board-Posn]
;; to construct a list of body of an DOWN airplane
(define (construct-down-airplane head)
  (list (make-posn (- (posn-x head) 2) (- (posn-y head) 1))
        (make-posn (- (posn-x head) 1) (- (posn-y head) 1))
        (make-posn (posn-x head) (- (posn-y head) 1))
        (make-posn (+ (posn-x head) 1) (- (posn-y head) 1))
        (make-posn (+ (posn-x head) 2) (- (posn-y head) 1))  ;;wing
        (make-posn (posn-x head) (- (posn-y head) 2))        ;;body
        (make-posn (- (posn-x head) 1) (- (posn-y head) 3))
        (make-posn (posn-x head) (- (posn-y head) 3))
        (make-posn (+ (posn-x head) 1) (- (posn-y head) 3))));;tail

;; construct-left-airplane : head -> [Listof Board-Posn]
;; to construct a list of body of an LEFT airplane
(define (construct-left-airplane head)
  (list (make-posn (+ (posn-x head) 1) (- (posn-y head) 2))
        (make-posn (+ (posn-x head) 1) (- (posn-y head) 1))
        (make-posn (+ (posn-x head) 1) (posn-y head))
        (make-posn (+ (posn-x head) 1) (+ (posn-y head) 1))
        (make-posn (+ (posn-x head) 1) (+ (posn-y head) 2))  ;;wing
        (make-posn (+ (posn-x head) 2) (posn-y head))        ;;body
        (make-posn (+ (posn-x head) 3) (- (posn-y head) 1))
        (make-posn (+ (posn-x head) 3) (posn-y head))
        (make-posn (+ (posn-x head) 3) (+ (posn-y head) 1))));;tail


;; construct-right-airplane : head -> [Listof Board-Posn]
;; to construct a list of body of an LEFT airplane
(define (construct-right-airplane head)
  (list (make-posn (- (posn-x head) 1) (- (posn-y head) 2))
        (make-posn (- (posn-x head) 1) (- (posn-y head) 1))
        (make-posn (- (posn-x head) 1) (posn-y head))
        (make-posn (- (posn-x head) 1) (+ (posn-y head) 1))
        (make-posn (- (posn-x head) 1) (+ (posn-y head) 2))  ;;wing
        (make-posn (- (posn-x head) 2) (posn-y head))        ;;body
        (make-posn (- (posn-x head) 3) (- (posn-y head) 1))
        (make-posn (- (posn-x head) 3) (posn-y head))
        (make-posn (- (posn-x head) 3) (+ (posn-y head) 1))));;tail

(check-equal? (count-list-of-airplane POSN-OF-MAP)
              (list
               (list (posn 1 0) (posn 1 1) (posn 1 2) (posn 1 3) (posn 1 4) (posn 2 2) (posn 3 1) (posn 3 2) (posn 3 3))
               (list (posn 1 1) (posn 1 2) (posn 1 3) (posn 1 4) (posn 1 5) (posn 2 3) (posn 3 2) (posn 3 3) (posn 3 4))
               (list (posn 1 2) (posn 1 3) (posn 1 4) (posn 1 5) (posn 1 6) (posn 2 4) (posn 3 3) (posn 3 4) (posn 3 5))
               (list (posn 2 0) (posn 2 1) (posn 2 2) (posn 2 3) (posn 2 4) (posn 3 2) (posn 4 1) (posn 4 2) (posn 4 3))
               (list (posn 2 1) (posn 2 2) (posn 2 3) (posn 2 4) (posn 2 5) (posn 3 3) (posn 4 2) (posn 4 3) (posn 4 4))
               (list (posn 2 2) (posn 2 3) (posn 2 4) (posn 2 5) (posn 2 6) (posn 3 4) (posn 4 3) (posn 4 4) (posn 4 5))
               (list (posn 0 1) (posn 1 1) (posn 2 1) (posn 3 1) (posn 4 1) (posn 2 2) (posn 1 3) (posn 2 3) (posn 3 3))
               (list (posn 0 2) (posn 1 2) (posn 2 2) (posn 3 2) (posn 4 2) (posn 2 3) (posn 1 4) (posn 2 4) (posn 3 4))
               (list (posn 0 3) (posn 1 3) (posn 2 3) (posn 3 3) (posn 4 3) (posn 2 4) (posn 1 5) (posn 2 5) (posn 3 5))
               (list (posn 0 4) (posn 1 4) (posn 2 4) (posn 3 4) (posn 4 4) (posn 2 5) (posn 1 6) (posn 2 6) (posn 3 6))
               (list (posn 0 3) (posn 1 3) (posn 2 3) (posn 3 3) (posn 4 3) (posn 2 2) (posn 1 1) (posn 2 1) (posn 3 1))
               (list (posn 0 4) (posn 1 4) (posn 2 4) (posn 3 4) (posn 4 4) (posn 2 3) (posn 1 2) (posn 2 2) (posn 3 2))
               (list (posn 0 5) (posn 1 5) (posn 2 5) (posn 3 5) (posn 4 5) (posn 2 4) (posn 1 3) (posn 2 3) (posn 3 3))
               (list (posn 1 1) (posn 2 1) (posn 3 1) (posn 4 1) (posn 5 1) (posn 3 2) (posn 2 3) (posn 3 3) (posn 4 3))
               (list (posn 1 2) (posn 2 2) (posn 3 2) (posn 4 2) (posn 5 2) (posn 3 3) (posn 2 4) (posn 3 4) (posn 4 4))
               (list (posn 1 3) (posn 2 3) (posn 3 3) (posn 4 3) (posn 5 3) (posn 3 4) (posn 2 5) (posn 3 5) (posn 4 5))
               (list (posn 1 4) (posn 2 4) (posn 3 4) (posn 4 4) (posn 5 4) (posn 3 5) (posn 2 6) (posn 3 6) (posn 4 6))
               (list (posn 1 3) (posn 2 3) (posn 3 3) (posn 4 3) (posn 5 3) (posn 3 2) (posn 2 1) (posn 3 1) (posn 4 1))
               (list (posn 1 4) (posn 2 4) (posn 3 4) (posn 4 4) (posn 5 4) (posn 3 3) (posn 2 2) (posn 3 2) (posn 4 2))
               (list (posn 1 5) (posn 2 5) (posn 3 5) (posn 4 5) (posn 5 5) (posn 3 4) (posn 2 3) (posn 3 3) (posn 4 3))
               (list (posn 2 1) (posn 3 1) (posn 4 1) (posn 5 1) (posn 6 1) (posn 4 2) (posn 3 3) (posn 4 3) (posn 5 3))
               (list (posn 2 2) (posn 3 2) (posn 4 2) (posn 5 2) (posn 6 2) (posn 4 3) (posn 3 4) (posn 4 4) (posn 5 4))
               (list (posn 2 3) (posn 3 3) (posn 4 3) (posn 5 3) (posn 6 3) (posn 4 4) (posn 3 5) (posn 4 5) (posn 5 5))
               (list (posn 2 4) (posn 3 4) (posn 4 4) (posn 5 4) (posn 6 4) (posn 4 5) (posn 3 6) (posn 4 6) (posn 5 6))
               (list (posn 2 3) (posn 3 3) (posn 4 3) (posn 5 3) (posn 6 3) (posn 4 2) (posn 3 1) (posn 4 1) (posn 5 1))
               (list (posn 2 4) (posn 3 4) (posn 4 4) (posn 5 4) (posn 6 4) (posn 4 3) (posn 3 2) (posn 4 2) (posn 5 2))
               (list (posn 2 5) (posn 3 5) (posn 4 5) (posn 5 5) (posn 6 5) (posn 4 4) (posn 3 3) (posn 4 3) (posn 5 3))
               (list (posn 4 0) (posn 4 1) (posn 4 2) (posn 4 3) (posn 4 4) (posn 3 2) (posn 2 1) (posn 2 2) (posn 2 3))
               (list (posn 4 1) (posn 4 2) (posn 4 3) (posn 4 4) (posn 4 5) (posn 3 3) (posn 2 2) (posn 2 3) (posn 2 4))
               (list (posn 4 2) (posn 4 3) (posn 4 4) (posn 4 5) (posn 4 6) (posn 3 4) (posn 2 3) (posn 2 4) (posn 2 5))
               (list (posn 5 0) (posn 5 1) (posn 5 2) (posn 5 3) (posn 5 4) (posn 4 2) (posn 3 1) (posn 3 2) (posn 3 3))
               (list (posn 5 1) (posn 5 2) (posn 5 3) (posn 5 4) (posn 5 5) (posn 4 3) (posn 3 2) (posn 3 3) (posn 3 4))
               (list (posn 5 2) (posn 5 3) (posn 5 4) (posn 5 5) (posn 5 6) (posn 4 4) (posn 3 3) (posn 3 4) (posn 3 5))))
