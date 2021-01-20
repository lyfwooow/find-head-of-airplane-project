#lang racket

(require "dd.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction)
(require rackunit)
(provide (all-defined-out))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list1 (list 2 2 3 0
                    2 2 0 3
                    3 0 2 2
                    0 3 2 2))

(define list2 (list 4 0 2 1
                    1 4 0 2
                    2 1 0 4
                    1 2 4 0))

(define list3 (list 3 1 2 1
                    1 3 1 2
                    2 1 1 3
                    1 2 3 1))

(define list4 (list 2 2 2 1
                    2 2 1 2
                    2 1 2 2
                    1 2 2 2))

(define list5 (list 1 3 2 1
                    3 1 1 2
                    2 1 3 1
                    1 2 1 3))

(define list6 (list 0 4 2 1
                    4 0 1 2
                    2 1 4 0
                    1 2 0 4))

(define list7 (list 2 2 1 2
                    2 2 2 1
                    1 2 2 2
                    2 1 2 2))

(define list8 (list 3 1 0 3
                    1 3 3 0
                    0 3 1 3
                    3 0 3 1))

(define list9 (list 2 2 0 3
                    2 2 3 0
                    0 3 2 2
                    3 0 2 2))

(define list10 (list 1 3 0 3
                     3 1 3 0
                     0 3 3 1
                     3 0 1 3))


; counting-test : Board-POSN -> Number
; count how many airplanes the given posn could have
; strategy : functional composition
(define (counting-test pom)
  (+ (valid list1 pom 0)
     (valid list2 pom 0)
     (valid list3 pom 0)
     (valid list4 pom 0)
     (valid list5 pom 0)
     (valid list6 pom 0) 
     (valid list7 pom 0)
     (valid list8 pom 0)
     (valid list9 pom 0)
     (valid list10 pom 0)))

;;valid : [Listof Number] Board-POSN Number -> Number
;; count how many possibile airplane this block could hide
(define (valid list-of-border posn count)
  (cond
    [(empty? list-of-border) count]
    [(and (<= (+ (posn-x posn) (list-ref list-of-border 0)) BORDER-DOWN)
          (>= (- (posn-x posn) (list-ref list-of-border 1)) BORDER-TOP)
          (<= (+ (posn-y posn) (list-ref list-of-border 2)) BORDER-DOWN)
          (>= (- (posn-y posn) (list-ref list-of-border 3)) BORDER-TOP)) 
     (valid (take-right list-of-border (- (length list-of-border) 4))  posn (+ count 1))]
    [(not (and (<= (+ (posn-x posn) (list-ref list-of-border 0)) BORDER-DOWN)
               (>= (- (posn-x posn) (list-ref list-of-border 1)) BORDER-TOP)
               (<= (+ (posn-y posn) (list-ref list-of-border 2)) BORDER-DOWN)
               (>= (- (posn-y posn) (list-ref list-of-border 3)) BORDER-TOP)))
     (valid (take-right list-of-border (- (length list-of-border) 4)) posn count)]
    ))

(check-equal? (valid list5 (make-posn 3 4) 0)
              3)

(check-equal? (valid list7 (make-posn 0 0) 0)
              0)

(check-equal? (valid list1 (make-posn 1 1) 0)
              0)

(check-equal? (valid list1 (make-posn -10 -10) 0)
              0)

(check-equal? (counting-test (make-posn 3 4)) 26)
