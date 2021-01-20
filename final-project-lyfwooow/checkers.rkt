#lang racket

(require "dd.rkt" "convert.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction)
(require rackunit)
(provide (all-defined-out))

;; revealed-sp? : Board Number Number -> boolean
;; check if the given x and y is in the (board-revealed board)
(define (revealed-sp? board x y)
  (cond
    [(false? (convert-screen-posn (make-posn x y)))
     #f]
    [else
     (revealed? (board-revealed board) (convert-screen-posn (make-posn x y)))]))

; revealed? : [listof Board-POSN] Board-POSN Bool 
; check if is revealed
(define (revealed? list bp)
  (cond
    [(empty? list) #f]
    [else (or (equal-posn? (first list) bp) 
              (revealed? (rest list) bp))]))


; find-body-sp? : Board Screen-POSN Screen-POSN -> Bool  
; check if is a block of body
(define (find-body-sp? board x y)
  (cond
    [(false? (convert-screen-posn (make-posn x y)))
     #f]
    [else
     (find-body? (board-body board) (convert-screen-posn (make-posn x y)))]))

; find-head-sp? : Board Screen-POSN Screen-POSN -> Bool 
; check if is revealed
(define (find-head-sp? board x y)
  (cond
    [(false? (convert-screen-posn (make-posn x y)))
     #f] 
    [else
     (equal-posn? (board-head board) (convert-screen-posn (make-posn x y)))]))

;;equal-posn? : posn posn -> Boolean
;;check if two posns are equal or not
(define (equal-posn? a b)
  (cond
    [(or (false? a)
         (false? b))
     #f]
    [(and (= (posn-x a) (posn-x b))
          (= (posn-y a) (posn-y b)))
     #t]
    [else
     #f]))

;;find-body? : [Litsof Board-POSN] Board-POSN -> Boolean
;;To check if one of the body is found
;;Strategy: Struct Decomp
(define (find-body? body r)
  (cond
    [(empty? body) #f]
    [else (or (equal-posn? (first body) r)
              (find-body? (rest body) r))]))

(check-equal? (find-body? '() (make-posn 3 4)) #f)
(check-equal? (find-body? (list (make-posn 5 4)
                                (make-posn 3 3)
                                (make-posn 3 4))
                          (make-posn 3 4)) #t)

(check-equal? (revealed? (list (make-posn 1 4)
                               (make-posn 2 4)
                               (make-posn 3 4)
                               (make-posn 4 4)
                               (make-posn 5 4)
                               (make-posn 3 5)
                               (make-posn 2 6)
                               (make-posn 3 6)
                               (make-posn 4 6))
                         (make-posn 2 6))
              #t)

(check-equal? (revealed? '()
                         (make-posn 2 6)) 
              #f)

(check-equal? (revealed-sp? (make-board (make-posn 3 3)
                                        (list (make-posn 1 4)
                                              (make-posn 2 4)
                                              (make-posn 3 4)
                                              (make-posn 4 4)
                                              (make-posn 5 4)
                                              (make-posn 3 5)
                                              (make-posn 2 6)
                                              (make-posn 3 6)
                                              (make-posn 4 6))
                                        (list (make-posn 2 5)))
                            95
                            215)
              #t)

(check-equal? (find-body-sp? (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 2 5)))
                             95
                             255)
              #t)

(check-equal? (find-head-sp? (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 2 5)))
                             135
                             138)
              #t)



(check-equal? (equal-posn? (make-posn 3 4) (make-posn 3 4)) #t)
(check-equal? (equal-posn? (make-posn 3 5) (make-posn 2 4)) #f)
