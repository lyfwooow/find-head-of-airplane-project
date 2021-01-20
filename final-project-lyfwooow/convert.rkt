#lang racket

(require "dd.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction)
(require rackunit)
(provide (all-defined-out))

; convert-screen-posn : Screen-POSN -> Board-POSN or #false 
; conver the pixels to Board-POSN
(define (convert-screen-posn sp)
  (cond
    [(in-bound? sp)
     (make-posn (- (ceiling (/ (- (posn-x sp) 10) 40)) 1) (- (ceiling (/ (- (posn-y sp) 10) 40)) 1))]
    [else
     #f]))

;in-bound? : Screen-POSN -> Bool
;check if the given screen posn is inside the map
(define (in-bound? sp)
  (cond
    [(and (> (posn-x sp) MAP-MIN-POSN)
          (< (posn-x sp) MAP-MAX-POSN)
          (> (posn-y sp) MAP-MIN-POSN)
          (< (posn-y sp) MAP-MAX-POSN))
     #t]
    [else
     #f]))

; convert-board-posn : Board-POSN -> Screen-POSN
; convert the board posn to screen posn
(define (convert-board-posn bp)
  (make-posn (+ (* 40 (posn-x bp)) 30) (+ (* 40 (posn-y bp)) 30)))

(check-equal? (convert-screen-posn (make-posn 1 1))
              #f)

(check-equal? (convert-screen-posn (make-posn 95 15))
              (make-posn 2 0))
(check-equal? (convert-screen-posn (make-posn 131 131))
              (make-posn 3 3))

(check-equal? (convert-board-posn (make-posn 2 1))
              (make-posn 110 70))
(check-equal? (convert-board-posn (make-posn 5 5))
              (make-posn 230 230))