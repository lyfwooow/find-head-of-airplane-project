#lang racket


(require "dd.rkt" "checkers.rkt" "convert.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction)
(require rackunit)
(provide (all-defined-out))


; mouse : Airplane-map Screen-POSN Screen-POSN Mouse-Event -> Airplane-map
; takes in the position and mouse-event of mouse, returns an updated airplane map
(define (mouse am x y mouse-event)
  (cond
    [(string=? mouse-event "button-up")
     (reveal-block am x y)]
    [else
     am]))


; revealed-block : Airplane-map Screen-POSN Screen-POSN -> Airplane-map
; takes in the position of mouse, returns an updated airplane map
(define (reveal-block am x y)
  (cond
    [(revealed-sp? (am-board am) x y)
     am]
    [else
     (cond
       [(and (find-body-sp? (am-board am) x y)
             (> (am-ra am) 0))
        (make-am (make-board (board-head (am-board am))
                             (board-body (am-board am))
                             (cons (convert-screen-posn (make-posn x y))
                                           (board-revealed (am-board am))))
                 (am-mode am)
                 (am-timer am)
                 (- (am-ra am) 1)
                 (am-wol am))]
       [(find-head-sp? (am-board am) x y)
        (make-am (make-board (board-head (am-board am))
                             (board-body (am-board am))
                             (cons (convert-screen-posn (make-posn x y))
                                           (board-revealed (am-board am))))
                 (am-mode am)
                 (am-timer am)
                 (- (am-ra am) 1)
                 (make-wol #t 10))]
       [(and (<= (am-ra am) 0)
             (not (find-head-sp? (am-board am) x y)))
        (make-am (make-board (board-head (am-board am))
                             (board-body (am-board am))
                             (board-revealed (am-board am)))
                 (am-mode am)
                 (am-timer am)
                 (am-ra am)
                 (make-wol #f 10))]
       [(not (in-bound? (make-posn x y)))
        (make-am (make-board (board-head (am-board am))
                             (board-body (am-board am))
                             (board-revealed (am-board am)))
                 (am-mode am)
                 (am-timer am)
                 (am-ra am)
                 (am-wol am))]
       [else
        (make-am (make-board (board-head (am-board am))
                             (board-body (am-board am))
                             (cons (convert-screen-posn (make-posn x y))
                                           (board-revealed (am-board am))))
                 (am-mode am)
                 (am-timer am)
                 (- (am-ra am) 1)
                 (am-wol am))]
       )]))




(equal-posn? (first (board-revealed (am-board (mouse (make-am (make-board (make-posn 3 3)
                                            (list (make-posn 1 4)
                                                  (make-posn 2 4)
                                                  (make-posn 3 4)
                                                  (make-posn 4 4)
                                                  (make-posn 5 4)
                                                  (make-posn 3 5)
                                                  (make-posn 2 6)
                                                  (make-posn 3 6)
                                                  (make-posn 4 6))
                                            (list (make-posn 2 3)
                                                  (make-posn 2 4)))
                                "normal"
                                0
                                8
                                "underway")
                       95
                       95
                       "button-up"))))
               (make-posn 2 2))

(check-equal? (am-ra (mouse (make-am (make-board (make-posn 3 3)
                                            (list (make-posn 1 4)
                                                  (make-posn 2 4)
                                                  (make-posn 3 4)
                                                  (make-posn 4 4)
                                                  (make-posn 5 4)
                                                  (make-posn 3 5)
                                                  (make-posn 2 6)
                                                  (make-posn 3 6)
                                                  (make-posn 4 6))
                                            (list (make-posn 2 3)
                                                  (make-posn 2 4)))
                                "normal"
                                0
                                8
                                "underway")
                       95
                       95
                       "button-up")) 7)



(equal-posn? (first (board-revealed (am-board (reveal-block (make-am (make-board (make-posn 3 3)
                                                                                 (list (make-posn 1 4)
                                                                                       (make-posn 2 4)
                                                                                       (make-posn 3 4)
                                                                                       (make-posn 4 4)
                                                                                       (make-posn 5 4)
                                                                                       (make-posn 3 5)
                                                                                       (make-posn 2 6)
                                                                                       (make-posn 3 6)
                                                                                       (make-posn 4 6))
                                                                                 (list (make-posn 2 3)
                                                                                       (make-posn 2 4)))
                                                                     "normal"
                                                                     0
                                                                     8
                                                                     "underway")
                                                            105
                                                            95))))
             (make-posn 2 2))

(check-equal? (am-ra (reveal-block (make-am (make-board (make-posn 3 3)
                                                        (list (make-posn 1 4)
                                                              (make-posn 2 4)
                                                              (make-posn 3 4)
                                                              (make-posn 4 4)
                                                              (make-posn 5 4)
                                                              (make-posn 3 5)
                                                              (make-posn 2 6)
                                                              (make-posn 3 6)
                                                              (make-posn 4 6))
                                                        (list (make-posn 2 3)
                                                              (make-posn 2 4)))
                                            "normal"
                                            0
                                            8
                                            "underway")
                                   105
                                   95))
              7)

(equal-posn? (first (board-revealed (am-board (mouse (make-am (make-board (make-posn 3 3)
                                            (list (make-posn 1 4)
                                                  (make-posn 2 4)
                                                  (make-posn 3 4)
                                                  (make-posn 4 4)
                                                  (make-posn 5 4)
                                                  (make-posn 3 5)
                                                  (make-posn 2 6)
                                                  (make-posn 3 6)
                                                  (make-posn 4 6))
                                            (list (make-posn 2 3)
                                                  (make-posn 2 4)))
                                "normal"
                                0
                                8
                                "underway")
                       95
                       95
                       "button-down"))))
               (make-posn 2 3))

(equal-posn? (first (board-revealed (am-board (reveal-block (make-am (make-board (make-posn 3 3)
                                                                                 (list (make-posn 1 4)
                                                                                       (make-posn 2 4)
                                                                                       (make-posn 3 4)
                                                                                       (make-posn 4 4)
                                                                                       (make-posn 5 4)
                                                                                       (make-posn 3 5)
                                                                                       (make-posn 2 6)
                                                                                       (make-posn 3 6)
                                                                                       (make-posn 4 6))
                                                                                 (list (make-posn 2 2)
                                                                                       (make-posn 2 4)))
                                                                     "normal"
                                                                     0
                                                                     8
                                                                     "underway")
                                                            105
                                                            95))))
             (make-posn 2 2))

(equal-posn? (first (board-revealed (am-board (reveal-block (make-am (make-board (make-posn 3 3)
                                                                                 (list (make-posn 1 4)
                                                                                       (make-posn 2 4)
                                                                                       (make-posn 3 4)
                                                                                       (make-posn 4 4)
                                                                                       (make-posn 5 4)
                                                                                       (make-posn 3 5)
                                                                                       (make-posn 2 6)
                                                                                       (make-posn 3 6)
                                                                                       (make-posn 4 6))
                                                                                 (list (make-posn 2 2)
                                                                                       (make-posn 2 4)))
                                                                     "normal"
                                                                     0
                                                                     8
                                                                     "underway")
                                                            150
                                                            190))))
             (make-posn 3 4))

(equal-posn? (first (board-revealed (am-board (reveal-block (make-am (make-board (make-posn 3 3)
                                                                                 (list (make-posn 1 4)
                                                                                       (make-posn 2 4)
                                                                                       (make-posn 3 4)
                                                                                       (make-posn 4 4)
                                                                                       (make-posn 5 4)
                                                                                       (make-posn 3 5)
                                                                                       (make-posn 2 6)
                                                                                       (make-posn 3 6)
                                                                                       (make-posn 4 6))
                                                                                 (list (make-posn 2 2)
                                                                                       (make-posn 2 4)))
                                                                     "normal"
                                                                     0
                                                                     8
                                                                     "underway")
                                                            150
                                                            150))))
             (make-posn 3 3))

(equal-posn? (first (board-revealed (am-board (reveal-block (make-am (make-board (make-posn 3 3)
                                                                                 (list (make-posn 1 4)
                                                                                       (make-posn 2 4)
                                                                                       (make-posn 3 4)
                                                                                       (make-posn 4 4)
                                                                                       (make-posn 5 4)
                                                                                       (make-posn 3 5)
                                                                                       (make-posn 2 6)
                                                                                       (make-posn 3 6)
                                                                                       (make-posn 4 6))
                                                                                 (list (make-posn 2 2)
                                                                                       (make-posn 2 4)))
                                                                     "normal"
                                                                     0
                                                                     0
                                                                     "underway")
                                                            30
                                                            30))))
             (make-posn 0 0))

(equal-posn? (first (board-revealed (am-board (reveal-block (make-am (make-board (make-posn 3 3)
                                                                                 (list (make-posn 1 4)
                                                                                       (make-posn 2 4)
                                                                                       (make-posn 3 4)
                                                                                       (make-posn 4 4)
                                                                                       (make-posn 5 4)
                                                                                       (make-posn 3 5)
                                                                                       (make-posn 2 6)
                                                                                       (make-posn 3 6)
                                                                                       (make-posn 4 6))
                                                                                 (list (make-posn 2 2)
                                                                                       (make-posn 2 4)))
                                                                     "normal"
                                                                     0
                                                                     8
                                                                     "underway")
                                                            2
                                                            3))))
             #f)




