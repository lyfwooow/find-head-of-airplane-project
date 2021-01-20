
#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction) 
(require rackunit)
(require "dd.rkt" "draw.rkt" "convert.rkt" "checkers.rkt" "mouse-code.rkt" "update-valid.rkt"
         "construct.rkt")

 

#| 
specification:

1. The airplane hides under the map in random direction and position. User should only see an
   empty map with 49 white blocks at the beginning.
2. The user clicks a block to uncover it. (the block will change color)
3. Remaining attempts are displayed in the screen.
4. Remaining attempts will start at 10; every time user click a block, remaining attempts will
   decrement.
5. Game mode: There are 3 modes in the game. 
   - "normal mode": no hint will display on the map;
   - "number-mode": the possibility of each block could hide a body of airplane will display in
                   different percentage, will change when block is revealed;
   - "cheat-mode": When a block is revealed, a number of how many blue blocks are around the
                   uncovered block will display.
6. A timer will display to check how long the user spending on the game.
7. When user finds the head of the airplane, the head will explode in animation way and the body
   of airplane will display.



|#

;                                                
;                                                
;                                                
;                                                
;                                                
;                                                
;                                                
;   ;;;;;;;;                     ;               
;   ;;;;;;;;;                 ;;;;               
;   ;;;;;;;;;;                ;;;;               
;   ;;;;  ;;;;;     ;;;;;;   ;;;;;;;    ;;;;;;   
;   ;;;;   ;;;;    ;;;;;;;;  ;;;;;;;   ;;;;;;;;  
;   ;;;;   ;;;;   ;;;;  ;;;; ;;;;;;;  ;;;;  ;;;; 
;   ;;;;   ;;;;        ;;;;;  ;;;;         ;;;;; 
;   ;;;;   ;;;;    ;;;;;;;;;  ;;;;     ;;;;;;;;; 
;   ;;;;   ;;;;   ;;;;; ;;;;  ;;;;    ;;;;; ;;;; 
;   ;;;;  ;;;;;   ;;;;  ;;;;  ;;;;    ;;;;  ;;;; 
;   ;;;;;;;;;;    ;;;;  ;;;;  ;;;;    ;;;;  ;;;; 
;   ;;;;;;;;;     ;;;;;;;;;;  ;;;;;;  ;;;;;;;;;; 
;   ;;;;;;;;       ;;;; ;;;;   ;;;;;   ;;;; ;;;; 
;                                                
;                                                
;                                                 
;                                                
;                                                
;


;                                                                                                      
;                                                                                                      
;                                                                                                      
;      ;;;;;                                            ;                               ;              
;     ;;;;;;;;                                       ;;;;                            ;;;;              
;    ;;;;;;;;;                                       ;;;;                            ;;;;              
;   ;;;;; ;;;;;      ;;;;     ;;;; ;;;;     ;;;;;   ;;;;;;;    ;;;;;;    ;;;; ;;;;  ;;;;;;;    ;;;;;   
;   ;;;;   ;;      ;;;;;;;;   ;;;;;;;;;;   ;;;;;;;  ;;;;;;;   ;;;;;;;;   ;;;;;;;;;; ;;;;;;;   ;;;;;;;  
;   ;;;;           ;;;  ;;;   ;;;;;;;;;;  ;;;;  ;;; ;;;;;;;  ;;;;  ;;;;  ;;;;;;;;;; ;;;;;;;  ;;;;  ;;; 
;   ;;;;          ;;;;  ;;;;  ;;;;  ;;;;  ;;;;       ;;;;         ;;;;;  ;;;;  ;;;;  ;;;;    ;;;;      
;   ;;;;   ;;     ;;;;  ;;;;  ;;;;  ;;;;   ;;;;;;;   ;;;;     ;;;;;;;;;  ;;;;  ;;;;  ;;;;     ;;;;;;;  
;   ;;;;   ;;;;   ;;;;  ;;;;  ;;;;  ;;;;    ;;;;;;;  ;;;;    ;;;;; ;;;;  ;;;;  ;;;;  ;;;;      ;;;;;;; 
;   ;;;;; ;;;;;   ;;;;  ;;;;  ;;;;  ;;;;       ;;;;  ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;         ;;;; 
;    ;;;;;;;;;     ;;;  ;;;   ;;;;  ;;;;  ;;;  ;;;;  ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;  ;;;; 
;     ;;;;;;;;     ;;;;;;;;   ;;;;  ;;;;   ;;;;;;;   ;;;;;;  ;;;;;;;;;;  ;;;;  ;;;;  ;;;;;;   ;;;;;;;  
;      ;;;;;         ;;;;     ;;;;  ;;;;    ;;;;;     ;;;;;   ;;;; ;;;;  ;;;;  ;;;;   ;;;;;    ;;;;;   
;                                                                                                      
; 
 

                                            
;                                                                                                   
;                                                                                                   
;                                                                                                   
;   ;;;;;;;;;                                          ;    ;;;;                                    
;   ;;;;;;;;;                                       ;;;;    ;;;;                                    
;   ;;;;;;;;;                                       ;;;;                                            
;   ;;;;        ;;;;  ;;;;  ;;;; ;;;;     ;;;;;;   ;;;;;;;  ;;;;     ;;;;     ;;;; ;;;;     ;;;;;   
;   ;;;;        ;;;;  ;;;;  ;;;;;;;;;;   ;;;;;;;;  ;;;;;;;  ;;;;   ;;;;;;;;   ;;;;;;;;;;   ;;;;;;;  
;   ;;;;;;;;    ;;;;  ;;;;  ;;;;;;;;;;   ;;;  ;;;; ;;;;;;;  ;;;;   ;;;  ;;;   ;;;;;;;;;;  ;;;;  ;;; 
;   ;;;;;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;    ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;      
;   ;;;;;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;        ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;   ;;;;;;;  
;   ;;;;        ;;;;  ;;;;  ;;;;  ;;;;  ;;;;        ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;;;;; 
;   ;;;;        ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;    ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;       ;;;; 
;   ;;;;        ;;;;;;;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;;   ;;;  ;;;   ;;;;  ;;;;  ;;;  ;;;; 
;   ;;;;        ;;;;;;;;;;  ;;;;  ;;;;   ;;;;;;;;   ;;;;;;  ;;;;   ;;;;;;;;   ;;;;  ;;;;   ;;;;;;;  
;   ;;;;         ;;;; ;;;;  ;;;;  ;;;;    ;;;;;;     ;;;;;  ;;;;     ;;;;     ;;;;  ;;;;    ;;;;;   
;                                                                                                   
;














;;;;;;;;;;;  MAKE INITIAL AIRPLANE

;; random-airplane-head : -> Board-POSN
;; randomly give a screen position of head
(define (random-airplane-head)
  (make-posn (random MAX-POSN) (random MAX-POSN)))


;; make-initial-world : board-posn direction Board-POSN [Listof Board-POSN] [Listof Board-POSN]-> Board 
;; to construct an initial world with the airplane the given coordinates OR
;; #false if the airplane does not fit
;; Strategy : structural decomposition
(define (make-initial-world head dir)
  (cond
    [(false? (valid-airplane head dir))   ;if the random airplane is not valid, recursion
     (make-initial-world (random-airplane-head)
                         (random RANDOM-DIRECTION))] ; give random airplane posns, can't test
    [else
     (valid-airplane head dir)]))



;; valid-airplane : board-posn direction -> Board or #false
;; check if it's the valid airplane, return Board or false
;; strategy : structral decomposition
(define (valid-airplane head dir)
  (cond
    [(and (= dir 0) ;"up"
          (>= (- (posn-x head) AIRPLANE-WING) BORDER-TOP)
          (<= (+ (posn-x head) AIRPLANE-WING) BORDER-DOWN)
          (<= (+ (posn-y head) AIRPLANE-BODY) BORDER-DOWN))
     (make-board head (construct-up-airplane head) '())]
    [(and (= dir 1) ;"down"
          (>= (- (posn-x head) AIRPLANE-WING) BORDER-TOP)
          (<= (+ (posn-x head) AIRPLANE-WING) BORDER-DOWN)
          (>= (- (posn-y head) AIRPLANE-BODY) BORDER-TOP))
     (make-board head (construct-down-airplane head) '())]
    [(and (= dir 2) ;"left"
          (>= (- (posn-y head) AIRPLANE-WING) BORDER-TOP)
          (<= (+ (posn-y head) AIRPLANE-WING) BORDER-DOWN)
          (<= (+ (posn-x head) AIRPLANE-BODY) BORDER-DOWN))
     (make-board head (construct-left-airplane head) '())]
    [(and (= dir 3) ;"right"
          (>= (- (posn-y head) AIRPLANE-WING) BORDER-TOP)
          (<= (+ (posn-y head) AIRPLANE-WING) BORDER-DOWN)
          (>= (- (posn-x head) AIRPLANE-BODY) BORDER-TOP))
     (make-board head (construct-right-airplane head) '())]
    [else
     #f]))




; key : Airplane-map Key-event Screen-POSN-> Airplane-map   //?mouse-event gives posn
; -Response to mouse-event by changing the color, mode, remaining attemps and status.
; -Click the covered block to change color.
; -Every time click on a covered block, subtract one remaining attemps; won't subtract while
;  clicking on an uncovered block.
; -Use 1,2,3,4 to change mode. 1: normal 2: possibility 3: cheat.
; -change the status when win or lose the game
;
; Strategy: Functional Composition
(define (key am input-key)
  (cond
    [(string=? input-key "1")
     (make-am (am-board am)
              "normal"
              (am-timer am)
              (am-ra am)
              (am-wol am))]
    [(string=? input-key "2")
     (make-am (am-board am)
              "possibility"
              (am-timer am)
              (am-ra am)
              (am-wol am))]
    [(string=? input-key "3")
     (make-am (am-board am)
              "cheat"
              (am-timer am)
              (am-ra am)
              (am-wol am))]
    [else
     am]))






;; a Win-or-lose is either:
;;  - "underway"
;;  - (make-win Natural) 
;;  - (make-lose Natural)
;(define-struct win (ticks-left))
;(define-struct lose (ticks-left))




; tick : Airplane-map -> Airplane-map
; Advances the game world state every tick. update the timer
; Strategy: Functional Composition
(define (tick am)
  (cond
    [(string? (am-wol am))
     (make-am (am-board am)
              (am-mode am)
              (+ (am-timer am) 1)
              (am-ra am)
              "underway")]
    [(< (wol-size (am-wol am)) 200)
     (make-am (am-board am)
              (am-mode am)
              (am-timer am)
              (am-ra am)
              (make-wol (wol-win? (am-wol am))
                        (+ (wol-size (am-wol am)) 1)))]
    [else
     (make-am (am-board am)
              (am-mode am)
              (am-timer am)
              (am-ra am)
              (make-wol (wol-win? (am-wol am))
                        (wol-size (am-wol am))))]))

(check-equal? (wol-size
               (am-wol
                (tick
                 (make-am
                  (make-board (make-posn 3 3)
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
                  (make-wol #t 180)))))
              181)

(check-equal? (wol-size
               (am-wol
                (tick
                 (make-am
                  (make-board (make-posn 3 3)
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
                  (make-wol #t 200)))))
              200)

(check-equal? (am-timer (tick (make-am (make-board (make-posn 3 3)
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
                                       "underway")))
              1)

(check-equal? (am-mode (key (make-am (make-board (make-posn 3 3)
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
                            "4"))
              "normal")

(check-equal? (valid-airplane (make-posn 3 3) 0)
              (board (posn 3 3)
                     (list (posn 1 4)
                           (posn 2 4)
                           (posn 3 4)
                           (posn 4 4)
                           (posn 5 4)
                           (posn 3 5)
                           (posn 2 6)
                           (posn 3 6)
                           (posn 4 6))
                     '()))
(check-equal? (valid-airplane (make-posn 3 3) 1)
              (board (posn 3 3)
                     (list (posn 1 2)
                           (posn 2 2)
                           (posn 3 2)
                           (posn 4 2)
                           (posn 5 2)
                           (posn 3 1)
                           (posn 2 0)
                           (posn 3 0)
                           (posn 4 0))
                     '()))
(check-equal? (valid-airplane (make-posn 3 3) 2)
              (board (posn 3 3)
                     (list (posn 4 1)
                           (posn 4 2)
                           (posn 4 3)
                           (posn 4 4)
                           (posn 4 5)
                           (posn 5 3)
                           (posn 6 2)
                           (posn 6 3)
                           (posn 6 4))
                     '()))
(check-equal? (valid-airplane (make-posn 3 3) 3)
              (board (posn 3 3)
                     (list (posn 2 1)
                           (posn 2 2)
                           (posn 2 3)
                           (posn 2 4)
                           (posn 2 5)
                           (posn 1 3)
                           (posn 0 2)
                           (posn 0 3)
                           (posn 0 4))
                     '()))
(check-equal? (valid-airplane (make-posn 0 0) 4)
              #f)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(check-equal? (key AM0 "1")
              (make-am BOARD "normal" TIMER ATTEMPT WIN-OR-LOSE))
(check-equal? (key AM0 "2")
              (make-am BOARD "possibility" TIMER ATTEMPT WIN-OR-LOSE))
(check-equal? (key AM0 "3")
              (make-am BOARD "cheat" TIMER ATTEMPT WIN-OR-LOSE))

(define AM1 (make-am (make-initial-world (random-airplane-head) (random RANDOM-DIRECTION))
                     MODE
                     TIMER
                     ATTEMPT
                     WIN-OR-LOSE))

(big-bang AM1
  [on-tick tick 1/30]
  [on-mouse mouse]
  [on-key  key]
  [to-draw draw])


