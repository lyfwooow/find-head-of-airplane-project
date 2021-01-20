#lang racket

(require "dd.rkt" "convert.rkt" "checkers.rkt" "update-valid.rkt" "construct.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)
(require 2htdp/abstraction)
(require rackunit)
(provide (all-defined-out))


(define BACKGROUND
  (square WORLD-SIDE "solid" BACKGROUND-COLOR))

(define HEAD
  (rectangle BLOCK-SIDE BLOCK-SIDE "solid" HEAD-COLOR))


(define BODY
  (rectangle BLOCK-SIDE BLOCK-SIDE "solid" BODY-COLOR))

(define EMPTY-BLOCK
  (rectangle BLOCK-SIDE BLOCK-SIDE "solid" EMPTY-COLOR))

(define COVERED-BLOCK
  (rectangle BLOCK-SIDE BLOCK-SIDE "solid" BACKGROUND-COLOR))

(define TRANS
  (square BLOCK-SIDE "outline" "transparent"))

(define s1 (square BLOCK-SIDE 'outline 'black))
(define r1 (beside s1 s1 s1 s1 s1 s1 s1))
(define BLANK-BOARD (above  r1 r1 r1 r1 r1 r1 r1))

(define TOTAL-NUMBER-OF-POSSIBLE-AIRPLANE 40.0) ;;put formular, show the process


; draw : Airplane-map -> Scene
; Render the airplane map in the scene
; Strategy: Functional Composition
(define (draw am)
  (place-image
   BLANK-BOARD            ;49 empty blocks
   MAP-X MAP-Y
   (place-image  
    (draw-ra (am-ra am))                     ;draw remaining attempt
    REMAIN-ATTEMPT-X REMAIN-ATTEMPT-Y
    (place-image
     (draw-timer (am-timer am))              ;draw timer
     TIMER-X TIMER-Y
     (place-image 
      (draw-mode (am-mode am))               ;draw mode
      MODE-X MODE-Y
      (draw-blocks (am-board am)
                   (am-wol am)
                   (am-mode am)))))))

; draw-timer : Number -> Image
; helper: To print timer in the screen
; Strategy: Strcut Decomp
(define (draw-timer timer)                
  (text (timer-text timer) NUMBER-SIZE NUMBER-COLOR))

; timer-text : Number -> String
; given the string for drawing
(define (timer-text timer)
  (string-append (number->string (minute (sec-for-min timer)))
                 (string-append " : " (number->string (second timer)))))
 
; second : Number -> Number
; return seconds in bound of 60
(define (second timer)
  (- (floor (/ timer SEC-FOR-MIN))
     (* MINUTE (floor (/ (floor (/ timer SEC-FOR-MIN)) MINUTE)))))

(check-equal? (second 900) 30)
(check-equal? (second 2400) 20)
 
 
; sec-for-min : Number -> Number 
; return the total seconds
(define (sec-for-min timer)
  (floor (/ timer SEC-FOR-MIN)))

; minute : Number -> Number
; return total minutes
(define (minute sec)
  (floor (/ sec MINUTE)))

(check-equal? (draw-timer 3000)
              (text (timer-text 3000) NUMBER-SIZE NUMBER-COLOR))


; draw-mode : Mode -> Image
; helper: To print mode in the screen
; Strategy: Strcut Decomp
(define (draw-mode mode)                
  (text mode NUMBER-SIZE NUMBER-COLOR))

(check-equal? (draw-mode "normal")
              (text "normal" NUMBER-SIZE NUMBER-COLOR))

; draw-ra : Number -> Image
; helper: To print remaining attempts in the screen
; Strategy: Strcut Decomp
(define (draw-ra ra)
  (text (number->string ra) NUMBER-SIZE NUMBER-COLOR))

(check-equal? (draw-ra 10)
              (text "10" NUMBER-SIZE NUMBER-COLOR))

;;draw-blocks : Board Win-or-lose Natural Mode-> Image
;;To draw the block and animation in the map
;;Strategy: Struct Decomp
(define (draw-blocks board wol mode)
  (cond 
    [(string? wol)
     (draw-revealed-blocks board mode)] 
    [(wol-win? wol) (win-animation wol (board-head board) (board-body board))]
    [else
     (lose-animation wol (board-head board) (board-body board))]))


;;draw-revealed-blocks : Board Mode -> Image
;;To draw the revealed block in the map
;;Strategy: Struct Decomp
(define (draw-revealed-blocks board mode)
  (cond
    [(string=? mode "normal") (draw-normal (board-head board)
                                           (board-body board)
                                           (board-revealed board))]
    [(string=? mode "possibility")
     (define initial-view-model (prepare-view-model (board-revealed board)
                                                    (every-amount-as-percent (board-head board)
                                                                        (board-body board)
                                                                        (board-revealed board))))
     (define list-of-view-model (update-view-model (board-revealed board)
                                                   initial-view-model
                                                   POSN-OF-MAP))
     (draw-poss (board-head board)
                (board-body board)
                (board-revealed board)
                (draw-each-poss list-of-view-model)
                list-of-view-model)]
    [else
     (draw-cheat (board-head board)
                 (board-body board)
                 (board-revealed board)
                 (count-amount (board-head board)
                               (board-body board)
                               (board-revealed board)))]))


;An info is:
; (make-revealed-info Number Block-content Board-POSN)
; (make-covered-info Number Board-POSN)


; draw-poss : Board-Posn [Listof Board-Posn] [Listof Board-Posn] [Listof Image] [Listof View-model] -> Image
; given board and list of pictures of possibility and list of view model, return the image arranging all the posssibilities
(define (draw-poss head body revealed loi list-of-view-model)
  (cond
    [(empty? list-of-view-model) BACKGROUND]
    [(empty? revealed)
     (place-image
      (first loi)
      (posn-x (convert-board-posn (covered-info-board-posn (first list-of-view-model))))
      (posn-y (convert-board-posn (covered-info-board-posn (first list-of-view-model))))
      (place-image COVERED-BLOCK
                   (posn-x (convert-board-posn (covered-info-board-posn (first list-of-view-model))))
                   (posn-y (convert-board-posn (covered-info-board-posn (first list-of-view-model))))
                   (draw-poss head body '() (rest loi) (rest list-of-view-model))))]
    [(equal-posn? head (first revealed))
     (place-image HEAD
                  (posn-x (convert-board-posn (first revealed)))
                  (posn-y (convert-board-posn (first revealed)))
                  (draw-poss head body (rest revealed) loi list-of-view-model))]
    [(find-body? body (first revealed))
     (place-image BODY
                  (posn-x (convert-board-posn (first revealed)))
                  (posn-y (convert-board-posn (first revealed)))
                  (draw-poss head body (rest revealed) loi list-of-view-model))]
    [(and (not (find-body? body (first revealed)))
          (not (equal-posn? head (first revealed))))
     (place-image EMPTY-BLOCK
                  (posn-x (convert-board-posn (first revealed)))
                  (posn-y (convert-board-posn (first revealed)))
                  (draw-poss head body (rest revealed) loi list-of-view-model))]))


;draw-each-poss : [Listof View-mode] -> [Listof Image]
;gives a list of view model, returns the list of images of all possibilities
(define (draw-each-poss list-of-view-model)
  (for*/list ([l list-of-view-model])
    (text (number->string (covered-info-poss l))
          NUMBER-SIZE
          NUMBER-COLOR)))



;update-view-model : [Listof Board-POSN] [Listof View-mode] [Listof Board-POSN]-> [Listof Viewmodel]
;returns the view model without the revealed blocks
;strategy: functional decomposition
(define (update-view-model revealed list-of-view-model pom)
  (cond
    [(empty? list-of-view-model) '()]
    [else
     (cond
       [(revealed? revealed (covered-info-board-posn (first list-of-view-model)))
        (update-view-model revealed
                           (rest list-of-view-model)
                           pom)]
       
       [else
        (cons (first list-of-view-model)
              (update-view-model revealed
                                 (rest list-of-view-model)
                                 pom))])])) 


;prepare-view-model : [Listof Board-POSN] [Listof Number] -> [Listof View-mode]
;given list of percentage, returns list of view model contains the percentage 
;strategy: functional decomposition
(define (prepare-view-model revealed list-of-percent)
  (map (lambda (poss posn)
         (make-covered-info poss posn))
       list-of-percent
       POSN-OF-MAP))

; convert-to-percent : Board-POSN [Listof Board-POSN] [Listof Board-POSN] ->  [Listof Number]
; returns a list of the percentage of all the blocks.
(define (every-amount-as-percent head body revealed)
  (map (lambda (number)
         (/ number TOTAL-NUMBER-OF-POSSIBLE-AIRPLANE))
       (every-amount-test head body revealed POSN-OF-MAP)))


; every-amount-test : Board-POSN [Listof Board-POSN] [Listof Board-POSN] [Listof Board-POSN] -> [Listof Number]
; returns a list of number of how many airplane each block could have 

(define (every-amount-test head body revealed pom)
  (cond
    [(empty? pom) '()]
    [(empty? revealed)
     (cons (counting-test (first pom))
           (every-amount-test head body revealed (rest pom)))]
    [else
     (counting revealed body)]))
 


; counting : [Listof Board-POSN] [Listof Board-POSN] -> [Listof Number]
; return a list of number of amount of possible airplen for evech block 
(define (counting body revealed) 
  (for/list ([pom POSN-OF-MAP])
    (counting-one-block pom (count-list-of-airplane POSN-OF-MAP) body revealed)))

; catch : [Listof Board-POSN] [Listof Board-POSN] [Listof Borad-POSN] -> Number
; return sum of amount of possible airplane for given block 
(define (catch revealed body posn-of-an-airplane count)
  (for*/sum ([b body]
             [p posn-of-an-airplane])
    (revealed-number revealed b p)))

; revealed-number : [Listof Board-POSN] Board-POSN [Listof Board-POSN] -> Number
; return 1 if the given block is revealed and is body, otherwise return 0
(define (revealed-number revealed b p)
  (cond
    [(and (equal-posn? p b)
          (revealed? revealed p)) 1]
    [else 0]))

; counting-one-block :Borad-POSN [listof [Listof Borad-POSN]] [Listof Borad-POSN] [Listof Borad-POSN] -> Number
; return number of amount of possible airplane for given block 
(define (counting-one-block pom list-of-airplane body revealed)
  (cond
    [(empty? list-of-airplane) 0]
    [(revealed? (first list-of-airplane) pom)
     (define number-of-revealed (catch revealed body (first list-of-airplane) 0))
     (+ number-of-revealed
        (counting-one-block pom (rest list-of-airplane) body revealed))]
    [else
     (counting-one-block pom (rest list-of-airplane) body revealed)])) 

;draw-normal : Board-POSN [Litsof Board-POSN] [Litsof Board-POSN] -> Image
;;To draw normal blocks
;;Strategy: Struct Decomp
(define (draw-normal head body revealed)
  (cond
    [(empty? revealed) BACKGROUND]
    [(equal-posn? head (first revealed))
     (draw-image head body HEAD revealed)]
    [(find-body? body (first revealed))
     (draw-image head body BODY revealed)]
    [else
     (draw-image head body EMPTY-BLOCK revealed)]))

;draw-image : Board-POSN [Litsof Board-POSN] Image [Litsof Board-POSN] -> Image
;helper function to draw image
(define (draw-image head body img revealed)
  (place-image
   img
   (posn-x (convert-board-posn (first revealed)))
   (posn-y (convert-board-posn (first revealed)))
   (draw-normal head body (rest revealed))))


; draw-cheat : Board-POSN [Listof Board-POSN] [Listof Board-POSN] [Listof Image] -> Image
; draw the cheat mode to the map
(define (draw-cheat head body revealed loi)
  (cond
    [(empty? revealed) BACKGROUND]
    [(equal-posn? head (first revealed))
     (place-image
      (first loi)
      (posn-x (convert-board-posn (first revealed))) 
      (posn-y (convert-board-posn (first revealed)))
      (place-image HEAD
                   (posn-x (convert-board-posn (first revealed)))
                   (posn-y (convert-board-posn (first revealed)))
                   (draw-cheat head body (rest revealed) (rest loi))))]
    [(find-body? body (first revealed))
     (place-image
      (first loi)
      (posn-x (convert-board-posn (first revealed)))
      (posn-y (convert-board-posn (first revealed)))
      (place-image BODY
                   (posn-x (convert-board-posn (first revealed)))
                   (posn-y (convert-board-posn (first revealed)))
                   (draw-cheat head body (rest revealed) (rest loi))))]
    [else
     (place-image
      (first loi)
      (posn-x (convert-board-posn (first revealed)))
      (posn-y (convert-board-posn (first revealed)))
      (place-image EMPTY-BLOCK
                   (posn-x (convert-board-posn (first revealed)))
                   (posn-y (convert-board-posn (first revealed)))
                   (draw-cheat head body (rest revealed) (rest loi))))]))


; count-amount : Board-POSN [Listof Board-POSN] [Listof Board-POSN] -> [Listof image]
; return a list of images of number of body blocks that around the given posn
(define (count-amount head body revealed)
  (for/list ([r revealed])
    (text (number->string (number-of-body-or-head head body r 0))
          NUMBER-SIZE
          NUMBER-COLOR)))


; number-of-body-or-head : Board-POSN [Listof Board-POSN] Board-POSN Number -> Number
; return a number of body blocks that arougn the given posn
(define (number-of-body-or-head head body r count)
  (cond
    [(equal-posn? r head) 3]
    [else
     (define x1 (+ (posn-x r) 1))
     (define x2 (posn-x r))
     (define x3 (- (posn-x r) 1))
     (define y1 (+ (posn-y r) 1))
     (define y2 (posn-y r))
     (define y3 (- (posn-y r) 1))
     (for*/sum ([x (list x1 x2 x3)]
                [y (list y1 y2 y3)]
                ;#:when (not (and (= x x2) (= y y2)))
                )
       (+ (number-of-revealed-body (make-posn x y) body) count))
     ]))


;number-of-revealed-body : Board-POSN [Listof Board-POSN] Number -> Number
(define (number-of-revealed-body r body)
  (cond
    [(empty? body) 0]
    [else
     (cond
       [(equal-posn? r (first body)) 1]
       [else
        (number-of-revealed-body r (rest body))])]))


(check-equal? (number-of-body-or-head (make-posn 3 3)
                                      (list (make-posn 1 4)
                                            (make-posn 2 4)
                                            (make-posn 3 4)
                                            (make-posn 4 4)
                                            (make-posn 5 4)
                                            (make-posn 3 5)
                                            (make-posn 2 6)
                                            (make-posn 3 6)
                                            (make-posn 4 6))
                                      (make-posn 2 5)
                                      0) 6)

(check-equal? (count-amount (make-posn 3 3)
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
              (count-amount (make-posn 3 3)
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
                                  (make-posn 2 4))))




; win-animation : Win-or-lose Board-POSN [Listof Board-POSN] -> Scene
; draw the winning animation to the scene and also the red head 
(define (win-animation wol head body)
  (cond
    [(empty? body) BACKGROUND]
    [else
     (place-image
      (text/font "WIN :)" (wol-size wol) "olive"
                 "Gill Sans" 'swiss 'normal 'bold #f)
      150 150
      (place-image
       (star BLOCK-SIDE "solid" "red")
       (posn-x (convert-board-posn head))
       (posn-y (convert-board-posn head))
       (place-image
        HEAD
        (posn-x (convert-board-posn head))
        (posn-y (convert-board-posn head))
        (place-image
         BODY
         (posn-x (convert-board-posn (first body)))
         (posn-y (convert-board-posn (first body)))
         (win-animation wol head (rest body))))))]))



; lose-animation : Win-or-lose Board-POSN [Listof Board-POSN]  -> Scene
; draw the losing animation to the scene
(define (lose-animation wol head body)
  (cond
    [(empty? body) BACKGROUND]
    [else
     (place-image
      (text/font "LOSE :(" (wol-size wol) "olive"
                 "Gill Sans" 'swiss 'normal 'bold #f)
      150 150
      (place-image
       HEAD
       (posn-x (convert-board-posn head))
       (posn-y (convert-board-posn head))
       (place-image
        BODY
        (posn-x (convert-board-posn (first body)))
        (posn-y (convert-board-posn (first body)))
        (lose-animation wol head (rest body)))))]))




(check-equal? (draw (make-am (make-board (make-posn 3 3)
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
                             3000
                             8
                             "underway"))
              (draw (make-am (make-board (make-posn 3 3)
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
                             3000
                             8
                             "underway"))) 

(check-equal? (draw (make-am (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 3 3)
                                               (make-posn 1 4)
                                               (make-posn 2 2)
                                               (make-posn 0 0)))
                             "cheat"
                             2432
                             6
                             "underway"))
              (draw (make-am (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 3 3)
                                               (make-posn 1 4)
                                               (make-posn 2 2)
                                               (make-posn 0 0)))
                             "cheat"
                             2432
                             6
                             "underway")))

(check-equal? (draw (make-am (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 1 4)
                                               (make-posn 2 5)))
                             "possibility"
                             932
                             8
                             "underway"))
              (draw (make-am (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 1 4)
                                               (make-posn 2 5)))
                             "possibility"
                             932
                             8
                             "underway")))


(check-equal? (draw (make-am (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 3 3)
                                               (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6)))
                             "normal"
                             0
                             8
                             "underway"))
              (draw (make-am (make-board (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 3 3)
                                               (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6)))
                             "normal"
                             0
                             8
                             "underway")))




(check-equal? (draw-blocks (make-board (make-posn 3 3)
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
                           "underway"
                           "normal")
              (draw-blocks (make-board (make-posn 3 3)
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
                           "underway"
                           "normal"))

(check-equal? (draw-blocks (make-board (make-posn 3 3)
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
                           "underway"
                           "possibility")
              (draw-blocks (make-board (make-posn 3 3)
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
                           "underway"
                           "possibility"))

(check-equal? (draw-blocks (make-board (make-posn 3 3)
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
                           "underway"
                           "cheat")

              (draw-blocks (make-board (make-posn 3 3)
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
                           "underway"
                           "cheat"))



(check-equal? (draw-revealed-blocks (make-board (make-posn 3 3)
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
                                    "possibility")
              (draw-revealed-blocks (make-board (make-posn 3 3)
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
                                    "possibility"))  
(check-equal? (draw-revealed-blocks (make-board (make-posn 3 3)
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
                                    "cheat")
              (draw-revealed-blocks (make-board (make-posn 3 3)
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
                                    "cheat"))


(check-equal? (draw-poss (make-posn 3 3)
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
                               (make-posn 2 4))
                         (draw-each-poss (update-view-model (list (make-posn 2 3)
                                                                  (make-posn 2 4))
                                                            (prepare-view-model (list (make-posn 2 3)
                                                                                      (make-posn 2 4))
                                                                                (every-amount-as-percent (make-posn 3 3)
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
                                                                                                          (make-posn 2 4))))
                                                            POSN-OF-MAP))
                         (update-view-model (list (make-posn 2 3)
                                                  (make-posn 2 4))
                                            (prepare-view-model (list (make-posn 2 3)
                                                                      (make-posn 2 4))
                                                                (every-amount-as-percent (make-posn 3 3)
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
                                                                                          (make-posn 2 4))))
                                            POSN-OF-MAP))
              (draw-poss (make-posn 3 3)
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
                               (make-posn 2 4))
                         (draw-each-poss (update-view-model (list (make-posn 2 3)
                                                                  (make-posn 2 4))
                                                            (prepare-view-model (list (make-posn 2 3)
                                                                                      (make-posn 2 4))
                                                                                (every-amount-as-percent (make-posn 3 3)
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
                                                                                                          (make-posn 2 4))))
                                                            POSN-OF-MAP))
                         (update-view-model (list (make-posn 2 3)
                                                  (make-posn 2 4))
                                            (prepare-view-model (list (make-posn 2 3)
                                                                      (make-posn 2 4))
                                                                (every-amount-as-percent (make-posn 3 3)
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
                                                                                          (make-posn 2 4))))
                                            POSN-OF-MAP)))

(check-equal? (draw-poss (make-posn 3 3)
                         (list (make-posn 1 4)
                               (make-posn 2 4)
                               (make-posn 3 4)
                               (make-posn 4 4)
                               (make-posn 5 4)
                               (make-posn 3 5)
                               (make-posn 2 6)
                               (make-posn 3 6)
                               (make-posn 4 6))
                         '()
                         (draw-each-poss  (update-view-model '()
                                                             (prepare-view-model '()
                                                                                 (every-amount-as-percent (make-posn 3 3)
                                                                                                     (list (make-posn 1 4)
                                                                                                           (make-posn 2 4)
                                                                                                           (make-posn 3 4)
                                                                                                           (make-posn 4 4)
                                                                                                           (make-posn 5 4)
                                                                                                           (make-posn 3 5)
                                                                                                           (make-posn 2 6)
                                                                                                           (make-posn 3 6)
                                                                                                           (make-posn 4 6))
                                                                                                     '()))
                                                             POSN-OF-MAP))
                         (update-view-model '()
                                            (prepare-view-model '()
                                                                (every-amount-as-percent (make-posn 3 3)
                                                                                    (list (make-posn 1 4)
                                                                                          (make-posn 2 4)
                                                                                          (make-posn 3 4)
                                                                                          (make-posn 4 4)
                                                                                          (make-posn 5 4)
                                                                                          (make-posn 3 5)
                                                                                          (make-posn 2 6)
                                                                                          (make-posn 3 6)
                                                                                          (make-posn 4 6))
                                                                                    '()))
                                            POSN-OF-MAP))

              (draw-poss (make-posn 3 3)
                         (list (make-posn 1 4)
                               (make-posn 2 4)
                               (make-posn 3 4)
                               (make-posn 4 4)
                               (make-posn 5 4)
                               (make-posn 3 5)
                               (make-posn 2 6)
                               (make-posn 3 6)
                               (make-posn 4 6))
                         '()
                         (draw-each-poss  (update-view-model '()
                                                             (prepare-view-model '()
                                                                                 (every-amount-as-percent (make-posn 3 3)
                                                                                                     (list (make-posn 1 4)
                                                                                                           (make-posn 2 4)
                                                                                                           (make-posn 3 4)
                                                                                                           (make-posn 4 4)
                                                                                                           (make-posn 5 4)
                                                                                                           (make-posn 3 5)
                                                                                                           (make-posn 2 6)
                                                                                                           (make-posn 3 6)
                                                                                                           (make-posn 4 6))
                                                                                                     '()))
                                                             POSN-OF-MAP))
                         (update-view-model '()
                                            (prepare-view-model '()
                                                                (every-amount-as-percent (make-posn 3 3)
                                                                                    (list (make-posn 1 4)
                                                                                          (make-posn 2 4)
                                                                                          (make-posn 3 4)
                                                                                          (make-posn 4 4)
                                                                                          (make-posn 5 4)
                                                                                          (make-posn 3 5)
                                                                                          (make-posn 2 6)
                                                                                          (make-posn 3 6)
                                                                                          (make-posn 4 6))
                                                                                    '()))
                                            POSN-OF-MAP)))

(check-equal? (draw-poss (make-posn 3 3)
                         (list (make-posn 1 4)
                               (make-posn 2 4)
                               (make-posn 3 4)
                               (make-posn 4 4)
                               (make-posn 5 4)
                               (make-posn 3 5)
                               (make-posn 2 6)
                               (make-posn 3 6)
                               (make-posn 4 6))
                         (list (make-posn 3 3))
                         (draw-each-poss (update-view-model (list (make-posn 3 3))
                                                            (prepare-view-model (list (make-posn 3 3))
                                                                                (every-amount-as-percent (make-posn 3 3)
                                                                                                    (list (make-posn 1 4)
                                                                                                          (make-posn 2 4)
                                                                                                          (make-posn 3 4)
                                                                                                          (make-posn 4 4)
                                                                                                          (make-posn 5 4)
                                                                                                          (make-posn 3 5)
                                                                                                          (make-posn 2 6)
                                                                                                          (make-posn 3 6)
                                                                                                          (make-posn 4 6))
                                                                                                    (list (make-posn 3 3))))
                                                            POSN-OF-MAP))
                         (update-view-model (list (make-posn 3 3))
                                            (prepare-view-model (list (make-posn 3 3))
                                                                (every-amount-as-percent (make-posn 3 3)
                                                                                    (list (make-posn 1 4)
                                                                                          (make-posn 2 4)
                                                                                          (make-posn 3 4)
                                                                                          (make-posn 4 4)
                                                                                          (make-posn 5 4)
                                                                                          (make-posn 3 5)
                                                                                          (make-posn 2 6)
                                                                                          (make-posn 3 6)
                                                                                          (make-posn 4 6))
                                                                                    (list (make-posn 3 3))))
                                            POSN-OF-MAP))
              (draw-poss (make-posn 3 3)
                         (list (make-posn 1 4)
                               (make-posn 2 4)
                               (make-posn 3 4)
                               (make-posn 4 4)
                               (make-posn 5 4)
                               (make-posn 3 5)
                               (make-posn 2 6)
                               (make-posn 3 6)
                               (make-posn 4 6))
                         (list (make-posn 3 3))
                         (draw-each-poss (update-view-model (list (make-posn 3 3))
                                                            (prepare-view-model (list (make-posn 3 3))
                                                                                (every-amount-as-percent (make-posn 3 3)
                                                                                                    (list (make-posn 1 4)
                                                                                                          (make-posn 2 4)
                                                                                                          (make-posn 3 4)
                                                                                                          (make-posn 4 4)
                                                                                                          (make-posn 5 4)
                                                                                                          (make-posn 3 5)
                                                                                                          (make-posn 2 6)
                                                                                                          (make-posn 3 6)
                                                                                                          (make-posn 4 6))
                                                                                                    (list (make-posn 3 3))))
                                                            POSN-OF-MAP))
                         (update-view-model (list (make-posn 3 3))
                                            (prepare-view-model (list (make-posn 3 3))
                                                                (every-amount-as-percent (make-posn 3 3)
                                                                                    (list (make-posn 1 4)
                                                                                          (make-posn 2 4)
                                                                                          (make-posn 3 4)
                                                                                          (make-posn 4 4)
                                                                                          (make-posn 5 4)
                                                                                          (make-posn 3 5)
                                                                                          (make-posn 2 6)
                                                                                          (make-posn 3 6)
                                                                                          (make-posn 4 6))
                                                                                    (list (make-posn 3 3))))
                                            POSN-OF-MAP)))



(check-equal? (draw-each-poss  (update-view-model (list (make-posn 2 3)
                                                        (make-posn 2 4))
                                                  (prepare-view-model (list (make-posn 2 3)
                                                                            (make-posn 2 4))
                                                                      (every-amount-as-percent (make-posn 3 3)
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
                                                                                                (make-posn 2 4))))
                                                  POSN-OF-MAP))
              (draw-each-poss  (update-view-model (list (make-posn 2 3)
                                                        (make-posn 2 4))
                                                  (prepare-view-model (list (make-posn 2 3)
                                                                            (make-posn 2 4))
                                                                      (every-amount-as-percent (make-posn 3 3)
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
                                                                                                (make-posn 2 4))))
                                                  POSN-OF-MAP)))




(check-equal? (every-amount-as-percent (make-posn 3 3)
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
              '(0
                0
                0.025
                0.025
                0.05
                0.025
                0
                0
                0
                0.1
                0.075
                0.15
                0.075
                0.05
                0.025
                0.05
                0.2
                0.225
                0.4
                0.15
                0.075
                0
                0
                0.15
                0.175
                0.275
                0.1
                0.075
                0
                0.05
                0.2
                0.175
                0.275
                0.125
                0.075
                0
                0
                0.05
                0
                0.1
                0
                0.025
                0
                0
                0
                0
                0.05
                0
                0))


(check-equal? (every-amount-test (make-posn 3 3)
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
                                       (make-posn 2 4))
                                 POSN-OF-MAP)
              '(0
                0
                1
                1
                2
                1
                0
                0
                0
                4
                3
                6
                3
                2
                1
                2
                8
                9
                16
                6
                3
                0
                0
                6
                7
                11
                4
                3
                0
                2
                8
                7
                11
                5
                3
                0
                0
                2
                0
                4
                0
                1
                0
                0
                0
                0
                2
                0
                0))




(check-equal?(draw-normal (make-posn 3 3)
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
             (draw-normal (make-posn 3 3)
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
                                (make-posn 2 4))))


(check-equal? (draw-cheat (make-posn 3 3)
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
                                (make-posn 2 4))
                          (count-amount (make-posn 3 3)
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
                                              (make-posn 2 4))))
              (draw-cheat (make-posn 3 3)
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
                                (make-posn 2 4))
                          (count-amount (make-posn 3 3)
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
                                              (make-posn 2 4)))))
(check-equal? (count-amount (make-posn 3 3)
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
              (list (text "3"
                          NUMBER-SIZE
                          NUMBER-COLOR)
                    (text "4"
                          NUMBER-SIZE
                          NUMBER-COLOR)))
(check-equal? (draw-normal (make-posn 3 3)
                           (list (make-posn 1 4)
                                 (make-posn 2 4)
                                 (make-posn 3 4)
                                 (make-posn 4 4)
                                 (make-posn 5 4)
                                 (make-posn 3 5)
                                 (make-posn 2 6)
                                 (make-posn 3 6)
                                 (make-posn 4 6))
                           (list (make-posn 2 4)))
              (draw-normal (make-posn 3 3)
                           (list (make-posn 1 4)
                                 (make-posn 2 4)
                                 (make-posn 3 4)
                                 (make-posn 4 4)
                                 (make-posn 5 4)
                                 (make-posn 3 5)
                                 (make-posn 2 6)
                                 (make-posn 3 6)
                                 (make-posn 4 6))
                           (list (make-posn 2 4))))

(check-equal?  (draw-cheat (make-posn 3 3)
                           (list (make-posn 1 4)
                                 (make-posn 2 4)
                                 (make-posn 3 4)
                                 (make-posn 4 4)
                                 (make-posn 5 4)
                                 (make-posn 3 5)
                                 (make-posn 2 6)
                                 (make-posn 3 6)
                                 (make-posn 4 6))
                           (list (make-posn 2 4))
                           (count-amount (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 2 4))))
               (draw-cheat (make-posn 3 3)
                           (list (make-posn 1 4)
                                 (make-posn 2 4)
                                 (make-posn 3 4)
                                 (make-posn 4 4)
                                 (make-posn 5 4)
                                 (make-posn 3 5)
                                 (make-posn 2 6)
                                 (make-posn 3 6)
                                 (make-posn 4 6))
                           (list (make-posn 2 4))
                           (count-amount (make-posn 3 3)
                                         (list (make-posn 1 4)
                                               (make-posn 2 4)
                                               (make-posn 3 4)
                                               (make-posn 4 4)
                                               (make-posn 5 4)
                                               (make-posn 3 5)
                                               (make-posn 2 6)
                                               (make-posn 3 6)
                                               (make-posn 4 6))
                                         (list (make-posn 2 4)))))

(check-equal? (win-animation (make-wol #t 30)
                             (make-posn 3 3)
                             (list (make-posn 1 4)
                                   (make-posn 2 4)
                                   (make-posn 3 4)
                                   (make-posn 4 4)
                                   (make-posn 5 4)
                                   (make-posn 3 5)
                                   (make-posn 2 6)
                                   (make-posn 3 6)
                                   (make-posn 4 6)))
              (win-animation (make-wol #t 30)
                             (make-posn 3 3)
                             (list (make-posn 1 4)
                                   (make-posn 2 4)
                                   (make-posn 3 4)
                                   (make-posn 4 4)
                                   (make-posn 5 4)
                                   (make-posn 3 5)
                                   (make-posn 2 6)
                                   (make-posn 3 6)
                                   (make-posn 4 6))))
(check-equal? (win-animation (make-wol #t 30)
                             '()
                             '())

              
              BACKGROUND)


(check-equal? (draw-blocks (make-board (make-posn 3 3)
                                       (list (make-posn 1 4)
                                             (make-posn 2 4)
                                             (make-posn 3 4)
                                             (make-posn 4 4)
                                             (make-posn 5 4)
                                             (make-posn 3 5)
                                             (make-posn 2 6)
                                             (make-posn 3 6)
                                             (make-posn 4 6))
                                       '())
                           (make-wol #t 30)
                           "normal")
              (win-animation (make-wol #t 30)
                             (make-posn 3 3)
                             (list (make-posn 1 4)
                                   (make-posn 2 4)
                                   (make-posn 3 4)
                                   (make-posn 4 4)
                                   (make-posn 5 4)
                                   (make-posn 3 5)
                                   (make-posn 2 6)
                                   (make-posn 3 6)
                                   (make-posn 4 6))))
(check-equal? (draw-blocks (make-board (make-posn 3 3)
                                       (list (make-posn 1 4)
                                             (make-posn 2 4)
                                             (make-posn 3 4)
                                             (make-posn 4 4)
                                             (make-posn 5 4)
                                             (make-posn 3 5)
                                             (make-posn 2 6)
                                             (make-posn 3 6)
                                             (make-posn 4 6))
                                       '())
                           (make-wol #f 50)
                           "normal")
              (lose-animation (make-wol #f 50) (make-posn 3 3) (list (make-posn 1 4)
                                                                     (make-posn 2 4)
                                                                     (make-posn 3 4)
                                                                     (make-posn 4 4)
                                                                     (make-posn 5 4)
                                                                     (make-posn 3 5)
                                                                     (make-posn 2 6)
                                                                     (make-posn 3 6)
                                                                     (make-posn 4 6))))









