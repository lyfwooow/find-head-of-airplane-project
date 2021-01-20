#lang racket

(provide (all-defined-out))
(require 2htdp/image)


;An Airplane-map is
; (make-am Board Mode Number Number Win-or-Lose)
; interp.: if `an-am` is a Airplane-Map then all of:
; - (am-board an-am) is a struct of a head position, list of airplane positions and list of revealed block positions
; - (am-mode an-am) is the mode of the game
; - (am-timer an-am) is the time of user spending on the game
; - (am-ra an-am) is the remaining-attempt (10)
; - (am-wol an-am) is the status of the game, and animation
(define-struct am (board mode timer ra wol)#:transparent)

; A Board is:
; (make-board Board-POSN [List-of Board-POSN] [List-of Board-POSN])
; interp.: if `a-bd` is a Board then all of:
; - (board-head a-bd) is the head position of airplane represented in Board-posn 
; - (board-body a-bd) is the list of positions of airplane body represented in Board-posn
; - (board-revealed a-bd) is the list of positions of revealed blocks represented in Board-posn
(define-struct board (head body revealed)#:transparent)

;A View-model is a list of info:
; -[Listof Info]

; An info is:
; (make-covered-info Number Board-POSN)
(define-struct covered-info (poss board-posn))

; A Board-POSN is:
; posns represented as block number, like (1, 1) (2, 1)

; Screen-POSN is: 
; posns in mouse-moving pixels, like (123, 387)

; A mode is one of:
; - "normal"
; - "possibility"
; - "cheat"

; A Posn is (make-posn Real Real)
; (Note: `Real` means a real number, which excludes complex numbers.)
;; A posn:
(define-struct posn (x y)#:transparent)

;; A Win-or-lose is either
;;  - (make-wol Boolean Font-Size)
;;  - "underway"
(define-struct wol (win? size))

;; a Font-Size is
;;  a Natural between 0 and 200


;; define template for airplane-map
#;
(define (airplane-map am ...)
  ... (am-board am) ...                   ;list of a head position, airplane positions, revealed block positions
  ... (am-mode am) ...                    ;game mode (string)
  ... (am-timer am) ...                   ;timer (natural)
  ... (am-ra am) ...                      ;remaing attempts (natural)
  ... (am-wol am) ...)            ;status of game (string)


(define WORLD-SIDE         300)            ;window width and height(square)
(define BLOCK-SIDE          40)            ;block width and height(square)
(define MAP-MAX-POSN       290)
(define MAP-MIN-POSN        10)
(define MODE          "normal")        ;initial game mode
(define TIMER                0)            ;initial timer
(define ATTEMPT             10)     
(define POSSIBILITY          0)
(define SIZE                 2)
(define WIN-OR-LOSE   "underway")
(define BACKGROUND-COLOR   "White")
(define MAP-X            150)
(define MAP-Y            150)
(define TIMER-X         285)
(define TIMER-Y         5)
(define REMAIN-ATTEMPT-X           150)
(define REMAIN-ATTEMPT-Y           5)
(define MODE-X            30)
(define MODE-Y            5)
(define HEAD-COLOR       "Red")
(define BODY-COLOR      "Blue")
(define EMPTY-COLOR     "Grey")
(define RANDOM-DIRECTION     3)
(define MAX-POSN             6)
(define BORDER-TOP           0)
(define BORDER-DOWN          6)
(define POSS-SIZE           10)
(define POSS-COLOR     "black")
(define NUMBER-SIZE         10)
(define NUMBER-COLOR   "black")
(define AIRPLANE-WING        2)
(define AIRPLANE-BODY        3)
(define SEC-FOR-MIN         30)
(define MINUTE              60)


(define HEAD-POSN      (make-posn 0 3)) ;test airplane
(define BOARD      (make-board HEAD-POSN
                               (list (make-posn 1 1)
                                     (make-posn 2 1)
                                     (make-posn 3 1)
                                     (make-posn 4 1)
                                     (make-posn 5 1)
                                     (make-posn 3 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)
                                     (make-posn 4 3))
                               '()))

; MAP0 : airplane-map
; The initial state of the airplane map
(define AM0          (make-am BOARD MODE TIMER ATTEMPT WIN-OR-LOSE))






(define POSN-OF-MAP
  (for*/list ([x (in-range 0 7)]
              [y (in-range 0 7)])
    (make-posn x y)))

