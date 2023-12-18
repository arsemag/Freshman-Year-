;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get on errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the three functions below (we have deliberately omitted tests and purpose
;; statements):

;; flip: [List-of Boolean] -> [List-of Boolean]
(define (flip lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (not (first lob)) (flip (rest lob)))]))


;; until-zero: [List-of Number] -> [List-of Number]
(define (until-zero lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (= (first lon) 0)
         '()
         (cons (first lon) (until-zero (rest lon))))]))

;; words-until-period: [List-of String] -> [List-of String]
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) ".")
         '()
         (cons (first los) (words-until-period (rest los))))]))

;;! Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

; [String -> String][Numbers -> Numbers] -> [List of Numbers and String]




; until-sistuation ; Function -> ListofX
(define(until-situation l los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (cons (l (first los)
              (until-situation ( rest los))))]))






;;! Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can. Do not modify the code above. Instead, write your
;; functions here and call them flip/v2, until-zero/v2, or words-until-period/v2.

(define (words-until-period/v2 los)
  (until-situation  string=? los))

(define (until-zero/v2 lon)
  (until-situation  = lon))

(define (flip/v2 lon)
  (until-situation not lon))







;;! Problem 2

;; The objective in this problem is to define the following functions.
;; We have given their signatures, purpose statements, and check-expects.



(define-struct pair [first second])
;; A [Pair X] is a (make-pair X X) representing a pair of any type
;; - first is the first item in the pair
;; - second is the second item in the pair


; DifferentFunction ; X Y ListofPairs -> ListofPairs
; creates a list pairs using 2 different fuctions 
(define(different-function s b lol)
  (cond
    [(empty? lol) '()]
    [(cons? lol)
     (cons (make-pair (s (pair-first  (first lol))) (b (pair-second  (first lol))))
               (different-function s b ( rest lol)))]))
     





;; strings-or-odds : [List-of [Pair Number]] -> [List-of [Pair String]]
;; For each pair converts the first item to a string and the second to "odd".
(check-expect (strings-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (strings-or-odds (list (make-pair 20 30) (make-pair 0 1) (make-pair 3 4)))
              (list (make-pair "20" "odd") (make-pair "0" "odd") (make-pair "3" "odd")))
(check-expect (strings-or-odds '()) '())

; Odd; String
; takes in a value to produce the string odd
(check-expect (odd 23 )"odd")
(check-expect (odd 11 )"odd")
(check-expect (odd 30 )"odd")
(define (odd x)
  "odd")

(define (strings-or-odds los)
  (different-function number->string odd los))




;; alternate-case : [List-of [Pair String]] -> [List-of [Pair String]]
;; Uppercase the first item of each pair.
(check-expect (alternate-case (list (make-pair "hello" "world") (make-pair "this" "is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "is")))
(check-expect (alternate-case (list (make-pair "one" "two") (make-pair "three" "four") (make-pair "five" "six")))
              (list (make-pair "ONE" "two") (make-pair "THREE" "four") (make-pair "FIVE" "six")))
(check-expect (alternate-case (list (make-pair "apple" "banana"))) (list (make-pair "APPLE" "banana")))



;Recall X -> X
; returns given value
(check-expect (recall "world") "world")
(check-expect (recall "is") "is")
(check-expect (recall "two") "two")
(define (recall x)
  x)


(define ( alternate-case los)
  (different-function string-upcase recall los))




;; flip-or-keep-boolean : [List-of [Pair Boolean]] -> [List-of [Pair Boolean]]
;; Flip the first item of each pair, keep the second.
(check-expect (flip-or-keep-boolean (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))
(check-expect (flip-or-keep-boolean (list (make-pair #false #false) (make-pair #false #false)))
              (list (make-pair #true #false) (make-pair #true #false)))
(check-expect (flip-or-keep-boolean (list (make-pair #true #false) (make-pair #false #true)))
              (list (make-pair #false #false) (make-pair #true #true)))


(define (flip-or-keep-boolean los)
  (different-function not recall los))





;; However, you must not _directly_ use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.





;;! Problem 3

;; Objective: Build a Word Game

;; Your goal is to author a word-building game. You will start with an empty 5x1 grid
;; and a hidden list of random letters. When the player clicks on a cell, its
;; contents should be replaced by the next letter in the list. The game concludes
;; when the cells spell a five-letter word. (You should build a short list of
;; five letter words.)
;;
;; Here is a video that demonstrates the game:
;;
;;   https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw5.gif
;;
;; Here are questions to help you think through your program design:
;;
;; 1. What do you need in your world state? (What changes during the game?)
;;    Come up with a data design to represent the world state.
;;
;; 2. Your program needs to draw a board, handle mouse clicks, and stop when
;;    the player constructs a word or runs out of letters. These are three 
;;    functions that you need to design.
;;
;; 3. Finally, put it all together using big-bang.

(require 2htdp/image)
(require 2htdp/universe)



(define LETTER-COLOR "black")
(define LETTER-SIZE 15)



(define background
(beside  (rectangle 60 60 "outline" "white")
         (rectangle 60 60 "outline" "white")
         (rectangle 60 60 "outline" "white")
         (rectangle 60 60 "outline" "white")
         (rectangle 60 60 "outline" "white")))



(define CELL1 (rectangle 60 60 "outline" "black"))


         


; ListofWords; Words -> LOW
; ListofWords is
; "()
;  words created from the alphbet 
(define LOW0 '())
(define LOW1 (cons "lemon" LOW0))
(define LOW2 (cons "apple" LOW1))
(define LOW3 (cons "limes" LOW2))
(define LOW4 (cons "grape" LOW3))
(define LOW5 (cons "chair" LOW4))




;ListofLetters ; Letters -> ListofLetters
; ListofLetters is
; "()
;  letters from the alphbet 

(define LOLO '())
(define LOL1 (list "a" "b" "c" "d" "e"))
(define LOL2 (list  "f" "g" "h" "i" "j"))
(define LOL3 (list  "k" "l" "m" "n" "o"))
(define LOL4 (list "p" "q" "r" "s" "t"))
(define LOL5 (list  "u" "v" "w" "x" "y" "z"))



; WorldState is a (List (List ) List)
; Begins with a letter the mouse hovers then when pressed on the next letter changes.
(define MP1 (list (list "" "" "" "" "") LOL1))
(define MP2 (list (list "a" "b" "c" "d" "e") LOL2))
(define MP3 (list (list "f" "g" "h" "i" "j") LOL4))
(define MP4 (list (list "l" "e" "m" "o" "n") LOL3))


;draw-letter
; draws a letter
(check-expect (draw-letter "a") (text "a" LETTER-SIZE LETTER-COLOR))
(check-expect (draw-letter "b") (text "b" LETTER-SIZE LETTER-COLOR))
(check-expect (draw-letter "c") (text "c" LETTER-SIZE LETTER-COLOR))
(define (draw-letter l )
  (text l LETTER-SIZE LETTER-COLOR))


;DrawWord; ListOfLetter -> Image
; takes in a list of letters an draws the letters on to a cell


(define (draw-word lol)
 (if
  (empty? lol) (empty-scene 0 0)
  (beside
  (overlay (draw-letter (first lol))
          CELL1)
    (draw-word (rest lol)))))

(check-expect (draw-word (list "a")) (overlay (draw-letter "a") CELL1))
(check-expect (draw-word (list "b")) (overlay (draw-letter "b") CELL1))
(check-expect (draw-word (list "c")) (overlay (draw-letter "c") CELL1))



; draw-worldstate ; WorldState -> Image 
; takes in worldstate and and places letter onto worldstate
(check-expect (draw-world-state MP1)(beside
                                     (overlay (draw-letter "") CELL1)
                                     (overlay (draw-letter "") CELL1)
                                     (overlay (draw-letter "") CELL1)
                                     (overlay (draw-letter "") CELL1)
                                     (overlay (draw-letter "") CELL1)))
                                     
                                   


(check-expect (draw-world-state MP2) (beside
                                     (overlay (draw-letter "a") CELL1)
                                     (overlay (draw-letter "b") CELL1)
                                     (overlay (draw-letter "c") CELL1)
                                     (overlay (draw-letter "d") CELL1)
                                     (overlay (draw-letter "e") CELL1)))





(define (draw-world-state ws)
(draw-word (first ws)))
 

  

 ;box-index; Image -> Position of Box
; takes in image produces position of box

(check-expect (box-index 60) 1)
(check-expect (box-index 120) 2)
(check-expect (box-index 180) 3)

(define (box-index x )
  (floor (/ x 60)))

  





;; List-Index : Interger [Listof X] X -> Listof X
; finds the index of the lists and produces a list


(define (list-index in lol x )
  (cond
    [(empty? lol) '()]
    [(= 0 in ) (cons x (rest lol))]
    [else (cons (first lol) (list-index (- in 1) (rest lol) x))]))








; click-mouse MousePosition Int Int MouseEvent -> NextLetter
; when clicking on a box produces a next letter in list 

(check-expect (click-mouse MP1 30 60 "button-up") (list
 (list "a" "" "" "" "")
 (list "b" "c" "d" "e")))
                                                    
                                                       

(check-expect (click-mouse MP2  110  60  "button-up") (list
 (list "a" "f" "c" "d" "e")
 (list "g" "h" "i" "j")))




(define (click-mouse w x y mouse-event)
  (if
   (string=? "button-up" mouse-event)
   (list (list-index (box-index x) (first w) (first (second w))) (rest (second w)))
   w))









              





; Combine ; ListofStrings -> String
; takes a list of strings and combines them together to make on string 

(define (combine lol)
  (cond
  [(empty? lol) ""]
  [(cons? lol )
   (string-append (first lol)(combine (rest lol)))]))
                   



(define (word-made? w )
 (member (combine (first w))
         LOW5))


  
  
(check-expect (word-made? MP1) #f)
(check-expect (word-made? MP2) #f)
(check-expect (word-made? MP4) #t)








              
#;(define (word-game w)
(big-bang w
[to-draw draw-world-state]
[on-mouse click-mouse]
[stop-when word-made? draw-world-state]))

  



 




















