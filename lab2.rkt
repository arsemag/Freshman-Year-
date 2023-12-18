;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;;! Consider the following data definitions & interpretations:
(define-struct address [num st city us-state zip])

;;! An Address is a (make-address Nat String String String Nat)
;;! - where num is the number of the building on the street
;;! - st is the name of the street
;;! - city is the city the building is in
;;! - us-state is the state the city is in
;;! - and zip is the zipcode of the building
;;! Interpretation: a US address
(define-struct student [first last nuid local perm])

;;! An NUStudent is a (make-student String String PositiveNumber Address Address)
;;! - where first is the student's first name
;;! - last is the student's last name
;;! - nuid is the student's NUID #
;;! - local is the student's local address
;;! - and perm is the student's permanent address
;;! Interpretation: a Northeastern student

;;! Part A

;;! TODO:  Complete the data design recipe for both of the data definations above
;;! (i.e., provide examples and templates for address and student)

;; Example:
(define Address-1 (make-address 1600 "Pennsylvania Avenue NW" "Washington, D.C." "Washington" "20500"))
(define Address-2 (make-address 1500 "NE Miami Pl" "Miami" "FL" "33132"))
;; Template:
(define (address-temp a)
  (...(address-num a)...
      (address-st a)...
      (address-city a)...
      (address-us-state a)...
      (address-zip a)...))

;; Example:
(define Student-1 (make-student "Kevin" "Chen" "002345623" Address-1 Address-2))
;; Template:
(define (student-temp s)
  (... (student-first s)...
       (student-last s)...
       (student-nuid s)...
       (address-temp (student-local s))...
       (address-temp (studnet-perm s))))


;;! Part B

;;! TODO: Design the function student-email which takes an NUStudent and
;;! produces a string representing that student’s email address. For simplicity
;;! we will say that a student’s email address is always their last name
;;! (all lowercase),  followed by a period, followed by the first initial
;;! of their first name (also lowercase), and finished
;;! with "@northeastern.edu".

;;
(check-expect (student-email Student-1) "chen.kevin@northeastern.edu")
(define (student-email s)
        (string-append (string-downcase(student-last s)) "." (string-downcase(student-first s)) "@northeastern.edu"))

;;! Part C

;;! TODO: Design the function update-zipcode/address that takes an Adress and a
;;! number representing the zip code and produces a new address with the new zip code

(define (update-zipcode/address a zipcode)
  (make-address (address-num a) (address-st a) (address-city a) (address-us-state a) zipcode))

;;! Part D

;;! TODO: Design the function update-zipcode which takes an NUStudent and a
;;! number, representing the new zip code of the person and updates their permanent
;;! address to have that zip code. Be sure to follow the template!

(define (update-zipcode NUStudent zipcode)
  (make-student (student-first NUStudent) (student-last NUStudent) (student-nuid NUStudent) (student-local NUStudent) (update-zipcode/address (student-perm NUStudent) zipcode)))



;;! Problem 2

;;! You are to design a program text-mover to display and manipulate text on a
;;! background. Your program should accept some phrase to show, as well as initial
;;! location and color (we only support three: red, black, or purple) - you should
;;! then display the phrase on the screen as described.

;;! When the user presses a mouse button, the program should move the text to the
;;! location that they clicked. When the user presses a key on the keyboard, the
;;! program should rotate colors.

;;! You should at least make it through Part D,where you design the text-mover
;;! function.

;;! The following is already defined in Racket:
;;! (define-struct posn [x y])
;;! A Position is a (make-posn Real Real)
;;! Interpretation: a 2D location

;;! Part A

;;! TODO: Complete the data design recipe for Position

;; Example:
(define posn-1 (make-posn 50 50))
(define posn-2 (make-posn 20 30))
;; Template:
(define (posn-temp p)
  (...(posn-x p)...
      (posn-y p)...))

;;! Part B

;;! A RedBlackPurple (RBP) is one of:
;;! - "red"
;;! - "black"
;;! - "purple"
;;! Interpretation: available font colors

;;! TODO: Complete the data design recipe for RedBlackPurple,
;;! Use rbp-temp as the name of the template

;; Example:
(define RBP-red "red")
(define RBP-black "black")
(define RBP-purple "purple")
;; Template
;; rbp-temp: RedBlackPurple -> ?
#;
(define (rbp-temp rbp)
  (cond
    [(string=? rbp RBP-red)...]
    [(string=? rbp RBP-black)...]
    [(string=? rbp RBP-purple)...]))


;;! Part C

(define-struct tm [str pos col])
;; A TextMover (TM) is a (make-tm String Position RBP)
;; - str is the text to be displayed
;; - pos is the location of the text
;; - col is the color of the text
;; Interpretation: all the information needed for the text-mover program.

;;! TODO: Complete the data design recipe for TextMover

;; Example:
(define tm-1 (make-tm "Hi" posn-1 RBP-red))
(define tm-2 (make-tm "Hello" posn-2 RBP-black))
;;Template:
(define (tm-temp tm)
  (...(tm-str tm)...
      (posn-temp (tm-pos tm))...
      (tm-col tm)...))


;;! Part D

;;! TODO: Design the text-mover function think through the arguments to the
;;! function, how you will represent the world state, and what handlers you need
;;! to support. Actually designing the handlers will come in subsequent parts.

(define (text-mover tm)
  (big-bang
      tm
    [to-draw draw]
    [on-key key]
    [on-mouse mouse]))

;;! Part E

;;! TODO: Design a function to serve as your to-draw handler, utilizing the templates
;;! from the previous sections.

(define (draw tm)
  (place-image (text (tm-str tm) 20 (tm-col tm))
                  (posn-x (tm-pos tm)) (posn-y (tm-pos tm))
                  (square 200 "solid" "white")))


;;! Part F

;;! TODO: Design your remaining handler(s), again following the appropriate
;;! template(s).
;;! - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;;!            event, which you can check using the mouse=? function.
;;! - Hint #2: make sure to follow your templates, which may involve breaking
;;!            the handlers  into helper functions.

(define (change-color c)
   (cond
     [(string=? c RBP-red) "black"]
     [(string=? c RBP-black) "purple"]
     [(string=? c RBP-purple) "red"]))

(define (mouse tm x y event)
  (if (mouse=? "button-up" event)
      (make-tm (tm-str tm) (make-posn x y) (tm-col tm))
      tm))

(define (key tm k)
  (if(key=? k "x")
      (make-tm (tm-str tm) (tm-pos tm) (change-color (tm-col tm)))
      tm))
