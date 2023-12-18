;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
;; Purpose: Recipe recipe practice, now with structured data.

;;! Instructions
;;! 1. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 2. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).
;;! 3. You must follow the Style Guide:
;;!    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;;! 4. You must submit working code. In DrRacket, ensure you get on errors
;;!    when you click Run. After you submit on Gradescope, you'll get instant
;;!    feedback on whether or Gradescope can run your code, and your code must
;;!    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the following data definition and interpretation.

(define-struct time (hours minutes seconds))
;;! A Time is a (make-time Number Number Number)
;;! Represents the time of day where:
;;! – hours is between 0 and 23
;;! – minutes is between 0 and 59
;;! – seconds is between 0 and 59

;;! Part A
;; Complete the two remaining parts of the data design for Time.
;(define TIME (make-time HOURS MINUTES SECONDS))
(define TIME1 (make-time 23 59 59))
(define TIME2 (make-time 4 30 20))
(define TIME3 (make-time 10 5 10))
(define TIME4 (make-time 10 59 59))
(define TIME5 (make-time 10 5 59))

(define (time-temp time)
  (... (time-hours time) ... (time-minutes time) ... (time-seconds time) ...))

;;! Part B
;; Design a function called tick that adds one second to a Time.
; tick : Time -> Time
; adds one second to a Time
(check-expect (tick TIME1) (make-time 0 0 0))
(check-expect (tick TIME2) (make-time 4 30 21))
(check-expect (tick TIME3) (make-time 10 5 11))
(check-expect (tick TIME4) (make-time 11 0 0))
(check-expect (tick TIME5) (make-time 10 6 0))

(define (tick time)
  (cond [(and (= (time-hours time) 23) (= (time-minutes time) 59) (= (time-seconds time) 59))
         (make-time 0 0 0)] 
        [(and (= (time-minutes time) 59) (= (time-seconds time) 59))
         (make-time (add1 (time-hours time)) 0 0)] 
        [(= (time-seconds time) 59)
         (make-time (time-hours time) (add1 (time-minutes time)) 0)] 
        [else (make-time(time-hours time) (time-minutes time) (add1 (time-seconds time)))]))

;;! Part C

;; Design a function called time->image that draws an analog clock face with
;; three hands. (The hour hand is shortest and the minute and second hand should
;; be different.)
;;
;; See the link below for a refresher on how an analog clock works
;; https://en.wikipedia.org/wiki/Clock_face
;; Note: The hour hand does not need to base it's position on the minute hand
;; for this assignment

(define CLOCK (circle 100 "solid" "white"))
(define SECONDS (overlay/offset
  (line 0 100 "red")
  0 50
  (circle 100 "solid" "transparent")))
(define MINUTES (overlay/offset
  (line 0 80 "black")
  0 40
  (circle 100 "solid" "transparent")))
(define HOURS (overlay/offset
  (line 0 50 "black")
  0 25
  (circle 100 "solid" "transparent")))

; time->image : Time -> Image
; draws an analog clock that correlates with time given
(check-expect (time->image TIME1)
              (overlay/offset
                 (rotate -354 SECONDS)
                 0 0
                 (overlay/offset
                  (rotate -354 MINUTES)
                  0 0
                  (overlay/offset
                   (rotate -690 HOURS)
                   0 0
                   CLOCK))))

(check-expect (time->image TIME2)
              (overlay/offset
                 (rotate -120 SECONDS)
                 0 0
                 (overlay/offset
                  (rotate -180 MINUTES)
                  0 0
                  (overlay/offset
                   (rotate -120 HOURS)
                   0 0
                   CLOCK))))

(check-expect (time->image TIME3)
              (overlay/offset
                 (rotate -60 SECONDS)
                 0 0
                 (overlay/offset
                  (rotate -30 MINUTES)
                  0 0
                  (overlay/offset
                   (rotate -300 HOURS)
                   0 0
                   CLOCK))))

(define (time->image time)
  (overlay/offset
   (rotate (* -6 (time-seconds time)) SECONDS)
   0 0
   (overlay/offset
    (rotate (* -6 (time-minutes time)) MINUTES)
    0 0
    (overlay/offset
     (rotate (* -30 (time-hours time)) HOURS)
     0 0
     CLOCK))))

;;! Problem 2

;;! Part A

;; You are a feared restaurant critic whose ratings can make or break the
;; restaurants in Boston. Design a data definition called Review
;; that represents your review of a single restauant. A Review holds the
;; restaurant's name, its cuisine, the dish you ordered, its price, your
;; rating (1--5), and whether or not you saw any rats.

(define-struct review [name cuisine dish price rating rats?])
; A review is a [String, String, String, String, Number, Number, Boolean]
  
; A Review is a (make-review String String String Number Number Boolean)
; Interpretation: a review of a restaurant including the restaurant's name, its cuisine,
; the dish you ordered, its price, your rating (1--5) and whether or not you saw any rats
(define R1 (make-review "El Jefe's" "Mexican" "taco" 4 5 #f))
(define R2 (make-review "Cheesecake Factory" "Italian" "pasta" 18 3 #f))
(define R3 (make-review "Wendy's" "American" "burger" 5 2 #t))

(define (review-temp r)
  (... (review-name r) ... (review-cuisine r) ... (review-dish r) ... (review-price) ...
       (reivew-rating r) ... (review-rats? r) ...))

;;! Part B

;; Design a function called update-rating that takes a Review and a new rating,
;; and updates the review with the new rating.

; update-rating : Review Number -> Review
; takes in review and new rating and outputs review with new rating
(check-expect (update-rating R1 3) (make-review "El Jefe's" "Mexican" "taco" 4 3 #f))
(check-expect (update-rating R2 5) (make-review "Cheesecake Factory" "Italian" "pasta" 18 5 #f))
(check-expect (update-rating R3 4) (make-review "Wendy's" "American" "burger" 5 4 #t))

(define (update-rating review rating)
  (make-review (review-name review) (review-cuisine review) (review-dish review)
               (review-price review) rating (review-rats? review)))

;;! Part C

;; Design a function called rat-sighting that takes a Review and marks it as
;; a restaurant with rats. It also decreases its rating by 1 star, only if
;; the restaurant was not previously known to have rats.

; rat-sighting : Review -> Review
; takes in review, marks it as a restaurant with rats, and decreases rating by 1 star
(check-expect (rat-sighting R1) (make-review "El Jefe's" "Mexican" "taco" 4 4 #t))
(check-expect (rat-sighting R2) (make-review "Cheesecake Factory" "Italian" "pasta" 18 2 #t))
(check-expect (rat-sighting R3) (make-review "Wendy's" "American" "burger" 5 2 #t))

(define (rat-sighting review)
  (cond[(boolean=? (review-rats? review) #f)
       (make-review (review-name review) (review-cuisine review) (review-dish review)
       (review-price review) (sub1 (review-rating review)) #t)]
       [else (make-review (review-name review) (review-cuisine review) (review-dish review)
       (review-price review) (review-rating review) (review-rats? review))]))

;;! Problem 3

;; You are in the robot part business, making essential parts for robots.
;; The only parts you make are LIDAR sensors, depth cameras, accelerometers,
;; electric motors, and batteries. For every part, you track the kind of
;; part, the price of the item, and the number of items in stock.

;;! Part A

;; Design data definitions called PartType and Stock to represent
;; a single type of item in stock.

; A Stock is a (make-stock String Number Number)
(define-struct stock [part-type price number])

(define STOCK1 (make-stock "LIDAR sensors" 9 100))
(define STOCK2 (make-stock "depth cameras" 15 200))
(define STOCK3 (make-stock "accelerometers" 20 100))
(define STOCK4 (make-stock "electric motors" 30 150))
(define STOCK5 (make-stock "batteries" 5 100))
(define STOCK6 (make-stock "batteries" 5 100))

; A PartType is a String in:
; - "LIDAR sensors"
; - "depth cameras"
; - "accelerometers"
; - "electric motors"
; - "batteries"
; Interpretation: A part type is a type of robot part
(define LIDAR-SENSORS "LIDAR sensors")
(define DEPTH-CAMERAS "depth cameras")
(define ACCELEROMETERS "accelerometers")
(define ELECTRIC-MOTORS "electric motors")
(define BATTERIES "batteries")

(define (stock-temp s)
  (... (stock-part-type s) ... (stock-price s) ... (stock-number s) ...))

;;! Part B

;; Design a function called discount that takes an Stock and a discount
;; value, and reduces the price by the given value. However, if the price
;; is lower than $10, do not apply the discount. You can assume that the
;; discount is less than the original price.

; discount : Stock Number -> Number
; takes a stock and discount value and outputs discounted value if price is >$10
(check-expect (discount STOCK1 5) 9)
(check-expect (discount STOCK2 5) 10)
(check-expect (discount STOCK3 10) 10)
(check-expect (discount STOCK4 3) 27)
(check-expect (discount STOCK5 5) 5)

(define (discount stock discount-value)
  (cond[(< (stock-price stock) 10)
        (stock-price stock)]
       [(>= (stock-price stock) 10)
        (- (stock-price stock) discount-value)]))

;;! Part C

;; Design a function called greater-value? that takes two Stocks and
;; produces #true iff the value (quantity * price) of the first is greater than
;; or equal to the value of the second.
;; Note: To receive full credit, you will need to write a helper function that
;; follows the template.

; value : Stock -> Number
; takes in a stock and gives value based on quantity and price
(check-expect (value STOCK1) 900)
(check-expect (value STOCK2) 3000)
(check-expect (value STOCK3) 2000)

(define (value stock)
  (* (stock-price stock) (stock-number stock)))

; greater-value? : Stock Stock -> Boolean
; takes two stocks and returns true iff value of first >= value of second
(check-expect (greater-value? STOCK1 STOCK2) #f)
(check-expect (greater-value? STOCK2 STOCK3) #t)
(check-expect (greater-value? STOCK1 STOCK5) #t)
(check-expect (greater-value? STOCK5 STOCK6) #t)

(define (greater-value? stock1 stock2)
  (cond[(>= (value stock1) (value stock2)) #t]
       [(< (value stock1) (value stock2)) #f]))
