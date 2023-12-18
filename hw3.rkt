;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Purpose: Recipe recipe practice, now with unions and self-referential data definitions.

(require 2htdp/image)
(require 2htdp/universe)

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

;; Consider the following structure definitions:
(define-struct blender [brand wattage crushes-ice?])
(define-struct microwave [brand power-level])
(define-struct kettle [brand capacity])
(define-struct toaster [brand slices])

;;! Part A

;; Complete four data designs for each structure called Blender, Microwave,
;; Kettle, and Toaster.

; Blender is a (make-kettle String Number Boolean)
; a blender contain its brand the amount wattage and might or might not crush ice
(define BLENDER1 (make-blender  "ninja" 100 #f))
(define BLENDER2 (make-blender  "amazon" 200 #t))

(define (blender-temp b)
  (...
   [...(blender-brand b)...]
   [...(blender-wattage b)...]
   [...(blender-crushes-ice? b)...]))

;Microwave is a (make-microwave String Number)
;a microave consits of a brand and power-level
(define MICROWAVE1 ( make-microwave "ninja" 1))
(define MICROWAVE2 ( make-microwave "amazon" 5))

(define(microwave-temp m)
  (...
   [...(microwave-brand m)...]
   [...(microwave-power-level m)...]))

;A Kettle is a (make-kettle String Number)
;a kettle consists of its brand and capicty
(define KETTLE1 (make-kettle "ninja" 1))
(define KETTLE2 (make-kettle "amazon" 5))

(define(kettle-temp k)
  (...
   [...(kettle-brand k)...]
   [...(kettle-capacity k)...]))

;A Toaster is ( make-taster String Number)
; a toaster is a brand and the number amount of slices
(define TOASTER1 (make-toaster "ninja" 1))
(define TOASTER2 (make-toaster "amazon" 5))

(define(toaster-temp t)
  (...
   [...(toaster-brand t)...]
   [...(toaster-slices t)...]))


    


;;! Part B

;; Complete a data design called Appliance, which can represent any appliance
;; listed above.

; Appliances is one of
;- Toaster
;- Microwave
;- Kettle
;- Blender
; An Appliance is ether a toaster, microwave, kettle, or blender
(define APPLIANCE1 TOASTER1)
(define APPLIANCE2 MICROWAVE1)
(define APPLIANCE3  KETTLE1)
(define APPLIANCE4  BLENDER1)

(define (appliance-temp a)
  (...
   [...(toaster? a)...]
   [...(microwave? a)...]
   [...(kettle? a)...]
   [...(blender? a)...]))



;;! Part C

;; Complete a data design called Kitchen, which may have 1--3 appliances.
;; (If you have read ahead to lists, do not use lists.)

(define-struct kitchen [appliance-3 appliance-2 appliance-1])




; A Kitchen might have
; - microwave
; - toaster
; - kettle
; - blender
; Interpertain: A kitchen can consitst with 1 to 3 appliances like a microwave,
;toaster, lettle, or blender
(define KITCHEN1 (make-kitchen MICROWAVE1 TOASTER1 BLENDER1))
(define KITCHEN2 (make-kitchen MICROWAVE2 TOASTER2 BLENDER2))
(define KITCHEN3 (make-kitchen MICROWAVE2 TOASTER2 KETTLE1))

(define(kitchen-temp k )
  (...[(kitchen-appliance-3 k)...]
      [...(kitchen-appliance-2 k )...]
      [...(kitchen-appliance-1 k )...]))
    
    

;;! Part D

;; Design a function that takes a Kitchen and produces another Kitchen
;; that is identical, except that all microwaves have their power-level
;; incremented by 50.

;Powerup; KitchenAppliance MicrowwavePowerLevel -> Appliance MicrowwavePowerLevel
; determines if a ktich appliance is a microwave and adds 50 to the powerlevel

(define (powerup appliance)
  (if
   ( microwave? appliance) (+ (microwave-power-level appliance)50)
   appliance))



; NewKitchen ; Kitchen -> ( NewKitchen make-microwave)
; takes in a kitchen and creates a new kitchen with a 50 power-level for microwaves
(check-expect (NewKitchen KITCHEN1)(make-kitchen (powerup MICROWAVE1)(powerup TOASTER1)(powerup BLENDER1)))
(check-expect (NewKitchen KITCHEN2)(make-kitchen (powerup MICROWAVE2)(powerup TOASTER2)(powerup BLENDER2)))
(check-expect (NewKitchen KITCHEN3)(make-kitchen (powerup MICROWAVE2)(powerup TOASTER2)(powerup  KETTLE1)))

(define (NewKitchen kitchen)
  (make-kitchen (powerup (kitchen-appliance-3 kitchen)) 
                (powerup (kitchen-appliance-2 kitchen))
                (powerup (kitchen-appliance-1 kitchen))))









;;! Problem 2

;; You work at a vehicle dealership, and you need to keep track of different
;; types of vehicles: cars, motorcycles, and trucks. For each car, you track
;; its brand, mileage, and number of seats. For each motorcycle, you track its
;; brand, mileage, and engine size. For each truck, you track its brand, mileage
;; and payload capacity.

;;! Part A

;; Complete a data design called Vehicle that can represent any one vehicle.

(define-struct cars (brand mileage nos))
(define-struct motorcycle( brand mileage es))
(define-struct truck( brand mileage pc))

; A Vechicle is
;- a car
;- a motorcycle
;- a truck
; Interpertation:
;a car includes a brand, mileage, number of seats and engine size
;a motorcycle has a brand, milage, and engine size
;a truck has a brand, mileage, and payload capacity

(define CAR1 (make-cars "Benz" 0 4 ))
(define CAR2 (make-cars "Benz" 100000 2))
(define CAR3 (make-cars "Benz" 100001 2))

(define MOTORCYCLE1 (make-motorcycle "Toyota" 0 2))
(define MOTORCYCLE2 (make-motorcycle "Toyota" 100000 3))
(define MOTORCYCLE3 (make-motorcycle "Toyota" 100001 4))

(define TRUCK1 (make-truck "Ford" 200 1000))
(define TRUCK2 (make-truck "Ford" 250000 2000))
(define TRUCK3 (make-truck "Ford" 260000 3000))

(define (Vehicle-temp v)
  (cond
    [(cars? v)(...(cars-brand v) ... (cars-mileage v )...(cars-nos v) ...)]
    [(motorcycle? v) (...(motorcycle-brand v) ... (motorcycle-mileage v )...(motorcycle-es v) ...)]
    [(truck? v )(...(truck-brand v) ... (truck-mileage v )...(truck-pc v) ...)]))

    
;;! Part B

;; Design a predicate called `high-mileage?` that determines if a vehicle has
;; is high mileage. Trucks are high-mileage if they have completed more than
;; 250,000 miles, but the others are high-mileage if they have completed more
;; than 100,000 miles.

;high-mileage?: Number -> Boolean
; determines if a truck has more than 250,000 miles and if a truck or car has more
; than 100,000 miles

(check-expect(high-mileage? CAR1) #f)
(check-expect(high-mileage? CAR2) #f)
(check-expect(high-mileage? CAR3) #t)

(check-expect(high-mileage? MOTORCYCLE1) #f)
(check-expect(high-mileage? MOTORCYCLE2) #f)
(check-expect(high-mileage? MOTORCYCLE3) #t)

(check-expect(high-mileage? TRUCK1) #f)
(check-expect(high-mileage? TRUCK2) #f)
(check-expect(high-mileage? TRUCK3) #t)

(define(high-mileage? Vehicle)
  (cond
    [(cars? Vehicle) (> (cars-mileage Vehicle) 100000)]
    [(motorcycle? Vehicle) (> (motorcycle-mileage Vehicle) 100000)]
    [(truck? Vehicle) ( > (truck-mileage Vehicle ) 250000)]))



    
    



;;! Part C

;; Design a function with the following signature and purpose statement:

;;! add-miles : Vehicle Number -> Vehicle
;;! Adds the given number of miles to the vehicle's mileage.

(check-expect (add-miles MOTORCYCLE1 100) (make-motorcycle "Toyota" 100 2))
(check-expect (add-miles CAR1  40) (make-cars "Benz" 40 4))
(check-expect (add-miles TRUCK1 200) (make-truck "Ford" 400 1000))

(define (add-miles Vehicle number)
  (cond
    [(cars? Vehicle) (make-cars (cars-brand Vehicle) (+ ( cars-mileage Vehicle) number) (cars-nos Vehicle))]
    [(motorcycle? Vehicle)(make-motorcycle (motorcycle-brand Vehicle) (+(motorcycle-mileage Vehicle)number) (motorcycle-es Vehicle))]
    [(truck? Vehicle )(make-truck (truck-brand Vehicle) ( + (truck-mileage Vehicle) number) (truck-pc Vehicle))]))
  
  
             
;;! Part D

;; Design a function called `describe-vehicle` takes a `Vehicle` and
;; produces one of these strings:
;; - "A car that seats <n> people!"
;; - "A motorcycle with a <n>cc engine!"
;; - "A truck that hauls <n>lbs!"


  
;Describe-Vehicle : Vehicle -> String
;Produces a a string based off the type of vehicle

(check-expect (describe-vehicle CAR1) "A car that seats 4 people!")
(check-expect(describe-vehicle MOTORCYCLE1) "A motorcycle with a 2cc engine!")
(check-expect(describe-vehicle TRUCK1) "A truck that hauls 1000lbs!")


(define (describe-vehicle Vehicle)
  (cond
    [(cars? Vehicle) (string-append "A car that seats " (number->string( cars-nos Vehicle)) " people!")]
    [(motorcycle? Vehicle) (string-append "A motorcycle with a " (number->string( motorcycle-es Vehicle)) "cc engine!")]
    [(truck? Vehicle) (string-append "A truck that hauls " (number->string( truck-pc Vehicle)) "lbs!")]))

                   

;;! Problem 3

;; Write a world program that looks and behaves approximately like this:
;;
;; https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw3_demo.gif
;;
;; The two triangles must be oriented as shown, and they must follow the mouse
;; as shown. Beyond that, feel free to be creative.
;;
;; Your world program should have the following name and signature:

;; target-program : WorldState -> WorldState
;; (define (target-program initial-state)
;;  (big-bang initial-state
;;    ...))

;; (Recall that big-bang produces the final State.)
;;
;; Furthermore:
;; 1. You can define WorldState however you like.
;; 2. When you click Run, the window must *not* appear. i.e., use
;; target-program in Interactions, and not in Definitions.

(define TRIANGLE1 (rotate 270 (triangle 50 "solid" "red")))
(define TRIANGLE2 (rotate 180 (triangle 50 "solid" "red")))
(define ES (empty-scene 300 300))







;; WorldState is a (make-posn Integer Integer)
; Interpretation: represents the mouse position 
(define POSN-0 (make-posn 0 0))
(define POSN-1 (make-posn 10 20))
(define POSN-2 (make-posn 60 60))

(define (mouseposition-temp p)
  (... (posn-x p) ... (posn-y p) ...))




; draw-trangles: MousePosition -> Image
; takes in the positions of the x and y of the mouse and creates image

(check-expect (draw-triangles POSN-0)
              (place-image TRIANGLE1
                           0
                           0
                           (place-image TRIANGLE2
                                        0
                                        0
                                        ES)))

(check-expect (draw-triangles POSN-2)
              (place-image TRIANGLE1
                           0
                           60
                           (place-image TRIANGLE2
                                        60
                                        0           
                                        ES)))


(define (draw-triangles p )
  (place-image TRIANGLE1
               0
               (posn-y p)
               (place-image TRIANGLE2
                           (posn-x p)
                            0
                            ES)))


;move-triangles:  MousePosition Int Int MouseEvent -> MousePosition
;moves triangle1 position along the y axis and triangle2 along the x axis

(check-expect (move-triangles POSN-0 0 0 "move") (make-posn 0 0))

(check-expect (move-triangles POSN-2 60 60 "move") (make-posn 60 60 ))




(define (move-triangles p x y mouse-event)
  (make-posn x y))

  




; target-program : WorldState -> WorldState
; using a mouse to move one triangle along the x-axis
; while another moves along the y-axis

(define (target-program p)
  (big-bang p
    [to-draw draw-triangles]
    [on-mouse move-triangles]))
                      














