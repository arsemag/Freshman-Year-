;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Purpose: An introduction to data design (enumerations) and the design recipe.

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 3. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).

;;! Problem 1

;; Design a function called concat-space-separator-when-long that consumes two
;; strings and produces a single string that concatenates them. In the result,
;; the two strings should be separated by a space *only if* the first string
;; is longer than 5 characters.

; concat-space-separator-when-long : includes two sperate words or numbers 

;combines strings if first string is less than 5 characters
;if not seperates with a space

(define WORD1 "took" )
(define WORD2 "seventeen")
(define WORD3 "orange blue")


#;(define (concat-space-separator-when-long-temp x y)
                (cond
                  [ (< (string-length x) 5) ...]
                  [ (= (string-length x ) 5)...]
                  [ (> (string-length x ) 5)...]))

;concat-space-separator-when-long: String String->String
;if a x charcter is word then grater than 5
;sperate x and y if less than 5 then combine x and y

(check-expect(concat-space-separator-when-long "to" "ok") WORD1)
(check-expect(concat-space-separator-when-long "seven" "teen") WORD2)
(check-expect(concat-space-separator-when-long "orange" "blue") WORD3)
   

(define (concat-space-separator-when-long x y)
                (cond
                  [ (< (string-length x) 5 )(string-append x y)]
                  [ (= (string-length x ) 5 )(string-append x y)]
                  [ (> (string-length x ) 5 )(string-append x " " y)]))
                  
                                    
                 
              
;;! Problem 2

;;! Part A

;; Our solar systems traditionally had nine planets. Look them up, and
;; write a data definition called Planet that can represent any one of them.
;; NOTE: name your template planet-template.

;A Planet in our solor system is one of:
;-mercury
;-venus
;-earth
;-mars
;-jupiter
;-saturn
;-uranus
;-neptune
;-pluto



; Planet :  String->String
; to identify the nine planet in our the solar system

  (define MERCURY "mercury")
  (define VENUS  "venus")
  (define EARTH "earth" )
  (define MARS "mars")
  (define JUPITER "jupiter")
  (define SATURN "saturn")
  (define URANUS "uranus" )
  (define NEPTUNE "neptune")
  (define PLUTO "pluto") 

#;(define(planet-temp p)
  (cond
    [(... p MERCURY)...]
    [(... p VENUS)...]
    [(... p EARTH)...]
    [(... p MARS)...]
    [(... p JUPITER)...]
    [(... p SATURN)...]
    [(... p URANUS)...]
    [(... p NEPTUNE)...]
    [(... p PLUTO)...]))

                   
  



;;! Part B

;; One way to classify planets is as either terrestrial, gas giant, or dwarf planet.
;; Design a function called planet-kind that consumes a Planet and produces either
;; "terrestrial", "gas giant", or "dwarf planet".



;planet-kind includes "terrestrial", "gas giant", or "dwarf planet"
; 


(define MErcury "terrstrial")
(define VEnus "terrstrial")
(define EArth  "terrstrial")
(define MArs "terrstrial ")
(define JUpiter "gas gaint")
(define SAturn "gas gaint")
(define URanus "gas gaint")
(define NEptune "gas gaint")
(define PLuto "dwarf planet")


#;(define(planet-kind-temp k)
  (cond
   [(... k MErcury)...]
   [(... k VEnus)...]
   [(... k MArs)...]
   [(... k EArth)...]
   [(... k MArs)...]
   [(... k JUpiter)...]
   [(... k SAturn)...]
   [(... k URanus)...]
   [(... k NEptune)...]
   [(... k PLuto)...]))

;planet-kind: String-> String
; to inform which planet is either terrestrial, gas gaint, and dward planet


(check-expect(planet-kind MErcury) "terrestrial")
(check-expect(planet-kind VEnus) "terrestrial")
(check-expect(planet-kind EArth) "terrestrial")
(check-expect(planet-kind MArs) "terrestrial")
(check-expect(planet-kind JUpiter) "gas gaint")
(check-expect(planet-kind SAturn) "gas gaint")
(check-expect(planet-kind URanus) "gas gaint")
(check-expect(planet-kind NEptune) "gas gaint")
(check-expect(planet-kind PLuto) "dwarf planet")




(define(planet-kind k)
  (cond
   [(string=? k MErcury)"terrestrial"] 
   [(string=? k VEnus)"terrestrial"]
   [(string=? k EArth)"terrestrial"]
   [(string=? k MArs)"terrestrial"]
   [(string=? k JUpiter) "gas gaint"]
   [(string=? k SAturn) "gas gaint"]
   [(string=? k URanus)"gas gaint"]
   [(string=? k NEptune)"gas gaint"]
   [(string=? k PLuto) "dwarf planet"]))


  
             
;;! Part C

;; Design a predicate called has-moons? that produces true if a planet has any
;; moons.

;has-moons in our solor system:
;false-mercury 
;false-venus 
;true-earth 
;true-mars 
;true-jupiter 
;true-saturn 
;true-uranus
;true-neptune
;true-pluto

;interpertaion :
;which planets has moons and does not have planet 


;examples:
(define Mercury "mercury")
(define Venus  "venus")
(define Earth "earth" )
(define Mars "mars")
(define Jupiter "jupiter")
(define Saturn "saturn")
(define Uranus "uranus" )
(define Neptune "neptune")
(define Pluto "pluto") 

;template
#;(define (has-moons-temp m)
  (cond
   [(... m Mercury)...] 
   [(... m Venus)...]
   [(... m Earth)...]
   [(... m Mars)...]
   [(... m Jupiter)...]
   [(... m Saturn)...]
   [(... m Uranus)...]
   [(... m Neptune)...]
   [(... m Pluto) ...]))
  


;HasMoons : String -> Boolean
;determines if planets in our solor system
;has any moons

(check-expect(has-moons? Mercury) #false)
(check-expect(has-moons? Venus) #false)
(check-expect(has-moons? Earth) #true)
(check-expect(has-moons? Mars) #true)
(check-expect(has-moons? Jupiter) #true)
(check-expect(has-moons? Saturn) #true)
(check-expect(has-moons? Uranus) #true)
(check-expect(has-moons? Neptune) #true)
(check-expect(has-moons? Pluto) #true)
        
(define (has-moons? m)
  (cond
   [(string=? m Mercury) #false] 
   [(string=? m Venus)#false]
   [(string=? m Earth)#true]
   [(string=? m Mars)#true]
   [(string=? m Jupiter)#true]
   [(string=? m Saturn)#true]
   [(string=? m Uranus)#true]
   [(string=? m Neptune)#true]
   [(string=? m Pluto) #true]))
  






;;! Problem 3

;;! Part A

;; Design a data definition called RainbowColor that represents a color of the
;; rainbow. To avoid ambiguity, use the "modern" colors from this Wikipedia page:
;; https://en.wikipedia.org/wiki/Rainbow
;; NOTE: call your template rainbow-color-template.


;RainbowColor :
;includes red,orange, yellow,green,cyan, blue, and violet
;the seven colors in the rainbow


(define RED "red")
(define ORANGE  "orange")
(define YELLOW "yellow" )
(define GREEN "green")
(define CYAN "cyan")
(define BLUE "blue")
(define VIOLET "violet")

#;(define (rainbow-color-template c)
  (cond
    [(... c RED)...]
    [(... c ORANGE)...]
    [(... c YELLOW)...]
    [(... c GREEN)...]
    [(... c CYAN)...]
    [(... c BLUE)...]
    [(... c VIOLET)...]))
  





;;! Part B

;; Design a predicate called primary? to determine if a RainbowColor is a primary
;; color (red, yellow, or blue).

;primary? : String-> Boolean
;Purpose : to determine if the colors in the rainbow
; are primary colors


(check-expect(primary? RED) #true)
(check-expect(primary? ORANGE) #false)
(check-expect(primary? YELLOW) #true)
(check-expect(primary? GREEN) #false)
(check-expect(primary? CYAN) #true)
(check-expect(primary? BLUE) #true)
(check-expect(primary? VIOLET) #false)


(define (primary? p)
  (cond
    [(string=? p RED) #true]
    [(string=? p ORANGE)#false]
    [(string=? p YELLOW) #true]
    [(string=? p GREEN) #false]
    [(string=? p CYAN) #true]
    [(string=? p BLUE)#true]
    [(string=? p VIOLET)#false]))
             




;;! Part C

;; Design a function called next-color that consumes a RainbowColor and produces
;; the next color, where next goes from outside to inside of a rainbow. When
;; applies to the innermost color (violet), it produces the outermost color (red).


;Signature: String-> String
; gives the order of the rainbow outside
;to inside the restarts

#;(define(next-color-temp n)
  (cond
    [(string=? n RED)...]
    [(string=? n ORANGE)...]
    [(string=? n YELLOW)...]
    [(string=? n GREEN)...]
    [(string=? n CYAN)...]
    [(string=? n BLUE)...]
    [(string=? n VIOLET)...]))

(check-expect(next-color RED) ORANGE)
(check-expect(next-color ORANGE) YELLOW)
(check-expect(next-color YELLOW) GREEN)
(check-expect(next-color GREEN) CYAN)
(check-expect(next-color CYAN) BLUE)
(check-expect(next-color CYAN) BLUE)
(check-expect(next-color VIOLET) RED)


(define(next-color n)
  (cond
    [(string=? n RED) ORANGE]
    [(string=? n ORANGE)YELLOW]
    [(string=? n YELLOW)GREEN]
    [(string=? n GREEN) CYAN]
    [(string=? n CYAN)BLUE]
    [(string=? n BLUE) VIOLET]
    [(string=? n VIOLET)RED]))
   





























