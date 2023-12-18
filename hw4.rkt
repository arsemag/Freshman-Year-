;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Problem 1

(define-struct person [name next])
;;! A Line is one of:
;;! - "end of line"
;;! - (make-person String Line)
;;! Interpretation: A line of poeple.

(define LINE-EX-1 "end of line")
(define LINE-EX-2 (make-person "Alice" "end of line"))
(define LINE-EX-3 (make-person "Bob" (make-person "Alice" "end of line")))
(define LINE-EX-4 (make-person "Waldo" (make-person "Alice" (make-person "Bob" "end of line"))))
;;! Part A

;; Write the template for Line.

(define (line-temp n)
  (...(cond [(string? n)...]
    [(person? n)
     ...(person-name n )
     (line-temp (person-next n))])))



;;! Part B

;; Design a function called count-people that counts the number of people in
;; a line.

;Count-People: Person -> Number
; counts the amount of people in a line

(check-expect (count-people LINE-EX-1) 0)
(check-expect (count-people LINE-EX-2) 1)
(check-expect (count-people LINE-EX-3) 2)


(define (count-people n)
  (cond
    [(string? n) 0]
    [(person? n)
    (add1 (count-people (person-next n)))]))


;;! Part C

;; Design a predicate called waldo-in-line? that determines if a person named
;; "Waldo" is in the line.

; WaldoInLine? : Line -> Boolean
; informs if there is a "Waldo" in the line 

(check-expect (waldo-in-line? LINE-EX-1) #f)
(check-expect (waldo-in-line? LINE-EX-2) #f)
(check-expect (waldo-in-line? LINE-EX-4) #t)

(define (waldo-in-line? p)
  (cond
    [(string? p)#f]
    [(person? p)
    (or (string=? (person-name p ) "Waldo")
     (waldo-in-line? (person-next p)))]))




;;! Part D

;; Design a function that removes the first "Waldo" in the line. It should have the
;; signature remove-waldo : Line -> Line 




;Remove-Waldo : Line -> Line
; removes the first Waldo in line

(check-expect (remove-waldo LINE-EX-1) LINE-EX-1)
(check-expect (remove-waldo LINE-EX-2) LINE-EX-2)
(check-expect (remove-waldo LINE-EX-4) (make-person "Alice"(make-person "Bob" "end of line")))

(define (remove-waldo line)
  (cond
    [(string? line) line]
    [(person? line)
     (if
      (string=? (person-name line) "Waldo")
      (person-next line)
      (make-person (person-name line)
      (remove-waldo (person-next line))))])) 


    



;;! Problem 2


(define-struct quest-entry [name completed next])
;;! A QuestLog is one of:
;;! - "empty"
;;! - (make-quest-entry String Boolean QuestLog)
;;! Interpretation: A quest log where each entry contains a quest name and a
;;! boolean that indicates if that quest is completed.

;;! Part A

;; Complete the data design for QuestLog (examples and template)

(define QL0 "empty")
(define QL1 (make-quest-entry "Stop" #t QL0))
(define QL2 (make-quest-entry "RainForest" #f QL1))
(define QL3 (make-quest-entry "Fire" #t QL2))


(define (questlog-temp q)
  (...(cond
    [(string? q)...]
    [(and
     (quest-entry? q)
     (quest-entry-name q)
     (quest-completed? q)
     (questlog-temp (quest-entry-next q)))])))




;;! Part B

;; Design a function called count-completed-quests that counts the number of
;; completed quests in a quest log.






; CountCompletedQuests; QuestLog -> Number
; counts the amount of of completed quest longs

(check-expect (count-completed-quests QL0 ) 0)
(check-expect (count-completed-quests QL1 ) 1)
(check-expect (count-completed-quests QL3 ) 3)

(define (count-completed-quests questlog)
  (cond
    [(string? questlog) 0]
    [(quest-entry? questlog)
    (add1(count-completed-quests (quest-entry-next questlog)))]))



;;! Part C

;; Design a function that consumes a QuestLog and only produces the incomplete
;; quests. It should have the signature incomplete-quests : QuestLog -> QuestLog.

; IncompleteQuests : QuestLog -> QuestLog
; produces a queslog that has the incomplete quests

(check-expect(incomplete-quests QL0 )"empty")
(check-expect(incomplete-quests QL1 )"empty" )
(check-expect(incomplete-quests QL2 )(make-quest-entry "RainForest" #false "empty"))


(define (incomplete-quests questlog)
  (cond
    [(string? questlog)"empty"]
    [else
     (if
     (quest-entry-completed questlog)
     (incomplete-quests (quest-entry-next questlog))
     (make-quest-entry (quest-entry-name questlog) #f
     (incomplete-quests (quest-entry-next questlog))))])) 



;;! Part D

;; Design a function that consumes a QuestLog and produces a new QuestLog with
;; the same quests, but all marked completed.

;MarkCompleted; Questlog -> Completed Questlog
;marks all quests completed 


(check-expect(mark-complete QL0 )"empty")
(check-expect(mark-complete QL1 )(make-quest-entry "Stop" #t "empty"))
(check-expect(mark-complete QL2 )(make-quest-entry "RainForest" #true (make-quest-entry "Stop" #true "empty")))


(define (mark-complete questlog) 
    (cond 
        [(string? questlog) "empty"]
        [else
        (make-quest-entry (quest-entry-name questlog) #t 
                  (mark-complete (quest-entry-next questlog)))]))







;;! Problem 3

;; This problem has a partially-completed data definition that represents a
;; workout sequence.

(define-struct cardio [rest])
(define-struct strength [rest])
(define-struct flexibility [rest])
;;! A Workout is one of:
;;! - (make-cardio Workout)
;;! - (make-strength Workout)
;;! - (make-flexibility Workout)
;;! - "done"
;;! Interpretation: A list of exercises in a long workout.



;;! Part A

;; Give three examples of Workouts.


(define W0 "done")
(define W1 (make-cardio "done"))
(define W2 (make-strength W1))
(define W3 (make-flexibility W2 ))



;;! Part B

;; Write the template for Workouts.


(define (Workout-temp w)
  (...(cond
    [(string? w)...]
    [(cardio? w) (cardio-rest w)(Workout-temp (cardio-rest w))]
    [(strength? w)(strength-rest w)(Workout-temp (strength-rest w))]
    [(flexibility? w)(flexibility-rest w)
    (Workout-temp (flexibility-rest w))])))
   



;;! Part C

;; Design a function called recovery-sequence to generate the "recovery" sequence for a given
;; Workout. In the recovery sequence, cardio exercises become flexibility
;; exercises, strength exercises become cardio exercises, and flexibility
;; exercises become strength exercises.

; RecoverySquence : Workout -> Workout 
; takes in a cardio excercises to become flexibility excercises
; takes in a strength excercises to  become cardio excercises
; takes in a fliexibity excercises to become strength excercises

(check-expect (recovery-squence W0) "done")
(check-expect (recovery-squence W1) (make-flexibility "done"))
(check-expect (recovery-squence W2) (make-cardio (make-flexibility "done")))
(check-expect (recovery-squence W3) (make-strength (make-cardio (make-flexibility "done"))))

(define (recovery-squence w)
   (cond
    [(string? w) "done" ]
    [(cardio? w) (make-flexibility (recovery-squence (cardio-rest w)))]
    [(strength? w) (make-cardio (recovery-squence (strength-rest w)))]
    [(flexibility? w) (make-strength (recovery-squence (flexibility-rest w)))]))















