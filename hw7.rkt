;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Instructions                                                           ;;
;; 1. You must use list abstractions to receive credit. Do not write directly ;;
;;    recursive functions.                                                    ;;
;; 2. You may use Lambda if you wish.                                         ;;
;; 3. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This homework refers to the following data designs.

;;! A Category is one of:
;;! - "Personal"
;;! - "Work"
;;! - "Academic"
;;! Interpretation: a category of tasks in a task list.
(define PERSONAL "Personal")
(define WORK "Work")
(define ACADEMIC "Academic")

(define (category-template cat)
  (cond
    [(string=? cat PERSONAL) ...]
    [(string=? cat WORK) ...]
    [(string=? cat ACADEMIC) ...])) 

(define-struct task [category description priority])
;; An Task is (make-task Category String Number)
;; Interpretation: A task in a task list, with its category, description, and
;; priority. Lower numbers are more urgent.
(define EX-ASSIGNMENT (make-task ACADEMIC "Finish HW7" 0))
(define EX-LIBRARY (make-task WORK "Finishing shelving books in Snell" 10)) 
(define EX-PERSONAL (make-task PERSONAL "Do laundry this time" 20))

(define (task-template t)
  (... (task-category t) ... (task-description t) ... (task-priority t) ...))







;;! Problem 1

;; Design a function called priority-zero that consumes a list of tasks and
;; only produces those with priority 0.

;;! priority-zero : [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority 0.

(check-expect (priority-zero (list EX-ASSIGNMENT)) (list EX-ASSIGNMENT))

(check-expect (priority-zero (list EX-LIBRARY EX-ASSIGNMENT)) (list EX-ASSIGNMENT))
              

(check-expect (priority-zero (list EX-LIBRARY EX-ASSIGNMENT EX-PERSONAL))
                             (list EX-ASSIGNMENT))



(define (priority-zero lot)
  (filter (lambda (p) (= (task-priority p) 0 )) lot))






;;! Problem 2

;; Design a function called priority<= that consumes a priority number and
;; a list of tasks and produces only those with priority less than or equal
;; to the given number.


;;! priority<= : Number [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority less than or equal to the given
;;! number.

(check-expect (priority<= 0 (list EX-ASSIGNMENT EX-LIBRARY )) (list EX-ASSIGNMENT ))

(check-expect (priority<= 40 (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))

(check-expect (priority<= 10 (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list EX-ASSIGNMENT EX-LIBRARY))


(define (priority<= number lot)
 (filter (lambda (x) (<= (task-priority x) number)) lot))



;;! Problem 3

;; Design a function called prioritize that consumes a category and a list of
;; tasks, and sets the priority of all tasks in the given category to 0. The
;; produced list should contain all tasks in the original list.

;;! prioritize : Category [List-of Task] -> [List-of Task]
;;! Produces every task in the given list of tasks. But, sets the priority of
;;! tasks in the given category to zero.

(check-expect (prioritize (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
                      (list (make-task ACADEMIC "Finish HW7" 0)
                            (make-task WORK "Finishing shelving books in Snell" 0)
                            (make-task PERSONAL "Do laundry this time" 0)))
  
(check-expect (prioritize '()) '())

(check-expect (prioritize (list EX-PERSONAL EX-PERSONAL))
                         (list  (make-task PERSONAL "Do laundry this time" 0)
                            (make-task PERSONAL "Do laundry this time" 0)))
                             

(define (prioritize lot)
  (local
      [(define (zero task)
 (make-task (task-category task )
            (task-description task)
             0))]
  (map zero lot)))
     


;; Zero : Number -> Number
;; takes in priority number and produce a zero

#;(define (zero task)
 (make-task (task-category task )
            (task-description task)
             0))

                    



;;! Problem 4

;; Design a predicate called any-work? that determines if any task in a list
;; is a work task.

;;! any-work? : [List-of Task] -> Boolean
;;! Determines if any task in the given list is a work task.

(check-expect (any-work? (list EX-ASSIGNMENT EX-PERSONAL EX-LIBRARY)) #t)
(check-expect (any-work? (list EX-ASSIGNMENT EX-PERSONAL )) #f)
(check-expect (any-work? '()) #f)

(check-expect (any-work? (list EX-LIBRARY EX-LIBRARY EX-LIBRARY)) #t)

(define (any-work? lot)
 (ormap (lambda (x) (string=? (task-category x) "Work")) lot)) 




;;! Problem 5

;; Design a function called count-academic that consumes a list of tasks and
;; produces the number of tasks in the list that are academic tasks.

;;! count-academic : [List-of Task] -> Number
;;! Produces the number of tasks in the given list that are academic tasks.

(check-expect (count-academic (list EX-ASSIGNMENT EX-PERSONAL EX-LIBRARY))
                          1)

(check-expect (count-academic (list EX-ASSIGNMENT EX-PERSONAL ))
                          1)
(check-expect (count-academic '()) 0)


(define (count-academic lot)
 (length (filter (lambda (x)  (string=? (task-category x) "Academic")) lot)))
 


    
   

  


;;! Problem 6

;; Design a function called search that consumes a list of tasks and a string
;; and produces a list of tasks whose description contains the given string.

;;! search : String [List-of Task] -> [List-of Task]
;;! Produces a list of tasks whose description contains the given string.

(check-expect (search "Finish HW7" (list EX-ASSIGNMENT EX-ASSIGNMENT EX-ASSIGNMENT
                                         EX-LIBRARY EX-PERSONAL))
                      (list EX-ASSIGNMENT EX-ASSIGNMENT EX-ASSIGNMENT))
                            
                            
(check-expect (search "Finishing shelving books in Snell" (list EX-LIBRARY
                                         EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
                      (list EX-LIBRARY EX-LIBRARY))


(check-expect (search "Do laundry this time" (list EX-PERSONAL EX-PERSONAL EX-LIBRARY EX-LIBRARY))
                         (list EX-PERSONAL EX-PERSONAL))



(define (search string lot)
 (filter (lambda (x) (string-contains? (task-description x) string)) lot))
 


                             

;;! Problem 7

;; Design a function called search-work that consumes a string and produces
;; the descriptions of the work tasks that contain the given string.

;;! search-work : String [List-of Task] -> [List-of String]
;;! Produces a list of descriptions of work tasks that contain the given string.


(check-expect (search-work "Finishing shelving books in Snell" (list EX-LIBRARY EX-PERSONAL))
              (list "Finishing shelving books in Snell"))

(check-expect (search-work "Finishing shelving books in Snell" (list (make-task WORK "Finishing shelving books in Snell" 10)
                                                                     (make-task PERSONAL "Do laundry" 20)
                                                                     (make-task ACADEMIC "Finish HW7" 0)))
                                                               (list "Finishing shelving books in Snell" ))
                      

(check-expect (search-work "Do laundry this time" (list EX-PERSONAL EX-PERSONAL EX-LIBRARY EX-LIBRARY))
                         (list "Do laundry this time" "Do laundry this time"))


(define (search-work string lot)
  (map task-description (filter (lambda (x) (and (string=? (task-category x) "Work")
                        (string-contains? (task-description x) string))) lot))) 








