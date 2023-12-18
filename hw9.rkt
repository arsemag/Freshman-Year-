;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw9(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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
;; 1. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;; 2. When we write "complete the following function design", you should      ;;
;;    write the function definition and check-expects.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;! Problem 1

;; Complete the following function design.

;;! average-of-two-lists : [List-of Number] [List-of Number] -> [List-of Number]
;;! Produces a list of numbers where each number is the average of the
;;! corresponding numbers in the two lists.





(define (average-of-two-lists lon lon2)
  (map (lambda (x y) (/ (+ x y) 2)) lon lon2))

(check-expect (average-of-two-lists (list 1 2 3) (list 4 5 6)) (list 2.5 3.5 4.5))
(check-expect (average-of-two-lists (list 5 10 15) (list 20 25 30)) (list 12.5 17.5 22.5))
(check-expect (average-of-two-lists (list 10 20 30) (list 40 50 60)) (list 25 35 45))



;;! Problem 2

;; Complete the following function design *without using the builtin function
;; replicate*.

;;! repeat-strings-solo : Nat [List-of String] -> [List-of String]
;;! (repeat-strings-solo n slist) produces a produces a list of strings where
;;! each output string is the corresponding input string repeated n times.




(define (repeat-strings-solo n los)
  (cond
    [(empty? los) empty]
    [else (append (make-list n (first los)) (repeat-strings-solo n (rest los)))]))

(check-expect (repeat-strings-solo 3 (list "Hello")) (list "Hello" "Hello" "Hello"))
(check-expect (repeat-strings-solo 4 (list "World")) (list "World" "World" "World" "World"))
(check-expect (repeat-strings-solo 0 (list "Repeat me!")) '())







;;! Problem 3

;; Complete the following function design *and you may use the builtin
;; replicate.*

;;! repeat-strings : [List-of String] [List-of Nat] -> [List-of String]
;;! (repeat-strings slist nlist) produces a list of strings from slist, where
;;! each is duplicated N times, where N is the corresponding number in
;;! nlist. However:
;;!
;;! 1. If there  are more strings than numbers, assume that the extra strings
;;!    should be repeated twice each.
;;! 2. If there are more numbers than strings, for each extra number N,
;;!    repeat the the string "Extra!" N times.

;(check-expect (repeat-strings '() '()) '())

;(check-expect (repeat-strings '() (list 2)) (list " Extra! Extra!"))
;(check-expect (repeat-strings (list "school" "paper" ) (list 2 )) (list "school school " "paper paper "))
;(check-expect (repeat-strings (list "sun" "earth" "water" ) (list 1 2 3)) (list "sun " "earth earth " "water water water "))





 (define (repeat-strings los lon)
  (local
    [;;replicate-helper: Number String -> String
      ;; determines if the number needs repeaded or not 
   (define (replicate-helper n s)
    (if (zero? n)
    ""
    (string-append s "" (replicate-helper (- n 1) s))))] 
  (cond
    [(and (empty? los) (empty? lon)) '()]
    [(and (empty? los) (cons? lon)) (cons (replicate (first lon) " Extra!") (repeat-strings  (rest lon) los))] 
    [(and (cons? los) (empty? lon)) (cons (replicate-helper 2 (first los))  (repeat-strings (rest los) lon))]
    [(and (cons? los) (cons? lon)) (cons (replicate-helper (first lon) (first los)) (repeat-strings (rest los)(rest lon )))])))
                                   

                                                                   
    





;;! Problem 4

;; Consider the following data definitions (we have omitted examples and
;; templates).

(define-struct student [name nuid])
;;! A Student is a (make-student String Number)
;;! Interpretation: represents a student

(define STUDENT1 (make-student "James" 123456))
(define STUDENT2 (make-student "Smith" 654321))
(define STUDENTLIST (list (make-student "James" 123456) (make-student "Smith" 654321)))
 



(define-struct grade [nuid course value])
;;! A Grade is a (make-grade Number String Number)
;;! (make-grade nuid course grade) represents the grade that
;;! a student received in a course.

(define GRADE1 (make-grade 123456 "FUNDIES" 98))
(define GRADE2 (make-grade 123456 "MATH" 84))
(define GRADELIST (list (make-grade 123456 "FUNDIES" 98 ) (make-grade 123456 "MATH" 84) (make-grade 654321 "FUNIDES" 86) (make-grade 654321 "MATH" 90)))




(define-struct student-grades [name grades])
;;! A StudentGrades is a (make-student-grades String [List-of Number]).
;;! (make-student-grades name grades) represents the grades
;;! that a student has received in all courses.
(define STUDENT-GRADES1 (make-student-grades "Lisa " (list (make-grade 123456 "FUNDIES" 48) (make-grade 123456 "MATH" 100))))
(define STUDENT-GRADES2 (make-student-grades "James" (list (make-grade 123456 "FUNDIES" 98) (make-grade 123456 "MATH" 84))))
(define STUDENT-GRADES3 (make-student-grades "Smith" (list (make-grade 654321 "FUNDIES" 86) (make-grade 654321 "MATH" 90))))




;; Complete the following function design.

;;! students->student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;;! Produces a StudentGrade for each student, with the list of grades that
;;! student received. The list produced should have an item for every student in the
;;! input list, even if there are no grades for that student.


(check-expect (students->student-grades '() '()) '())
(check-expect (students->student-grades '() GRADELIST) '())
(check-expect (students->student-grades STUDENTLIST '()) (list (make-student-grades "James" '()) (make-student-grades "Smith" '())))
(check-expect (students->student-grades STUDENTLIST GRADELIST) (list (make-student-grades "James" (list  98 84))
                                                                     (make-student-grades "Smith" (list  86  90)))) 




(define (students->student-grades los log)

(local
  [;; grade-student: Student -> StudentGrade
  (define (grade-student s)
   (make-student-grades (student-name s) (map grade-value (filter (lambda (g) (= (student-nuid s) (grade-nuid g))) log))))]
  (map grade-student los)))
  
 
   














