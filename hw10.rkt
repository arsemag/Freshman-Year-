;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct manager [name team])
(define-struct ic [name])
(define-struct team [name members])

;;! A Manager is a (make-manager String Team)
;;! Interpretation: a manager at a company who directly manages the team.

;;! A Person is one of:
;;! - (make-manager String Team)
;;! - (make-ic String)
;;! Interpretation: A person at a company who is either a manager or an an
;;! individual contributor ("IC").

;;! A Team is a (make-team String [List-of Person])
;;! Interpretation: A team at a company, with a name and a list of members.

;;! Problem 1

;; Define three examples of Manager. One of them should have someone with exactly
;; the same name at two levels of the hierarchy, because that is a common source
;; of confusion at large companies.

; Example of a team with individual contributors
(define MANAGER-1 (make-manager "Alice" (make-team "TEAM 1" (list (make-ic "Bob") (make-ic "Alice")))))
(define MANAGER-2 (make-manager "Bob" (make-team "TEAM 2" (list (make-ic "Charlie")
                                                                (make-ic "Henry")
                                                                (make-ic "Stan")))))
(define MANAGER-3 (make-manager "Charlie" (make-team "TEAM 3" (list (make-ic "Matt")
                                                                    (make-ic "Luke")))))



;;! Problem 2

;; Complete the following function design.

;;! list-direct-reports : String Manager -> [List-of String]
;;! Produces a list of all the direct reports of the manager with the given
;;! name. When several managers have the same name, all of them are included.

(define (list-direct-reports-temp name manager)
  (cond
    [(string=? manager-name (name manager))
     ...]
    [else ...]))

(check-expect (list-direct-reports "Alice" MANAGER-1)(list "Bob" "Alice"))
(check-expect (list-direct-reports "Bob" MANAGER-2) (list "Charlie" "Henry" "Stan"))
(check-expect (list-direct-reports "Charlie" MANAGER-3) (list "Matt" "Luke"))

(define (list-direct-reports name manager)
  (cond
    [(string=? name (manager-name manager))
     (map (lambda (ic) (ic-name ic)) (team-members (manager-team manager)))]
    [else
     (apply append (map (lambda (ic) 
                         (list-direct-reports name ic)) 
                       (team-members (manager-team manager))))]))
;;! Problem 3

;; Complete the following function design. Hint: this requires an accumulator

;;! list-managers : String Manager -> [List-of String]
;;! Produces a list of all the managers who directly manage someone with the
;;! given name. When several people have the same name, list all their managers.


(define (list-managers-temp person manager)
  (local [(define (find-manager lop name Mname)
            ...)]
          (find-manager (team-members (manager-team manager)) 
                        person
                        (manager-name manager))))

(check-expect (list-managers "Alice" MANAGER-1)(list "Alice"))
(check-expect (list-managers "Henry" MANAGER-2) (list "Bob"))
(check-expect (list-managers "Matt" MANAGER-3) (list "Charlie"))


(define (list-managers person manager)
  ;; ACCUMLATOR: finds manager of a person
 (local
   [;; find-manager : ListofPerson String String -> ListofString
    ;; finds the manager using a name and a person
 (define (find-manager lop name Mname)
      (cond
        [(empty? lop)'()]
        [(ic? (first lop)) 
  (append
      (if
        (string=? name (ic-name (first lop))) 
        (list Mname)
         '()) (find-manager (rest lop) name Mname))]
     [(manager? (first lop))
     (append
        (if
            (string=? name (manager-name (first lop)))
               (list Mname)
             '())
      (find-manager (rest lop) name Mname)
      (find-manager (team-members (manager-team (first lop))) name (manager-name (first lop))))]))]
          (find-manager (team-members (manager-team manager)) person (manager-name manager))))