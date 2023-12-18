;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 

;;! Problem 1

;;! Part A

;; TODO: design the data necessary to represent a book, which can
;; either be physical or electronic. All books have a title and author.
;; Physical books are either paperback or harcover, and have some number of
;; pages. Electronic (e-books) have a format (pdf, epub, txt) and a source URL.

(define-struct physical (type title aurthor pages))
;physical is (make-physical String String String Number)
; a type book is either a paperback or harcover
; the number of pages in a physical book 
(define PHYSICAL1 (make-physical "paperback" "CS" "James White" 10 ))
(define PHYSICAL2 (make-physical "hardcover" "Macbook" "Sam" 100))

(define (physical-temp p)
  (...(physical-type p)...
      (physical-pages p)...
      (physical-aurthor p)...
      (physical-title p)...))


(define-struct electronic (format url title aruthor ))
;electronic is (make-electronic String String String String)
;a format of an electronic book is either a pdf,epub, or txt
;a url is the location of where the book can be found
(define ELECTRONIC1 (make-electronic "pdf" "http://1" "DrRacket" "Nancy P"))
(define ELECTRONIC2 (make-electronic "epub" "http://2" "Apples" "Steve Jobs"))
(define ELECTRONIC3 (make-electronic "txt" "http://3" "Blue Fish" "50 Cent" ))

(define (electronic-temp e)
  (...(electronic-format e)...
      (electronic-url e)...
      (electronic-title e)...
      (electronic-aruthor e)...))


; book is either
; physical book
; electronic book
(define BOOK1 (make-physical "paperback" "CS" "James White" 10 ))
(define BOOK2 (make-electronic "txt" "http://3" "Blue Fish" "50 Cent" ))

(define (book-temp b)
  (cond
    [(physical? b )...(physical-temp b)]
     [ (electronic? b)...(electronic-temp b)]...))
     
  


;;! Part B

; TODO: now design the function where-to-find that
; accepts a book and returns where you can find it:
; physical books are either in the "hardcover section"
; or "paperback section", whereas electronic books are
; found at their URL.

; WheretoFind: Book -> LocationofBook
; to find the location of a physical or electronic book 

(define(where-to-find book)
  (cond
    [(((physical? b ) (physical-hardcover? book)) (physical-paperback? book))]
     [((electronic? b)...(electronic-url book))]))











;;! Problem 2

;; Consider the follow structure definitions:
(define-struct meme [name dank? upvotes])
(define-struct animal [name generative-ai? upvotes])
(define-struct automobile [name upvotes])

;;! Part A

;; Complete the data designs for each structure: Meme, Animal, and Automobile

; A Meme is a (make-meme [String Boolean Number])
; name is what a meme is called
; dank? is if the post is disagreeably or not
; upvotes is the number of votes the meme has

(define MEME1 (make-meme "Happy" #false 100))
(define MEME2 (make-meme "Sad" #true 200))

(define(meme m)
  (...(meme-name m)...(meme-dank? m)...(meme-upvotes m)...))

; A Animal is a (make-animal[String Boolean? Number])
; name is the what te animal is called
; genertive-ai? if the animal is created by an ai or not
; upvotes is how many votes the animal recieved

(define ANIMAL1 (make-animal "Dog" #t 10000))
(define ANIMAL2 (make-animal "Cat" #f 0))

(define(animal a)
  (...(animal-name a)...(animal-generative-ai? a)...(animal-upvotes a)))

; A Automobile is a [make-automobile String Number]









;;! Part B

;; Complete a data design called Post, which can represent any of the
;; posts listed above.

; 






;;! Part C

;; Consider this definition:

(define-struct subreddit [first second third fourth])
;; A Subreddit is a (make-subreddit Post Post Post Post)
;; which represents a Subreddit that has 4 posts uploaded.

(define (subreddit-template subred)
  (...
   (subreddit-first subred) ...
   (subreddit-second subred) ...
   (subreddit-third subred) ...
   (subreddit-fourth subred) ... ))

;; Design a function adjust-upvotes that takes in a Subreddit
;; and produces a Subreddit with all the upvotes of
;; the posts have increased by 150.
;; However, if the post is an animal post and the picture was generated
;; by AI, then the upvotes should decrease by 100.

