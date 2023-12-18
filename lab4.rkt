;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Problem 0

;; Before you start the questions, make sure you have played Pipe Dream
;; https://archive.org/details/win3_PipeDr3x
;; You will build something similar (but simplified) to this game in the next few weeks, starting from HW6.

;;! Problem 1

;; Consider the following data definitions:

;; A Genre is one of:
;; - "comedy"
;; - "drama"
;; - "action"
;; - "education"
;; Interpretation: genre for a video

(define GENRE-COMEDY "comedy")
(define GENRE-DRAMA "drama")
(define GENRE-ACTION "action")
(define GENRE-EDUCATION "education")

(define (genre-temp g)
  (...
   (cond
     [(constant? g GENRE-COMEDY) ...]
     [(string=? g GENRE-DRAMA) ...]
     [(string=? g GENRE-ACTION) ...]
     [(string=? g GENRE-EDUCATION) ...])))


(define-struct video [name duration hd? genre next])
;; A StreamingQueue is one of:
;; - #false
;; - (make-video String PosInteger Boolean Genre StreamingQueue)
;; Interpretation: either an empty queue
;; or a video with a name, duration in minutes,
;; whether it's available in HD, and its genre, followed by the rest of the queue.

(define QUEUE-EMPTY #false)

(define QUEUE-CRASH
  (make-video "Crash Course Organic Chemistry #5"
              14 #true GENRE-EDUCATION
              QUEUE-EMPTY))

(define QUEUE-OLIVER
  (make-video
   "Prisons & Jails: Last Week Tonight with John Oliver"
   18 #true GENRE-COMEDY
   QUEUE-CRASH))

(define QUEUE-DUEL
  (make-video
   "Duel" 2 #false GENRE-ACTION QUEUE-OLIVER))

(define QUEUE-STORM
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

;;! Part A
;; Write a template for StreamingQueue, and name your template sq-temp.
(define (StreamingQueue-temp t)
  (...(cond
        [(empty? t) ...]
        [(video? t)]
        [(string?
        ...(video-name-temp t))...]
        [(integer?
          ...(video-duration-temp t))...]
        [(boolean?
          ...(video-hd?-temp t))...]
        [(string?
          ...(video-genre-temp t))...]
        [(string?
          ...(video-next-temp t))...]
        [else #false])))
         



;;! Part B

;; Design the function good-for-friday? that determines if
;; a streaming queue has content that is comedy or action


(define (good-for-friday? f)
   (cond
     [(video? f)
     (string=? (video-genre f) "comedy")]
     [(video? f)
     (string=? (video-genre f) "action")]
     [else #false]))


    

(check-expect (good-for-friday? QUEUE-CRASH) #false)
(check-expect (good-for-friday? QUEUE-DUEL) #false)
(check-expect (good-for-friday? QUEUE-OLIVER) #true)

;;! Part C

;; Design the function duration that returns the number of minutes
;; of content in a streaming queue.

(define QUEUE-STORM4
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))
;;! Part D

;; Design the function upgrade that takes a streaming queue
;; and produces HD versions of all the videos
(define QUEUE-STORM3
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

;;! Problem 2
;; List operations

;;! Part A
;; Write a function called last, which produces the last item in the list.
(define QUEUE-STOdRM2
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

;;! Part B
;; Write a function named rotate-one-clockwise that returns a list with the last item moved to the first
;; position.
(define QUEUE-STORd
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))
;; If the list is empty, produce an empty list.

;; Hint: You might want to use the last function from Part A and some helper function.



