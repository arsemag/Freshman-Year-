;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 7

;; For each TODO below, refer to the following data definitions, and use
;; pre-defined list abstraction(s) when appropriate. As a reminder, they
;; include (but are not limited to):
;;
;; - map
;; - filter
;; - andmap
;; - ormap
;; - foldr
;; - foldl
;;
;; It is encouraged that you use Lambda in these problems when applicable.

;; Just because we now have cool abstractions doesn't mean you should forget
;; about the design recipe and following templates (which particularly come up
;; for abstraction helpers)!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions (do not change these)

;; A Genre is one of
;; - "Pop"
;; - "Classical"
;; - "Country"
;; - "Rock"
;; Interpretation: a song genre

(define GENRE-POP "Pop")
(define GENRE-CLASSICAL "Classical")
(define GENRE-COUNTRY "Country")
(define GENRE-ROCK "Rock")

(define (genre-temp g)
  (cond
    [(string=? g GENRE-POP) ...]
    [(string=? g GENRE-CLASSICAL) ...]
    [(string=? g GENRE-COUNTRY) ...]
    [(string=? g GENRE-ROCK) ...]))


(define-struct song [name artist duration genre fav?])

;; A Song is a (make-song String String Nat Genre Boolean)
;; Interpretation: a song
;; - name is the title of the song
;; - artist is the song's artist
;; - duration is the length in seconds
;; - genre is the song's genre
;; - fav? is this a liked song?

(define SONG-1 (make-song "Redesigning Women" "The Highwomen" 174 GENRE-COUNTRY #true))
(define SONG-2 (make-song "Your Song" "Elton John" 241 GENRE-POP #true))
(define SONG-3 (make-song "All Along the Watchtower" "Jimi Hendrix" 241 GENRE-ROCK #false))
(define SONG-4 (make-song "Nessun Dorma" "Luciano Pavarotti" 184 GENRE-CLASSICAL #false))
(define SONG-5 (make-song "Hold Me Closer" "Elton John" 202 GENRE-POP #false))

(define (song-temp song)
  (... (song-name song) ...
       (song-artist song) ...
       (song-duration song) ...
       (genre-temp (song-genre song)) ...
       (song-fav? song) ...))

(define-struct pl [name songs])
;; A Playlist is a (make-pl String [List-of Song])
;; Interpretation: a sequence of songs
(define PL-0 (make-pl "Quiet :)" '()))
(define PL-1 (make-pl "Coding Beats" (list SONG-1 SONG-2 SONG-3 SONG-4 SONG-5)))
(define (pl-temp pl)
  (... (pl-name pl) ...
       (los-temp (pl-songs pl)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part A

;; Design the function artist-songs that produces a list of the songs in the
;; playlist with the given artist. We have given you a signature, purpose statement,
;; and check-expects. So, you only need to write the function body!

;; artist-songs : Playlist String -> [List-of String]
;; Produces all the song titles from a playlist with the given author.
(check-expect (artist-songs PL-0 "The Highwomen") '())
(check-expect (artist-songs PL-1 "Lady Gaga") '())
(check-expect (artist-songs PL-1 "Elton John") (list "Your Song" "Hold Me Closer"))

;; [TODO] Complete the function body





(define (artist-songs pl artist)
  (map song-name 
       (filter (lambda (s) (string=? (song-artist s) artist)) 
               (pl-songs pl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part B

;; Design the function any-pop? that determines if a playlist has any pop songs.
;; We have given you the signature, purpose statement, and check-expects. So,
;; you just need to complete the body, and design a helper function.

;; any-pop? : Playlist -> Boolean
;; Determines if any are pop songs.
(check-expect (any-pop? PL-0) #false)
(check-expect (any-pop? PL-1) #true)
(check-expect (any-pop? (make-pl "Infinite repeat" (list SONG-3))) #false)

;; [TODO] Function design.

(define (any-pop? pl)
  (ormap (lambda (p) (string=? (song-genre p) GENRE-POP)) (pl-songs pl)))
    





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part C

;; Design the function only-faves that when supplied a playlist returns a new
;; playlist (with the name "Faves") that only contains the liked songs.

;; only-faves : Playlist -> Playlist

;; [TODO] Function design

(define (only-faves pl)
  (make-pl "Faves"
           (filter song-fav? (pl-songs pl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part D

;; Design the function all-short? that determines if a playlist contains only
;; songs shorter than three minutes (180 seconds).

;; [TODO] Function design.
;; all-short? :  Playlist -> Boolean
(check-expect (all-short? PL-0 ) #t) 
(check-expect (all-short? PL-1) #f)
;(check-expect (all-short? (list SONG-2 SONG-3 )) #t)


(define (all-short? pl )
  (andmap (lambda (x) (> (song-duration x) 180)) (pl-songs pl)))


  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part E

;; Design the function total-duration that returns  the total length of a
;; playlist.

;; [TODO] Function design.


(define (total-duration pl)
  (foldl (lambda (s acc) (+ (song-duration s) acc)) 0
           (pl-songs pl)))

(check-expect (total-duration PL-0) 0)
;(check-expect (total-duration PL-1) )
;(check-expect (total-duration PL-2) 400)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! Part F

;; Design the function pl-string that returns a string representation of a
;; playlist. The string should be formatted as follows:

;; "Playlist: <name> Songs: <song_1> by <artist_1>, <song_2> by <artist_2>, ..., <song_n> by <artist_n>."
;; Note: there is no comma after the last song.

;; [TODO] Function design.
;; pl-string: Playlist  -> String
;; gives the presntation of the given play list starting with Playlist 

(check-expect (pl-string PL-0) "Playlist: Quiet :) Songs: .")
(check-expect (pl-string PL-1) "Playlist: Coding Beats Songs: Redesigning Women by The Highwomen, Your Song by Elton John, All Along the Watchtower by Jimi Hendrix, Nessun Dorma by Luciano Pavarotti, Hold Me Closer by Elton John.")

(define (pl-string  pl )
  (local
  [(define COMBINE  (string-append "Playlist: " (pl-name pl) " Songs: " 
    (foldr (lambda (song text) (string-append (song-name song) " by " (song-artist song) ", " text )) ""
                                                  (pl-songs pl ))))]
    (cond
      [(empty? (pl-songs pl)) (string-append COMBINE ".")]
      [(cons? (pl-songs pl)) (string-append (substring COMBINE 0 (- (string-length COMBINE) 2)) ".")])))
       
       
                       
         



