;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Finished Pipes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TLBR (make-pipe #true #true #true #true))

(define ALL-PIPES (cons PIPE-TB (cons PIPE-LR (cons PIPE-TL (cons PIPE-TR (cons PIPE-BL
                                                                                     (cons PIPE-BR (cons PIPE-TLBR '()))))))))

;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the  pipe is pipe-width. Pipe-width should be less than tile-side-length
(define (pipe->image pipe tile-side-length pipe-width)
  (local  ;; get-color: Boolean -> Color
    ;; produces a color for type of boolean
    [(define (get-color boolean)
       (if boolean
           "black"
           "silver"))]
    (overlay (above
              (rectangle  pipe-width (/ (- tile-side-length pipe-width) 2)  "solid" (get-color ( pipe-top pipe)))

              (beside
               (rectangle (/ (- tile-side-length pipe-width) 2) pipe-width   "solid" (get-color ( pipe-left pipe)))
               (rectangle pipe-width pipe-width "solid"  "black")
               (rectangle  (/ (- tile-side-length pipe-width) 2) pipe-width "solid" (get-color (pipe-right pipe))))
              (rectangle pipe-width (/ (- tile-side-length pipe-width) 2) "solid" (get-color ( pipe-bot pipe))))
             (square tile-side-length "solid" "silver"))))
;(pipe->image pipe 40 14 results in best look)

;; Part 2



;; Task 4

(define-struct grid [PIPE row col])
; Grid ; ListofPlaceNumber Number -> Grid
; takes in a list of titles with pipes and the dimension to produce a grid

(define GRID-1 (list 
                (make-grid grid-PIPE 1 1)
                (make-grid grid-PIPE 1 2)
                (make-grid grid-PIPE 1 3)
                (make-grid grid-PIPE 1 4)
                (make-grid grid-PIPE 1 5)
                (make-grid grid-PIPE 1 6)
                (make-grid grid-PIPE 1 7)
                (make-grid grid-PIPE 2 1)
                (make-grid grid-PIPE 2 2)
                (make-grid grid-PIPE 2 3)
                (make-grid grid-PIPE 2 4)
                (make-grid grid-PIPE 2 5)
                (make-grid grid-PIPE 2 6)
                (make-grid grid-PIPE 2 7)
                (make-grid grid-PIPE 3 1)
                (make-grid grid-PIPE 3 2)
                (make-grid grid-PIPE 3 3)
                (make-grid grid-PIPE 3 4)
                (make-grid grid-PIPE 3 5)
                (make-grid grid-PIPE 3 6)
                (make-grid grid-PIPE 3 7)
                (make-grid grid-PIPE 4 1)
                (make-grid grid-PIPE 4 2)
                (make-grid grid-PIPE 4 3)
                (make-grid grid-PIPE 4 4)
                (make-grid grid-PIPE 4 5)
                (make-grid grid-PIPE 4 6)
                (make-grid grid-PIPE 4 7)
                (make-grid grid-PIPE 5 1)
                (make-grid grid-PIPE 5 2)
                (make-grid grid-PIPE 5 3)
                (make-grid grid-PIPE 5 4)
                (make-grid grid-PIPE 5 5)
                (make-grid grid-PIPE 5 6)
                (make-grid grid-PIPE 5 7)
                (make-grid grid-PIPE 6 1)
                (make-grid grid-PIPE 6 2)
                (make-grid grid-PIPE 6 3)
                (make-grid grid-PIPE 6 4)
                (make-grid grid-PIPE 6 5)
                (make-grid grid-PIPE 6 6)
                (make-grid grid-PIPE 6 7)
                (make-grid grid-PIPE 7 1)
                (make-grid grid-PIPE 7 2)
                (make-grid grid-PIPE 7 3)
                (make-grid grid-PIPE 7 4)
                (make-grid grid-PIPE 7 5)
                (make-grid grid-PIPE 7 6)
                (make-grid grid-PIPE 7 7)))

;Task 5

(define STARTING-GRID 
  (make-list 7 (make-list 7 empty)))

;Task 6

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(define (place-pipe grid pipe row col)
  (let ((new-row (replace (list-ref grid row) col pipe)))
    (replace grid row new-row)))

(define (replace lst index elem)
  (if (empty? lst)
      '()
      (if (zero? index)
          (cons elem (cdr lst))
          (cons (car lst) (replace (cdr lst) (- index 1) elem)))))
 
(check-expect (place-pipe STARTING-GRID PIPE-TB 3 2) (list
                                                      (list '() '() '() '() '() '() '())
                                                      (list '() '() '() '() '() '() '())
                                                      (list '() '() '() '() '() '() '())
                                                      (list '() '() (make-pipe #true #true #false #false) '() '() '() '())
                                                      (list '() '() '() '() '() '() '())
                                                      (list '() '() '() '() '() '() '())
                                                      (list '() '() '() '() '() '() '())))

(check-expect (place-pipe STARTING-GRID PIPE-TB 0 0)
              (list
               (list (make-pipe #true #true #false #false) '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())))

(check-expect (place-pipe STARTING-GRID PIPE-TB 6 3)
              (list
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() '() '() '() '())
               (list '() '() '() (make-pipe #true #true #false #false) '() '() '())))


;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.

(define (pipe-at grid row col)
  (let ((cell (list-ref (list-ref grid row) col)))
    (if (equal? cell empty)
        #false
        cell)))


(define STARTING-GRID-WITH-PIPE 
  (let ((grid (make-list 7 (make-list 7 empty))))
    (place-pipe grid PIPE-TB 3 3)))


(check-expect (pipe-at STARTING-GRID-WITH-PIPE 3 3) PIPE-TB)
(check-expect (pipe-at STARTING-GRID-WITH-PIPE 0 0) #false)
(check-expect (pipe-at STARTING-GRID-WITH-PIPE 6 6) #false)

;Task 7:
;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.



(define (grid->image grid tile-side-length pipe-width)
  (above (row->image ( - tile-side-length 1))
         (row->image ( - tile-side-length 1))
         (row->image ( - tile-side-length 1))
         (row->image ( - tile-side-length 1))
         (row->image ( - tile-side-length 1))
         (row->image ( - tile-side-length 1))
         (row->image ( - tile-side-length 1))))

; 8 by 8 works best


; Row->Image ;
; draw a tile and put it beside each other
(define size 50)
(define tile (rectangle size size "outline" "black"))

(define(row->image tile-side-length )
  (if
   (= tile-side-length 1)
   tile
   (beside tile (row->image ( - tile-side-length 1)))))

;Task 8

(define-struct GameState [grid incoming-pipes])


(define GAME (make-GameState STARTING-GRID ALL-PIPES))

(define (place-incoming-pipe GameState row col)
  (let* ([incoming-pipes (GameState-incoming-pipes GameState)]
         [pipe (first incoming-pipes)]
         [remaining-pipes (rest incoming-pipes)]
         [grid (place-pipe (GameState GameState) pipe row col)])
    (make-GameState grid remaining-pipes)))
;Task 9

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
 
(define (place-pipe-on-click GameState row col mouse-event)
  (if (string=? "button-down" mouse-event)
      (let* ([incoming-pipes (GameState-incoming-pipes GameState)]
             [pipe (first incoming-pipes)]
             [remaining-pipes (rest incoming-pipes)]
             [grid (place-pipe (GameState-grid GameState) pipe row col)])
        (make-GameState grid remaining-pipes))
      GameState))

;Task 10

(define (pipe-fantasy initial-GameState)
  (big-bang initial-GameState
            [to-draw draw-game]
            [on-mouse place-pipe-on-click]))


(define (draw-game game-state)
 (grid->image (GameState-grid game-state) 8 8))
