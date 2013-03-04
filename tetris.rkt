#lang plai-typed

(define-type (Handler 'w)
  [handler-dummy])

(require (opaque-type-in 2htdp/image
                         [Image image?])
         (opaque-type-in 2htdp/universe
                         [Key key-event?])
         (typed-in racket
                   [take : ((listof 'a) number -> (listof 'a))]
                   [drop : ((listof 'a) number -> (listof 'a))]
                   [andmap : (('a -> boolean) (listof 'a) -> boolean)]
                   [ormap : (('a -> boolean) (listof 'a) -> boolean)]
                   [random : (number -> number)])
         (typed-in 2htdp/image
                   [empty-scene : (number number -> Image)]
                   [empty-image : Image]
                   [square : (number string string -> Image)]
                   [rectangle : (number number string string -> Image)]
                   [place-image : (Image number number Image -> Image)]
                   [overlay : (Image Image -> Image)])
         (typed-in 2htdp/universe
                   ; plai-typed doesn't support varargs so we just fix the
                   ; number of handlers...
                   [big-bang : ('w
                                (Handler 'w)
                                (Handler 'w)
                                (Handler 'w)
                                (Handler 'w)
                                -> 'w)]
                   [to-draw : (('w -> Image) -> (Handler 'w))]
                   [on-tick : (('w -> 'w) number -> (Handler 'w))]
                   [on-key : (('w Key -> 'w) -> (Handler 'w))]
                   [stop-when : (('w -> boolean) -> (Handler 'w))]
                   [key=? : (Key string -> boolean)]))

(define-type Posn
  [posn (x : number) (y : number)])

(define TICK .25)
(define BLOCK-SIZE 23)
(define OFFSET (/ BLOCK-SIZE 2))
(define WIDTH 15)
(define HEIGHT 23)
(define INIT-POS (posn 8 0))

(define-type Block
  [I] [T] [O] [J] [L] [S] [Z])

(define-type Rotation
  [r0] [r90] [r180] [r270])

(define-type World
  [playing (paused? : boolean)
           (board : (listof (listof (optionof Block))))
           (piece : Block)
           (pos : Posn)
           (rot : Rotation)])

(define (piece (p : Block)) : (listof Posn)
  (type-case Block p
    [I () (list (posn 0 -2) (posn 0 -1) (posn 0 0) (posn 0 1))]
    [T () (list (posn 0 0) (posn -1 0) (posn 0 -1) (posn 1 0))]
    [O () (list (posn -1 -1) (posn -1 0) (posn 0 -1) (posn 0 0))]
    [J () (list (posn 0 -2) (posn 0 -1) (posn 0 0) (posn -1 0))]
    [L () (list (posn 0 -2) (posn 0 -1) (posn 0 0) (posn 1 0))]
    [S () (list (posn -1 0) (posn 0 0) (posn 0 -1) (posn 1 -1))]
    [Z () (list (posn -1 -1) (posn 0 -1) (posn 0 0) (posn 1 0))]))

(define (repeat (n : number) (e : 'a)) : (listof 'a)
  (build-list n (lambda (_) e)))

(define empty-row (repeat WIDTH (none)))

(define (rotate (pos : Posn) (r : Rotation)) : Posn
  (type-case Rotation r
    [r0 () pos]
    [r90 () (posn (* -1 (posn-y pos)) (posn-x pos))]
    [r180 () (posn (* -1 (posn-x pos)) (* -1 (posn-y pos)))]
    [r270 () (posn (posn-y pos) (* -1 (posn-x pos)))]))

(define (rotate-piece (p : Block) (r : Rotation)) : (listof Posn)
  (map (lambda (pos) (rotate pos r)) (piece p)))

(define (block-posns (piece : Block)
                     (pos : Posn)
                     (rot : Rotation)) : (listof Posn)
  (map (lambda (block-pos) (offset pos block-pos))
       (rotate-piece piece rot)))

(define (offset (a : Posn) (b : Posn)) : Posn
  (posn (+ (posn-x a) (posn-x b)) (+ (posn-y a) (posn-y b))))

(define (in-bounds? (p : Posn)) : boolean
  (type-case Posn p
    [posn (x y) (and (x . >= . 0) (x . < . WIDTH)
                     (y . < . HEIGHT))]))

(define (i->pix (i : number)) : number
  (+ OFFSET (* BLOCK-SIZE i)))

(define (block->img (block : (optionof Block))) : Image
  (type-case (optionof Block) block
    [none () empty-image]
    [some (b) (overlay (square BLOCK-SIZE
                               "outline"
                               "black")
                       (square BLOCK-SIZE
                               "solid"
                               (type-case Block b
                                 [I () "turquoise"]
                                 [T () "purple"]
                                 [O () "yellow"]
                                 [J () "blue"]
                                 [L () "orange"]
                                 [S () "green"]
                                 [Z () "red"])))]))

(define (draw-block (img : Image)
                    (pos : Posn)
                    (b : (optionof Block))) : Image
  (if ((posn-y pos) . < . 0)
      img
      (place-image (block->img b)
                   (i->pix (posn-x pos))
                   (i->pix (posn-y pos))
                   img)))

(define (draw-blocks (img : Image)
                     (x : number)
                     (y : number)
                     (blocks : (listof (optionof Block)))) : Image
  (cond [(empty? blocks) img]
        [(cons? blocks) (draw-block (draw-blocks img
                                                 (add1 x)
                                                 y
                                                 (rest blocks))
                                    (posn x y) (first blocks))]))

(define (draw-rows (img : Image)
                   (y : number)
                   (rows : (listof (listof (optionof Block))))) : Image
  (cond [(empty? rows) img]
        [(cons? rows) (draw-blocks (draw-rows img (add1 y) (rest rows))
                                   0
                                   y
                                   (first rows))]))

(define (draw-board (b : (listof (listof (optionof Block))))) : Image
  (draw-rows (rectangle (* BLOCK-SIZE (length (first b)))
                        (* BLOCK-SIZE (length b))
                        "solid"
                        "black")
             0
             b))


(define (draw-world (w : World)) : Image
  (type-case World w
    [playing (p? b piece p r) (foldl (lambda (pos img)
                                       (draw-block img pos (some piece)))
                                     (draw-board b)
                                     (block-posns piece p r))]))

(define (legal? (w : World)) : boolean
  (type-case World w
    [playing (p? board piece p r)
             (andmap (lambda (block-pos)
                       (and (in-bounds? block-pos)
                            (or ((posn-y block-pos) . < . 0)
                                (none? (list-ref (list-ref board
                                                           (posn-y block-pos))
                                                 (posn-x block-pos))))))
                     (block-posns piece p r))]))

(define (insert (board : (listof (listof (optionof Block))))
                (pos : Posn)
                (piece : Block)) : (listof (listof (optionof Block)))
  (if ((posn-y pos) . < . 0)
      board
      (append (take board (posn-y pos))
              (cons (let ((row (list-ref board (posn-y pos))))
                      (append (take row (posn-x pos))
                              (cons (some piece)
                                    (drop row (add1 (posn-x pos))))))
                    (drop board (add1 (posn-y pos)))))))

(test (insert (list (list (none))) (posn 0 0) (I))
      (list (list (some (I)))))

(define (remove-full-lines (board : (listof (listof (optionof Block)))))
  : (listof (listof (optionof Block)))
  (let ((rows (filter (lambda (row) (ormap none? row)) board)))
    (append (repeat (- HEIGHT (length rows)) empty-row)
            rows)))

(define (random-piece) : Block
  (list-ref (list (I) (T) (O) (J) (L) (S) (Z)) (random 7)))

(define (random-rotation) : Rotation
  (list-ref (list (r0) (r90) (r180) (r270)) (random 4)))

(define (drop-piece (world : World) (repeat? : boolean)) : World
  (type-case World world
    [playing (p? board piece p r)
             (let ((new-world (playing true
                                       board
                                       piece
                                       (offset p (posn 0 1)) r)))
               (if (legal? new-world)
                   (if repeat? (drop-piece new-world true) new-world)
                   (playing true
                            (remove-full-lines (foldl (lambda (block-pos b)
                                                        (insert b
                                                                block-pos
                                                                piece))
                                                      board
                                                      (block-posns piece p r)))
                            (random-piece)
                            INIT-POS
                            (random-rotation))))]))

(define (update/tick (world : World)) : World
  (if (playing-paused? world)
      (drop-piece world false)
      world))

(define (update/key (world : World) (key : Key)) : World
  (type-case World world
    [playing (p? board piece p r)
             (let ((new-world (cond [(key=? key "left")
                                     (playing true
                                              board
                                              piece
                                              (offset p (posn -1 0))
                                              r)]
                                    [(key=? key "right")
                                     (playing true
                                              board
                                              piece
                                              (offset p (posn 1 0))
                                              r)]
                                    [(key=? key "down")
                                     (playing true
                                              board
                                              piece
                                              (offset p (posn 0 1))
                                              r)]
                                    [(key=? key "up")
                                     (playing true board piece p
                                              (type-case Rotation r
                                                [r0 () (r90)]
                                                [r90 () (r180)]
                                                [r180 () (r270)]
                                                [r270 () (r0)]))]
                                    [(key=? key " ") (drop-piece world true)]
                                    [(key=? key "p") (playing (not p?)
                                                              board
                                                              piece
                                                              p
                                                              r)]
                                    [else world])))
               (if (legal? new-world)
                   new-world
                   world))]))

(define (stop? (world : World)) : boolean
  (not (legal? world)))

(big-bang (playing true
                   (repeat HEIGHT empty-row)
                   (random-piece)
                   INIT-POS
                   (random-rotation))
          (to-draw draw-world)
          (on-tick update/tick TICK)
          (on-key update/key)
          (stop-when stop?))
