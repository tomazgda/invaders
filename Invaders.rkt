#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; A GameState is
;; - a Ship, the player's ship
;; - an Invader, the enemy
;; - a Bullet, the player's bullet. of #f if there is no bullet.

(struct Ship (x))
(struct Bullet (x y))
(struct Invader (x y direction))
(struct Bomb (x y))
(struct GameState (Ship Bullet Invader Bomb))

;; The initial state of the game
(define GS-INITIAL
  (GameState
   (Ship 250)
   #f
   (Invader 30 50 'right)
   #f))

;; Shapes for drawing
(define ENEMY (square 20 "solid" "White"))
(define SHIP (triangle 30 "solid" "White"))
(define BULLET (circle 5 "solid" "blue"))
(define BOMB (circle 5 "solid" "Red"))
(define SCENE (empty-scene 500 500 "black"))

;; Speeds and distances
(define SPEED-INVADER 1.5)
(define SPEED-BULLET 10)                                                                                           
(define SPEED-SHIP 5)
(define DESCENT-INVADER 10)
(define SHIP-Y 450)
(define SPEED-BOMB 10)

;; draw-game : GameState -> Scene
;; Produce a scene representing the whole game
(define (draw-game gs)
  (define bullet (GameState-Bullet gs))
  (define bomb (GameState-Bomb gs))
  (define image-ship-and-invader
    (place-image SHIP ;; Show the player's ship ...
                 (Ship-x (GameState-Ship gs)) SHIP-Y
                 (place-image ENEMY ;; ... and the invader ..
                              (Invader-x (GameState-Invader gs))
                              (Invader-y (GameState-Invader gs))
                              SCENE ;; ... on the background
                              )))
  (define image-ship-invader-and-bullet
    (if bullet
      (place-image BULLET
                   (Bullet-x bullet) (Bullet-y bullet)
                   image-ship-and-invader)
      image-ship-and-invader))

  (if bomb
      (place-image BOMB
                   (Bomb-x bomb) (Bomb-y bomb)
                   image-ship-invader-and-bullet)
      image-ship-invader-and-bullet))

;; Respond to keyboard input
(define (handle-key gs key)
  (cond
    ((key=? key "left") (move-ship gs (- SPEED-SHIP)))
    ((key=? key "right") (move-ship gs SPEED-SHIP))
    ((key=? key " ") (add-bullet gs))
    ((key=? key "shift") (add-bomb gs))
    (else gs)))

;; move-ship : GameState Number -> GameState
(define (move-ship gs delta)
  (define current-ship (GameState-Ship gs))
  (define new-ship (Ship (+ (Ship-x current-ship) delta)))
  (struct-copy GameState gs
               [Ship new-ship]))

;; add-bullet : GameState -> GameState
;; Add a bullet at the player's ship, heading upwards,
;; unless one already exists
(define (add-bullet gs)
  ;; Don't do anything unless there's no bullet already
  (define ship-x (Ship-x (GameState-Ship gs)))
  (if (GameState-Bullet gs)
      gs
      (struct-copy GameState gs
                   [Bullet (Bullet ship-x SHIP-Y)])))

(define (add-bomb gs)
  (define invader-x (Invader-x (GameState-Invader gs)))
  (define invader-y (Invader-y (GameState-Invader gs)))
  (if (GameState-Bomb gs)
      gs
      (struct-copy GameState gs
                   [Bomb (Bomb invader-x invader-y)])))

;; Respond to tick event
(define (handle-tick gs)
  (struct-copy GameState gs
               [Bomb (bomb-update (GameState-Bomb gs))]
               [Invader (invader-update (GameState-Invader gs))]
               [Bullet  (bullet-update  (GameState-Bullet  gs))]))

;; Update the position and, if necessary, direction of the invader
(define (invader-update inv)
  (define x (Invader-x inv))
  (define y (Invader-y inv))
  (define direction (Invader-direction inv))
  (define new-x
    (if (eq? direction 'right)
        (+ x SPEED-INVADER)
        (- x SPEED-INVADER)))
  (define change-direction?
    (or (> new-x 475) (< new-x 25)))
  (define new-direction
    (if change-direction?
        (if (eq? direction 'right) 'left 'right)
        direction))
  (define new-y
    (if change-direction?
        (+ y DESCENT-INVADER)
        y))
  (Invader new-x new-y new-direction))

;; Update the position of the bullet
(define (bullet-update bullet)   
  (if (not bullet)
      #f
      (bullet-update-helper bullet)))

(define (bomb-update bomb)
  (if (not bomb)
      #f
      (bomb-update-helper bomb)))

(define (bullet-update-helper bullet)
  (define x (Bullet-x bullet))
  (define y (Bullet-y bullet))
  (define new-y (- y SPEED-BULLET))
  (if (< new-y 0)
      #f  ;; Remove bullet if it leaves the screen
      (Bullet x new-y)))

(define (bomb-update-helper bomb)
  (define x (Bomb-x bomb))
  (define y (Bomb-y bomb))
  (define new-y (+ y SPEED-BOMB))
  (if (> new-y 500)
      #f
      (Bomb x new-y)))

   













  

