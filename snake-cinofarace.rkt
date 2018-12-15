;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake-cinofarace) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname racket-snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Clone of snake - snake walking accross the screen looking for food

;; Constants:

(define COL 15)                       
(define ROW 15)                        
(define FIELDSIZE 20)          
(define GAMEWIDTH (* FIELDSIZE (sub1 COL)))
(define GAMEHEIGHT (* FIELDSIZE (sub1 ROW)))
(define TEXTSIZE FIELDSIZE)
(define SNKCOLOUR "black")
(define TEXTCLR "black")
(define FOODCLR "red")
(define SCENE (empty-scene GAMEWIDTH GAMEHEIGHT)) 
(define-struct Dimension (x y))
(define DIM (make-Dimension GAMEWIDTH GAMEHEIGHT))

; Data definitions:


(define-struct field (col row))

; 
; Field is (make-field Natural[1, numbCOL] Natural[1, numbROW])
; Interp: numbCOL is the number of column of the field game 
;         numbROW is the number of column of the field game 

(define F1 (make-field 1 1))
(define F2 (make-field 4 5))
(define F3 (make-field COL ROW))
#;
(define (t-field-fun f)
  (... (field-col f)
       (field-row f)))


; FieldList is one of:
; - (empty)
; - (cons Field FieldList)
; Interp: list of fields
(define LOF1 empty)
(define LOF2 (list F1 F2))
#;
(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else (... (t-field-fun (first lof))   
                   (fn-for-lof (rest lof)))]))               

; Snake is FieldList
; interp. Snake is FieldList with at least 3 fields
;         each field is adjacent to at least one other field


(define S1 (list (make-field 2 2)
                 (make-field 2 3)
                 (make-field 2 4)))

(define S2 (list (make-field 4 2)
                 (make-field 3 2)
                 (make-field 2 2)))

(define S3 (list (make-field 1 1)
                 (make-field 1 2)
                 (make-field 1 3)))

(define S4 (list (make-field COL (- ROW 1))
                 (make-field COL ROW)
                 (make-field (- COL 1) ROW)))

(define S5 (list (make-field 2 2) ; head
                 (make-field 2 1)
                 (make-field 1 1)
                 (make-field 1 2)
                 (make-field 1 3)))

#;
(define (t-snake-fun s)
  (cond [(empty? s) (...)] 
        [else (... (t-field-fun (first s))   
                   (t-snake-fun (rest s)))])) 


;; level is FieldList
;; interp. level is FieldList that containts the level set of walls
;; can include all borders or leave empty spaces, and create obstacles.

(define (list-border dim)
 (cond
   [(and
     (= (Dimension-x dim) 2)
     (= (Dimension-y dim) 2)) (make-field 2 2)]
   [(and
     (> (Dimension-x dim) 2)
     (> (Dimension-y dim) 2))(list (list-border (make-Dimension (Dimension-x dim) (- (Dimension-y dim) 1))) (make-field (Dimension-x dim) (Dimension-y dim)))]
   [(and
    (= (Dimension-x dim) 2)
    (> (Dimension-y dim) 2))(list (list-border (make-Dimension (Dimension-x dim) (- (Dimension-y dim) 1))) (make-field (Dimension-x dim) (Dimension-y dim)))]
    [(and
      (= (Dimension-y dim) 2)
      (> (Dimension-x dim) 2))(list (list-border (make-Dimension (- (Dimension-x dim) 1) (Dimension-y dim))) (make-field (Dimension-x dim) (Dimension-y dim)))]))
   
(define L1 (list-border DIM))

#;
(define (t-snake-fun s)
  (cond [(empty? s) (...)] 
        [else (... (t-field-fun (first s))   
                   (t-snake-fun (rest s)))])) 



;; Direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"

#;
(define (t-direction-fun d)
  (cond [(string=? "up" d) (...)]
        [(string=? "down" d) (...)]
        [(string=? "left" d) (...)]
        [else (...)]))


(define-struct world (snake direction trace food score))
;; World is (make-world Snake Direction Field Field Natural)
;; interp. snake is the main object of the game, controlled by the user
;;         direction of the snake's head
;;         trace is the last field of the snake from previous position, necessary for proper growth
;;         food is the field which when reached makes the snake grow
;;         score means how many foods were eaten

#;
(define (fn-for-world w)
  (... (t-snake-fun (world-snake w))
       (t-direction-fun (world-direction w))
       (t-field-fun (world-trace w))
       (t-field-fun (world-food w))
       (world-score w)))



;; =================
;; Functions:

;; World -> World
;; start the world with:
#;
(main (make-world S1
                    "right"
                    (make-field 5 2)
                    (make-field 1 2)
                    0))

(define (main w)
  (big-bang w                   ; World
            (on-tick   next-world 0.4) 
            (to-draw   draw-world)  
            (stop-when endgame? last-picture) 
            (on-key    handle-key)))    


;; World -> World
;; produce the next World:
;; - each tick move the snake one field further according to direction
;; - when snake eats food make it grow, increase the score, generate new food

(check-expect (next-world (make-world S1    
                                        "up" 
                                        (make-field 2 5)
                                        (make-field 4 4)   
                                        10))
              (make-world (list (make-field 2 1)
                                (make-field 2 2)
                                (make-field 2 3)) 
                          "up"    
                          (make-field 2 4)
                          (make-field 4 4)
                          10))    

(define (next-world w)
  (if (food-eaten? (world-snake w)
                   (world-food w))
      (make-world (grow-snake (world-snake w)
                              (world-trace w))
                  (world-direction w)  
                  (world-trace w)   
                  (new-food (world-snake w)
                            (world-food w)) 
                  (+ 1 (world-score w)))  
      (make-world (move-snake (world-snake w)
                              (world-direction w))
                  (world-direction w)   
                  (last (world-snake w))
                  (world-food w)    
                  (world-score w))))


;; Snake Food -> Boolean
;; Produce true when snake's head equals food field

(check-expect (food-eaten? S1
                           (make-field 5 5))
              false)
(check-expect (food-eaten? S1
                           (make-field 2 2))
              true)


(define (food-eaten? snake food)
  (field-equal? (first snake)
                food))


;; Field Field -> Boolean
;; Produce true when f1 and f2 are equal, cols and rows of both fields are equal
(check-expect (field-equal? (make-field 1 1)
                            (make-field 1 2))
              false)
(check-expect (field-equal? (make-field 1 2)
                            (make-field 1 2))
              true)

(define (field-equal? f1 f2)
  (and (= (field-col f1)
          (field-col f2))
       (= (field-row f1)
          (field-row f2))))


;; Snake Field -> Snake
;; Append trace to snake 

(check-expect (grow-snake S1
                          (make-field 2 5))
              (list (make-field 2 2)
                    (make-field 2 3)
                    (make-field 2 4)
                    (make-field 2 5)))

(check-expect (grow-snake S4
                          (make-field (- COL 2) ROW))
              (list (make-field COL (- ROW 1))
                    (make-field COL ROW)
                    (make-field (- COL 1) ROW)
                    (make-field (- COL 2) ROW)))

(check-expect (grow-snake S5
                          (make-field 2 3))
              (list (make-field 2 2)
                    (make-field 2 1)
                    (make-field 1 1)
                    (make-field 1 2)
                    (make-field 1 3)
                    (make-field 2 3)))

(check-expect (grow-snake empty
                          (make-field 1 1))
              (list (make-field 1 1)))


(define (grow-snake snake trace)
  (cond [(empty? snake) (cons trace empty)]
        [else (cons (first snake)
                    (grow-snake (rest snake)
                                trace))]))


;; Snake Field -> Field
;; Produce new random food

(check-expect (contains-field? S1
                               (new-food S1
                                         (make-field 2 2)))
              false)

(check-expect (adjacent? (first S1)
                         (new-food S1
                                   (make-field 2 2)))
              false)


(define (new-food snake current-food)
  (if (or (contains-field? snake current-food)
          (adjacent? (first snake)
                     current-food))
      (new-food snake
                (make-field (add1 (random COL))
                            (add1 (random ROW))))
      current-food))




;; FieldList Field -> Boolean
;; Produce true if f is an element of lof
(check-expect (contains-field? empty
                               (make-field 1 1))
              false)
(check-expect (contains-field? (list (make-field 1 2)
                                     (make-field 2 2))
                               (make-field 1 1))
              false)
(check-expect (contains-field? (list (make-field 1 1)
                                     (make-field 2 2)
                                     (make-field 3 3))
                               (make-field 1 1))
              true)

(define (contains-field? lof f)
  (cond [(empty? lof) false]
        [else (if (field-equal? (first lof) f)
                  true
                  (contains-field? (rest lof) f))]))


;; Field Field -> Boolean
;; Produce true if f1 is adjacent to f2

(check-expect (adjacent? (make-field 2 2)
                         (make-field 5 5))
              false)
(check-expect (adjacent? (make-field 2 2)
                         (make-field 2 1))
              true)
(check-expect (adjacent? (make-field 2 2)
                         (make-field 1 2))
              true)
(check-expect (adjacent? (make-field 2 2)
                         (make-field 3 2))
              true)
(check-expect (adjacent? (make-field 2 2)
                         (make-field 2 3))
              true)

(define (adjacent? f1 f2)
  (cond [(and (= (field-col f1)
                 (field-col f2))
              (= (abs (- (field-row f1)
                         (field-row f2)))
                 1))
         true]
        [(and (= (field-row f1) 
                 (field-row f2))
              (= (abs (- (field-col f1)
                         (field-col f2)))
                 1))
         true]
        [else false]))


;; Snake Direction -> Snake
;; Move the snake according to its direction

(check-expect (move-snake S1 "up")
              (list (make-field 2 1)
                    (make-field 2 2)
                    (make-field 2 3)))

(check-expect (move-snake S1 "right")
              (list (make-field 3 2)
                    (make-field 2 2)
                    (make-field 2 3)))

(check-expect (move-snake S1 "left")
              (list (make-field 1 2)
                    (make-field 2 2)
                    (make-field 2 3)))

(check-expect (move-snake S1 "down")
              (list (make-field 2 3)
                    (make-field 2 2)
                    (make-field 2 3)))

(check-expect (move-snake S2 "down")
              (list (make-field 4 3)
                    (make-field 4 2)
                    (make-field 3 2)))


(define (move-snake s d)
  (cons (move-field (first s)
                    d)
        (follow (rest s)
                (first s)))) 



;; Field Direction -> Field
;; Move field f according to direction d

(check-expect (move-field (make-field 2 2)
                          "up")
              (make-field 2 1))

(check-expect (move-field (make-field 2 2)
                          "down")
              (make-field 2 3))

(check-expect (move-field (make-field 2 2)
                          "right")
              (make-field 3 2))

(check-expect (move-field (make-field 2 2)
                          "left")
              (make-field 1 2))


(define (move-field f d)
  (cond [(string=? "up" d) (make-field (field-col f)
                                       (sub1 (field-row f)))]
        [(string=? "down" d) (make-field (field-col f)
                                         (add1 (field-row f)))]
        [(string=? "left" d) (make-field (sub1 (field-col f))
                                         (field-row f))]
        [(string=? "right" d) (make-field (add1 (field-col f))
                                          (field-row f))]))


;; FieldList Field -> FieldList
;; Produce ListOfFiled so that first becomes f, second becomes first of lof
;; second becomes first of lof, and so on..., last of lof is discarded

(check-expect (follow empty
                      (make-field 2 1))
              empty)

(check-expect (follow (list (make-field 2 2)
                            (make-field 2 3))
                      (make-field 2 1))
              (list (make-field 2 1)
                    (make-field 2 2)))

(check-expect (follow (list (make-field 2 2)
                            (make-field 3 2))
                      (make-field 2 3))
              (list (make-field 2 3)
                    (make-field 2 2)))

(define (follow lof f)
  (cond [(empty? lof) empty] 
        [else (cons f 
                    (follow (rest lof)
                            (first lof)))])) 


;; Snake -> Field
;; Get the last field of the snake

(check-expect (last S1)
              (make-field 2 4))

(check-expect (last S2)
              (make-field 2 2))

(define (last s)
  (cond [(empty? (rest s)) (first s)]
        [else (last (rest s))]))  


;; World -> Image
;; render world

(check-expect (draw-world (make-world S4    
                                        "up"   
                                        (make-field (- COL 2) 
                                                    ROW)
                                        (make-field 1 1) 
                                        20)) 
              (overlay/align "center"
                             "top"
                             (text (string-append "Score: "
                                                  (number->string 20))
                                   TEXTSIZE
                                   TEXTCLR) 
                             (overlay/align "right"
                                            "bottom"
                                            (beside/align "bottom"
                                                          (square FIELDSIZE "solid" SNKCOLOUR)
                                                          (above (square FIELDSIZE "solid" SNKCOLOUR)
                                                                 (square FIELDSIZE "solid" SNKCOLOUR)))
                                            (overlay/align "left"
                                                           "top"
                                                           (square FIELDSIZE "solid" FOODCLR)  
                                                           SCENE)))) 

(check-expect (draw-world (make-world S3
                                        "right"
                                        (make-field 1 4)  
                                        (make-field COL ROW)
                                        10))
              (overlay/align "center"
                             "top"
                             (text (string-append "Score: " (number->string 10))
                                   TEXTSIZE
                                   TEXTCLR)
                             (overlay/align "left"
                                            "top"
                                            (above (square FIELDSIZE "solid" "SNKCOLOUR")
                                                   (square FIELDSIZE "solid" "SNKCOLOUR")
                                                   (square FIELDSIZE "solid" "SNKCOLOUR"))
                                            (overlay/align "right" "bottom" (square FIELDSIZE "solid" FOODCLR) SCENE))))

(define (draw-world w)
  (render-snake (world-snake w)
                (render-field (world-food w)
                              FOODCLR
                              (render-score (world-score w)
                                            SCENE))))

;; Snake Image -> Image
;; render snake into scene

(check-expect (render-snake S3 SCENE)
              (overlay/align "left"
                             "top"
                             (above (square FIELDSIZE "solid" SNKCOLOUR)
                                    (square FIELDSIZE "solid" SNKCOLOUR)
                                    (square FIELDSIZE "solid" SNKCOLOUR))
                             SCENE))

(check-expect (render-snake S4 SCENE)
              (overlay/align "right"
                             "bottom"
                             (beside/align "bottom"
                                           (square FIELDSIZE "solid" "SNKCOLOUR")
                                           (above (square FIELDSIZE "solid" "SNKCOLOUR")
                                                  (square FIELDSIZE "solid" "SNKCOLOUR")))
                             SCENE))




(define (render-snake snake scene)
  (cond [(empty? snake) (place-image empty-image
                                     0
                                     0
                                     scene)]
        [else (render-field (first snake)
                            SNKCOLOUR
                            (render-snake (rest snake)
                                          scene))]))


;; Field Image -> Image
;; render field into scene in proper coordinates

(check-expect (render-field (make-field 1 1)
                            "black" 
                            SCENE)    
              (overlay/align "left"
                             "top"
                             (square FIELDSIZE "solid" "black")
                             SCENE))

(check-expect (render-field (make-field COL ROW)
                            "red"
                            SCENE)
              (overlay/align "right"
                             "bottom"
                             (square FIELDSIZE "solid" "red")
                             SCENE))


(define (render-field f clr scene)
  (place-image (square FIELDSIZE "solid" clr)
               (field-x f)
               (field-y f)
               scene))

;; Field -> Natural
;; Produce x coordinate of f

(check-expect (field-x (make-field 1 1))
              (- (* 1 FIELDSIZE)
                 (/ FIELDSIZE 2)))
(check-expect (field-x (make-field 2 1))
              (- (* 2 FIELDSIZE)
                 (/ FIELDSIZE 2)))

(define (field-x f)
  (- (* (field-col f)
        FIELDSIZE)
     (/ FIELDSIZE 2)))


;; Field -> Natural
;; Produce y coordinate of f
(check-expect (field-y (make-field 1 1)) 
              (- (* 1 FIELDSIZE)
                 (/ FIELDSIZE 2)))
(check-expect (field-y (make-field 2 1))
              (- (* 1 FIELDSIZE)
                 (/ FIELDSIZE 2)))


(define (field-y f)
  (- (* (field-row f)
        FIELDSIZE)
     (/ FIELDSIZE 2)))


;; Natural Image -> Image
;; render score into scene
(check-expect (render-score 10 SCENE)
              (overlay/align "center"
                             "top"
                             (text "Score: 10"
                                   TEXTSIZE
                                   TEXTCLR)
                             SCENE))


(define (render-score score scene)
  (overlay/align "center"
                 "top"
                 (text (string-append "Score: "
                                      (number->string score))
                       TEXTSIZE
                       TEXTCLR)
                 scene))


;; World -> Boolean
;; Produce true when snake bites itself or when it hits the edge of the scene

(check-expect (endgame? (make-world S1
                                       "up"
                                       (make-field 2 5)
                                       (make-field 5 5) 
                                       10))          
              false)

(check-expect (endgame? (make-world (list (make-field 0 2)
                                             (make-field 1 2)
                                             (make-field 2 2))
                                       "left"
                                       (make-field 3 2)
                                       (make-field 5 5) 
                                       10))          
              true)

(check-expect (endgame? (make-world (list (make-field 2 0)
                                             (make-field 2 1)
                                             (make-field 2 2))
                                       "up"
                                       (make-field 2 3) 
                                       (make-field 5 5) 
                                       10))       
              true)


(check-expect (endgame? (make-world (list (make-field 2 (+ ROW 1))
                                             (make-field 2 ROW)
                                             (make-field 2 (- ROW 1)))
                                       "down"
                                       (make-field 2 (- ROW 2)) 
                                       (make-field 5 5)          
                                       10))                     
              true)

(check-expect (endgame? (make-world (list (make-field (+ COL 1) 2)
                                             (make-field COL 2)
                                             (make-field (- COL 1) 2))
                                       "right"
                                       (make-field (- COL 2) 2)
                                       (make-field 5 5) 
                                       10))            
              true)


(check-expect (endgame? (make-world (list (make-field 2 2)
                                             (make-field 3 2)
                                             (make-field 3 3)
                                             (make-field 2 3)
                                             (make-field 2 2))
                                       "up"
                                       (make-field 1 2)
                                       (make-field 5 5)
                                       10))     
              true)

(define (endgame? w)
  (or (self-bite? (world-snake w))
      (edge-collision? (world-snake w))))


;; Snake -> Boolean
;; Produce true when snake's tail contains head
(check-expect (self-bite? S1) false)

(check-expect (self-bite? (list (make-field 2 2)
                                (make-field 3 2)
                                (make-field 3 3)
                                (make-field 2 3)
                                (make-field 2 2)))
              true)

(define (self-bite? snake)
  (contains-field? (rest snake) 
                   (first snake))) 


;; Snake -> Boolean
;; Produce true if hits the edge:
;;  - head's row is < 1 or > ROW 
;;  - head's col is < 1 or > ROW
(check-expect (edge-collision? S1) false)
(check-expect (edge-collision? (list (make-field 0 2)
                                     (make-field 1 2)
                                     (make-field 2 2)))
              true)
(check-expect (edge-collision? (list (make-field 2 0)
                                     (make-field 2 1)
                                     (make-field 2 2)))
              true)
(check-expect (edge-collision? (list (make-field (+ COL 1) 2)
                                     (make-field COL 2)
                                     (make-field (- COL 1) 2)))
              true)
(check-expect (edge-collision? (list (make-field 2 (+ ROW 1))
                                     (make-field 2 ROW)
                                     (make-field 2 (- ROW 1))))
              true)


(define (edge-collision? s)
  (or (< (field-row (first s))
         1)
      (> (field-row (first s))
         ROW)
      (< (field-col (first s))
         1)
      (> (field-col (first s))
         COL)))


;; World KeyEvent -> World
;; Produce new world by changing direction of the snake:
;; - up arrow changes direction to "up", unless current direction is "down", if so leave as it is
;; - down arrow changes direction to "down", unless current direction is "up", if so leave as it is
;; - right arrow changes direction to "right", unless current direction is "left", if so leave as it is
;; - left arrow changes direction to "left", unless current direction is "right", if so leave as it is

(check-expect (handle-key (make-world S3
                                      "right"
                                      (make-field 1 4)
                                      (make-field COL ROW)
                                      10)
                          "right")
              (make-world S3
                          "right"
                          (make-field 1 4)        
                          (make-field COL ROW) 
                          10))

(check-expect (handle-key (make-world S3
                                      "right"
                                      (make-field 1 4)
                                      (make-field COL ROW)
                                      10)
                          "up")
              (make-world S3
                          "up"
                          (make-field 1 4)        
                          (make-field COL ROW) 
                          10))

(check-expect (handle-key (make-world S3
                                      "right"
                                      (make-field 1 4)
                                      (make-field COL ROW)
                                      10)
                          "left")
              (make-world S3
                          "right"
                          (make-field 1 4)        
                          (make-field COL ROW) 
                          10))

(check-expect (handle-key (make-world S3
                                      "right"
                                      (make-field 1 4)
                                      (make-field COL ROW)
                                      10)
                          "down")
              (make-world S3
                          "down"
                          (make-field 1 4)        
                          (make-field COL ROW) 
                          10))

(check-expect (handle-key (make-world S3
                                      "up"
                                      (make-field 1 4)
                                      (make-field COL ROW)
                                      10)
                          "down")
              (make-world S3
                          "up"
                          (make-field 1 4)        
                          (make-field COL ROW) 
                          10))

(define (handle-key w ke)
  (cond [(opposite? (world-direction w)
                    ke)
         w]
        [else (make-world (world-snake w)
                          ke
                          (world-trace w)
                          (world-food w)
                          (world-score w))]))

;; Direction Direction -> Boolean
;; Produce true when d1 and d2 are opposite to each other
(check-expect (opposite? "up" "right") false)
(check-expect (opposite? "up" "up") false)
(check-expect (opposite? "up" "down") true)
(check-expect (opposite? "down" "right") false)
(check-expect (opposite? "down" "up") true)
(check-expect (opposite? "left" "up") false)
(check-expect (opposite? "left" "left") false)
(check-expect (opposite? "left" "right") true)
(check-expect (opposite? "right" "left") true)

(define (opposite? d1 d2)
  (cond [(and (string=? "up" d1)
              (string=? "down" d2))
         true]
        [(and (string=? "down" d1)
              (string=? "up" d2))
         true]
        [(and (string=? "left" d1)
              (string=? "right" d2))
         true]
        [(and (string=? "right" d1)
              (string=? "left" d2))
         true]
        [else false]))


;; World -> Image
;; Produce image to display at the end of the game
(check-expect (last-picture (make-world S3
                                        "up"
                                        (make-field 1 4)        
                                        (make-field COL ROW) 
                                        10))
              (place-image (above (text "GAME OVER!"
                                        15
                                        "black")
                                  (text (string-append "Your score is "
                                                       (number->string 10))
                                        15
                                        "black"))
                           (/ GAMEWIDTH 2)
                           (/ GAMEHEIGHT 2)
                           SCENE))

(define (last-picture w)
  (place-image (above (text "GAME OVER!"
                            15
                            "black")
                      (text (string-append "Your score is "
                                           (number->string (world-score w)))
                            15
                            "black"))
               (/ GAMEWIDTH 2)
               (/ GAMEHEIGHT 2)
               SCENE))
; ok, that's it. thank you for watching