(import (scheme base)
        (scheme inexact)
        (hoot ffi)
        (hoot match)
        (hoot debug)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window)
        (console))


(define game-width    640.0)
(define game-height   480.0)

(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))

(define-record-type <gelement-type>
  (make-gelement-type interact? image)
  gelement-type?
  (interact? gelement-interact?)
  (image gelement-type-iamge))

(define-record-type <gelement>
  (make-gelement type content)
  gelement?
  (type gelement-type)
  (content gelement-content set-gelement-content!))

(define-record-type <parentheses>
  (make-parentheses type pos content)
  parentheses?
  (type parentheses-type)
  (pos parentheses-pos set-parentheses-pos!)
  (content parentheses-content set-parentheses-content!))

(define-record-type <level>
  (make-level state grid left-parenthes right-parenthes goal)
  level?
  (state level-state set-level-state!)
  (grid level-grid set-level-grid!)
  (left-parenthes level-left-parenthes set-left-parenthes!)
  (right-parenthes level-right-parenthes set-right-parenthes!)
  (goal level-goal set-level-goal!))


(define wall (make-gelement (make-gelement-type #f "________________") '()))
(define wall2 (make-gelement (make-gelement-type #f "|") '()))
(define apple (make-gelement (make-gelement-type #t "apple") '(4 5)))
(define l-st (make-gelement (make-gelement-type #t "string-append") '(string-append)))
(define l-hello (make-gelement (make-gelement-type #t "Hello ") '("Hello ")))
(define l-world (make-gelement (make-gelement-type #t "World!") '("World!")))
(define air (make-gelement (make-gelement-type #t "") '()))
(define l-plus (make-gelement (make-gelement-type #t "+") '(+)))
(define l-sub (make-gelement (make-gelement-type #t "-") '(-)))
(define l-div (make-gelement (make-gelement-type #t "÷") '(/)))
(define l-time (make-gelement (make-gelement-type #t "×") '(*)))

(define l-11 (make-gelement (make-gelement-type #t "11") '(11)))
(define l-2 (make-gelement (make-gelement-type #t "2") '(2)))
(define l-4 (make-gelement (make-gelement-type #t "4") '(4)))
(define l-336 (make-gelement (make-gelement-type #t "336") '(336)))

(define (make-level-1) (make-level
                        'run
                        (vector
                         (vector air air air air air)
                         (vector wall wall wall wall wall)
                         (vector air l-st l-hello l-world air)
                         (vector wall wall wall wall wall)
                         (vector air air air air air))
                        (make-parentheses 'left (cons 0 2) '())
                        (make-parentheses 'right (cons 4 2) '())
                        "Hello World!"))

(define (make-level-2) (make-level
                        'run
                        (vector
                         (vector air l-div air l-11 air)
                         (vector air air air air air)
                         (vector l-2 air l-plus air l-4)
                         (vector air air air air l-336)
                         (vector air l-time air air air))
                        (make-parentheses 'left (cons 0 0) '())
                        (make-parentheses 'right (cons 4 4) '())
                        42))

(define *current-level* 1)
(define (make-current-level)
  (cond ((= *current-level* 1)
         (make-level-1))
        ((= *current-level* 2)
         (make-level-2))
        )

  )

(define *level* (make-current-level))

(define (set-grid! x y val)
  (vector-set! (vector-ref (level-grid *level*) y) x val))

(define (funcall fun args)
  (cond ((equal? fun '+)
         (apply + args))
        ((equal? fun '-)
         (apply - args))
        ((equal? fun '*)
         (apply * args))
        ((equal? fun '/)
         (apply / args))
        ((equal? fun 'string-append)
         (apply string-append args))))

(define (eval-parenthes content)
  (append '()
          (funcall (car content)
                   (cdr content))))

(define (convert e)
  (cond ((symbol? e)
         (symbol->string e))
        ((number? e)
         (number->string e))
        ((string? e)
         e)))

(define (convert-iter e)
  (string-append " " (convert e) " "))

(define (slist->string slst)
  (apply string-append (map convert-iter slst)))

(define (collide-gelement! x y parenthes)
  (dprint "x" x)
  (dprint "y" y)
  (dprint "content" (parentheses-content parenthes))
  (dprint "par pos" (parentheses-pos parenthes))
  (if (and (and (>= x 0) (>= y 0) (and  (< x (vector-length (level-grid *level*))) (< y (vector-length (level-grid *level*))))))
      (let ((e (vector-ref (vector-ref (level-grid *level*) y) x))
            (len (vector-length (level-grid *level*))))
        (if (and  (< x len) (< y len))
            (if (gelement-interact? (gelement-type e))
                (begin
                  (set-grid! x y air)
                  (set-parentheses-pos! parenthes (cons x y))
                  (set-parentheses-content! parenthes (append (parentheses-content parenthes) (gelement-content e)))
                  (dprint "after-content" (parentheses-content parenthes)))
                (dprint "Boop!"))))
      (dprint "Boop!")))

(define (collide-pareneheses! level)
  (let ((left-parenthes (level-left-parenthes level))
        (right-parenthes (level-right-parenthes level))
        (goal (level-goal level)))
    (if (equal? (parentheses-pos left-parenthes) (parentheses-pos right-parenthes))
        (let ((val (eval-parenthes (append (parentheses-content left-parenthes)
                                            (parentheses-content right-parenthes)))))
          (if (equal? val goal)
              (set-level-state! level 'win)
              (begin
                (set-parentheses-content! left-parenthes '())
                (set-parentheses-content! right-parenthes `(,val))))))))

;; Draw
(define (draw prev-time)
  (clear-rect context 0.0 0.0 game-width game-height)
  (set-fill-color! context "#140c1c")
  (set-text-align! context "center")
  (set-font! context "bold 24px monospace")
  (let ((grid (level-grid *level*))
        (left-parenthes (level-left-parenthes *level*))
        (right-parenthes (level-right-parenthes *level*))
        (goal (level-goal *level*)))
    ; Draw grid
    (do ((i 0 (+ i 1)))
        ((= i (vector-length grid)))
      (do ((j 0 (+ j 1)))
          ((= j (vector-length grid)))

        (let* ((gele (vector-ref (vector-ref grid j) i))
               (avg-width (/ game-width (vector-length grid)))
               (avg-hight (/ game-height (vector-length grid))))

          (set-font! context "16px monospace")
          (fill-text context (gelement-type-iamge (gelement-type gele)) (* 100 (+ i 1)) (* 50 (+ j 1))))))

    (set-font! context "24px monospace")
    ; Draw left-parenthes
    (fill-text context "("
               (* (+ (car (parentheses-pos left-parenthes)) 1) 100)
               (* (+ (cdr (parentheses-pos left-parenthes)) 1) 50))

    ; Draw right-parenthes
    (fill-text context ")"
               (* (+ (car (parentheses-pos right-parenthes)) 1) 100)
               (* (+ (cdr (parentheses-pos right-parenthes)) 1) 50))

    ; Draw hub
    (set-text-align! context "left")
    (fill-text context
               (string-append "(" (slist->string (parentheses-content left-parenthes)))
               0 350)

    (fill-text context
               (string-append (slist->string (parentheses-content right-parenthes)) ")")
               0 400)

    (fill-text context
               (string-append "Goal: "  (convert goal))
               0 450)

    ; Draw title
    (match (level-state *level*)
      ('win
       (set-text-align! context "center")
       (set-font! context "bold 24px monospace")
       (fill-text context "Goal equal! Press Enter to continue" (/ game-width 2.0) (/ game-height 2.0)))
      (_ #t))
    (request-animation-frame draw-callback)))
(define draw-callback (procedure->external draw))

(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(request-animation-frame draw-callback)

;; Input
(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (cond
     ((string=? key "KeyR")
      (set! *level* (make-current-level))
      (request-animation-frame draw-callback))
     ((string=? key "Enter")
      (if (equal? (level-state *level*) 'win)
          (begin
            (set! *current-level* (+ *current-level* 1))
            (set! *level* (make-current-level))
            (request-animation-frame draw-callback))))
     ((string=? key "KeyA")
      (collide-gelement! (- (car (parentheses-pos (level-left-parenthes *level*))) 1) (cdr (parentheses-pos (level-left-parenthes *level*))) (level-left-parenthes *level*)))
     ((string=? key "KeyD")
      (collide-gelement! (+ (car (parentheses-pos (level-left-parenthes *level*))) 1) (cdr (parentheses-pos (level-left-parenthes *level*))) (level-left-parenthes *level*)))
     ((string=? key "KeyW")
      (collide-gelement! (car (parentheses-pos (level-left-parenthes *level*))) (- (cdr (parentheses-pos (level-left-parenthes *level*))) 1) (level-left-parenthes *level*)))
     ((string=? key "KeyS")
      (collide-gelement! (car (parentheses-pos (level-left-parenthes *level*))) (+ (cdr (parentheses-pos (level-left-parenthes *level*))) 1) (level-left-parenthes *level*)))
     ((string=? key "ArrowLeft")
      (collide-gelement! (- (car (parentheses-pos (level-right-parenthes *level*))) 1) (cdr (parentheses-pos (level-right-parenthes *level*))) (level-right-parenthes *level*)))
     ((string=? key "ArrowRight")
      (collide-gelement! (+ (car (parentheses-pos (level-right-parenthes *level*))) 1) (cdr (parentheses-pos (level-right-parenthes *level*))) (level-right-parenthes *level*)))
     ((string=? key "ArrowUp")
      (collide-gelement! (car (parentheses-pos (level-right-parenthes *level*))) (- (cdr (parentheses-pos (level-right-parenthes *level*))) 1) (level-right-parenthes *level*)))
     ((string=? key "ArrowDown")
      (collide-gelement! (car (parentheses-pos (level-right-parenthes *level*))) (+ (cdr (parentheses-pos (level-right-parenthes *level*))) 1) (level-right-parenthes *level*))))
    (collide-pareneheses! *level*)
    )
  )

(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
