(import (scheme base)
        (scheme inexact)
        (hoot ffi)
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

(define *element-x* (/ game-width 2.0))

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
  (make-level grid left-parenthes right-parenthes goal)
  level?
  (grid level-grid set-level-grid!)
  (left-parenthes level-left-parenthes set-left-parenthes!)
  (right-parenthes level-right-parenthes set-right-parenthes!)
  (goal level-goal set-level-goal!))


(define wall (make-gelement (make-gelement-type #f "__________") '()))
(define wall2 (make-gelement (make-gelement-type #f "|") '()))
(define apple (make-gelement (make-gelement-type #t "apple") '(4 5)))
(define air (make-gelement (make-gelement-type #t "") '()))

(define (make-level-1) (make-level
                        (vector
                         (vector air air air air air)
                         (vector wall wall wall wall wall)
                         (vector air air air air air)
                         (vector wall wall wall wall wall)
                         (vector air air air air air))
                        (make-parentheses 'left (cons 0 2) '(+ 1 2))
                        (make-parentheses 'right (cons 4 2) '())
                        "Hello World!"))

(define *level* (make-level-1))

(define (set-grid! x y val)
  (vector-set! (vector-ref (level-grid *level*) y) x val))

(define (funcall fun args)
  (cond ((equal? fun '+)
         (apply + args))
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
                  (dprint "Move!")
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
              (dprint "Goal!")
              (begin
                (set-parentheses-content! left-parenthes '())
                (set-parentheses-content! right-parenthes `(,val))
              )
            )))))

;; Draw
(define (draw prev-time)
  (clear-rect context 0.0 0.0 game-width game-height)
  (set-fill-color! context "#140c1c")
  (set-text-align! context "center")
  (set-font! context "bold 24px monospace")
  (let ((grid (level-grid *level*))
        (left-parenthes (level-left-parenthes *level*))
        (right-parenthes (level-right-parenthes *level*)))
    ; Draw grid
    (do ((i 0 (+ i 1)))
        ((= i (vector-length grid)))
      (do ((j 0 (+ j 1)))
          ((= j (vector-length grid)))

        (let* ((gele (vector-ref (vector-ref grid j) i))
               (avg-width (/ game-width (vector-length grid)))
               (avg-hight (/ game-height (vector-length grid))))
          (fill-text context (gelement-type-iamge (gelement-type gele)) (* 100 (+ i 1)) (* 50 (+ j 1))))))

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
      (set! *level* (make-level-1))
      (request-animation-frame draw-callback))
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
