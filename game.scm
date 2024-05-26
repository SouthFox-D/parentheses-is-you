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
  (make-level grid left-parenthes right-parenthes)
  level?
  (grid level-grid set-level-grid!)
  (left-parenthes level-left-parenthes set-left-parenthes!)
  (right-parenthes level-right-parenthes set-right-parenthes!))


(define wall (make-gelement (make-gelement-type #f "wall") '()))
(define apple (make-gelement (make-gelement-type #t "apple") '(4 5)))
(define air (make-gelement (make-gelement-type #t "air") '()))

(define left-parenthes (make-parentheses 'left (cons 0 0) '(+ 1 2 3)))
(define right-parenthes (make-parentheses 'right (cons 1 1) '(1 2 3)))

(define ppp (vector (vector apple apple apple apple wall)
                    (vector air air air air air)
                    (vector apple wall wall wall wall)
                    (vector wall #nil wall wall wall)
                    (vector wall wall wall wall wall)))

(define *level* (make-level ppp left-parenthes right-parenthes))

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

(define (collide-gelement! x y parenthes)
  (dprint "x" x)
  (dprint "y" y)
  (dprint "content" (parentheses-content parenthes))
  (dprint "par pos" (parentheses-pos parenthes))
  (if (and (>= x 0) (>= y 0))
      (let ((e (vector-ref (vector-ref (level-grid *level*) y) x))
            (len (vector-length (level-grid *level*))))
        (if (and  (<= x len) (<= y len))
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
        (right-parenthes (level-right-parenthes level)))
    (if (equal? (parentheses-pos left-parenthes) (parentheses-pos right-parenthes))
        (dprint "eval!"
                (eval-parenthes
                 (append (parentheses-content left-parenthes)
                         (parentheses-content right-parenthes)))))))
;; Draw
(define (draw prev-time)
  (clear-rect context 0.0 0.0 game-width game-height)
  (set-fill-color! context "#140c1c")
  (set-text-align! context "center")
  (set-font! context "bold 24px monospace")
  (fill-text context "Hello, World! :)" *element-x* (/ game-height 2.0))
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(request-animation-frame draw-callback)

;; Input
(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (cond
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
      (collide-gelement! (car (parentheses-pos (level-right-parenthes *level*))) (+ (cdr (parentheses-pos (level-right-parenthes *level*))) 1) (level-right-parenthes *level*)))
     )
    (collide-pareneheses! *level*)
    )
  )

(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
