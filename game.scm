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

(dprint "game-height" game-height)
(console-log canvas)

;; Input
(define key:left "ArrowLeft")
(define key:right "ArrowRight")

(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (cond
     ((string=? key key:left)
      (dprint "key:" key)
      (set! *element-x* (- *element-x* 10)))
     ((string=? key key:right)
      (dprint "key:" key)
      (set! *element-x* (+ *element-x* 10))))))

(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))


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

(define wall (make-gelement (make-gelement-type #f "wall") #nil))
(define apple (make-gelement (make-gelement-type #t "apple") #nil))

(define ppp (vector (vector apple wall wall apple wall)
                    (vector wall wall wall wall wall)
                    (vector apple wall wall wall wall)
                    (vector wall wall wall wall wall)
                    (vector wall wall wall wall wall)))

(define (collide-gelement! x y)
  (let ((e (vector-ref (vector-ref ppp x) y)))
    (if (gelement-interact? (gelement-type e))
        (begin (display "True")
               (newline)
               (display (gelement-type-iamge (gelement-type e))))
        (display "False"))))

(define (funcall fun args)
  (cond ((equal? fun '+)
         (apply + args))
        ((equal? fun 'string-append)
         (apply string-append args))
        ))
