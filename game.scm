(import (scheme base)
        (scheme inexact)
        (hoot ffi)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window))


(define game-width    640.0)
(define game-height   480.0)

(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))

;; Draw
(define (draw prev-time)
  (set-fill-color! context "#140c1c")
  (set-text-align! context "center")
  (set-font! context "bold 24px monospace")
  (fill-text context "Hello, World! :)" (/ game-width 2.0) (/ game-height 2.0))
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(request-animation-frame draw-callback)
