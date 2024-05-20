(define-module (console)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (console-log))


(define-foreign console-log
  "console" "log"
  (ref extern) -> (ref extern))
