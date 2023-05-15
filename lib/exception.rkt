#lang racket

(provide
    raise_routine_exception
    routine_exception
    routine_exception?
)

(struct routine_exception exn:fail (
))

(define (raise_routine_exception message)
    (raise (routine_exception message (current-continuation-marks)))
)