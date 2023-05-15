#lang racket

(require net/url)
(require web-server/http)
(require json)

(provide
    http_status_forbidden
)


(define (http_status_forbidden req)
    (response/empty
        #:code 403
    )
)

