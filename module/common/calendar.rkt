#lang racket

(require web-server/dispatch)
(require web-server/http)
(require json)

(require "resp.rkt")
(require "../../lib/calendar.rkt")
(require "../../lib/http.rkt")
(require "../common.rkt")

(provide calendar-list)

; (define-values (calendar-dispatch calendar-url)
;     (dispatch-rules
;         [("calendar" "list") #:method "get" calendar-list]
;         ; [else http_status_forbidden])
;     )
; )

; {
;     solar: {
;         year: string
;         month: string
;         day_of_month: string
;     }
;     lunar: {
;         year: string
;         month: string
;         day_of_month: string
;     }
; }
(define (calendar-list req)
    (request_handler_wrapper req calendar_list_inner
        #:before (request_handler_chain
            remind_request_handler_before_chain
            (request_before_validator_querystring
                (cons "year" (λ (val) (let ([num_val (string->number val)])
                    (and (integer? num_val) (> num_val 0))
                )))
                (cons "month" (λ (val) (let ([num_val (string->number val)])
                    (and (integer? num_val) (> num_val 0))
                )))
            )
        )
        #:after remind_request_handler_after_chain
    )
)

(define (calendar_list_inner req_ctx)
    (define year_str (request_querystring_first req_ctx "year" "0"))
    (define month_str (request_querystring_first req_ctx "month" "0"))

    (if (string=? year_str "0")
        (result_failure "参数 year 不正确")
        (result_success (calender (string->number year_str) (string->number month_str)))
    )
)
