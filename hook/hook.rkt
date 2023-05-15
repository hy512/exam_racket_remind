#lang racket

(require "../lib/logger.rkt")
(require "../lib/type.rkt")
(require "../dao/dao.rkt")
(require "../schedule/remind.rkt")
(require "../service/schedule.rkt")

(provide
    func_service_start
    func_service_stop
)

; start 不隐藏错误, stop 隐藏.
(define func_service_start (function_fork void
    #:exn_separate #f
    logger_loop
    database_schema_create
    ; 启动定时
    (lambda()
        (schedule_start (hasheq
            'on_start void
            'crontab schedule_remind
        )
    ))
))

(define func_service_stop (function_fork void
    #:exn_separate #t
    schedule_stop
    logger_stop
))

(define func_request_start (list
))

(define func_request_stop (list
))