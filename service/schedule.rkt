#lang racket

(require crontab)

(require "../lib/config.rkt")
(require "../lib/type.rkt")
(require "../lib/logger.rkt")

(provide
    schedule_start
    schedule_stop
)

(define m_cfg_ctx (make_config_context))

(define (schedule_stop)
    (define args (config_value m_cfg_ctx "parameters"))
    (define list_stop (config_value m_cfg_ctx "list_func_stop" empty))

    (log_info logger_service_schedule "应用停止")
    (for ([s_stop list_stop])
        (s_stop)
    )
)

; 开启定时, 不会阻塞当前线程
(define (schedule_start args)

    ((hash-ref args 'on_start))
    (define list_crontab (hash-ref args 'crontab))

    (log_info logger_service_schedule (format "应用启动 ~a" (map (lambda (schedule_info)
        (format "~a ~a" (hash-ref schedule_info 'cron) (hash-ref schedule_info 'title))
    ) list_crontab)))

    ; (for ([hash_schedule list_crontab])
    ;     (config_value_push m_cfg_ctx "list_func_stop" 
    ;         (crontab [(hash-ref hash_schedule 'cron) (hash-ref hash_schedule 'handler)]))
    ; )
    ; (apply (lambda args
    ;     (crontab args)
    ; ) (list ["* * * * * *" println] ["* * * * * *" println]))

    (for ([hash_schedule list_crontab])
        (config_value_push m_cfg_ctx "list_func_stop" 
            (crontab [(hash-ref hash_schedule 'cron) (lambda (epoch_seconds)
                ((hash-ref hash_schedule 'handler) (hash_assign
                    (make-weak-hasheq (list
                        (cons 'request_time epoch_seconds)
                    ))
                    (hash-ref hash_schedule 'attrs)
                ))
            )]))
    )
    ; (define list_stop (apply crontab (map (lambda (schedule_info)
    ;     [(hash-ref schedule_info 'cron) (hash-ref schedule_info 'handler)]
    ; ) list_crontab)))

    (config_value_save m_cfg_ctx "parameters" args)

    schedule_stop
)