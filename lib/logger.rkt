#lang racket

(require json)
(require racket/date)
(require racket/undefined)

(require "config.rkt")

(provide
    logger_loop
    logger_stop
    log_info
    log_debug
    logger_root
    logger_dao
    logger_dao_calendar_activity
    logger_dao_userwww
    logger_dao_userwww_auth
    logger_main
    logger_lib
    logger_lib_cache
    logger_lib_http
    logger_service_http
    logger_service_schedule
    logger_module_calendar_activity
    logger_module_common
    logger_module_session
)

; 反向 rest 移除最后一个元素将
(define (list_rest_r lst)
    (drop-right lst 1)
)

; 使用特定分隔符拼接数据
; * strs: list<string>
(define (string_join [delimiter ","] . strs)
    ((compose1
        (lambda (lst) (apply string-append lst))
        list_rest_r
        flatten
    ) (map (lambda (str)
        (list str delimiter)
    ) strs))
)

(define logger_root (make-logger 
    'root
    #f
    'debug
))

(define logger_dao (make-logger 
    'dao
    logger_root
    'debug
))

(define logger_lib (make-logger 
    'lib
    logger_root
    'debug
))

(define logger_main (make-logger 
    'main
    logger_root
    'debug
))

(define logger_module (make-logger 
    'module
    logger_root
    'debug
))

(define logger_service (make-logger 
    'service
    logger_root
    'debug
))

(define logger_dao_calendar_activity (make-logger 
    'dao_calendar_activity
    logger_dao
    'debug
))

(define logger_dao_userwww (make-logger 
    'dao_userwww
    logger_dao
    'debug
))

(define logger_dao_userwww_auth (make-logger 
    'dao_userwww_auth
    logger_dao
    'debug
))

(define logger_lib_cache (make-logger 
    'lib_cache
    logger_lib
    'debug
))

(define logger_lib_http (make-logger 
    'lib_http
    logger_lib
    'debug
))

(define logger_module_common (make-logger 
    'module_common
    logger_module
    'debug
))

(define logger_module_session (make-logger 
    'module_session
    logger_module
    'debug
))

(define logger_module_calendar_activity (make-logger 
    'module_calendar_activity
    logger_module
    'debug
))

(define logger_service_http (make-logger 
    'http
    logger_service
    'debug
))

(define logger_service_schedule (make-logger 
    'service_schedule
    logger_service
    'debug
))

(define m_cfg_ctx (make_config_context))

(define logger_root_rec (
    make-log-receiver
    logger_root
    'debug
))

; 格式化错误信息栈
(define (exn_trace_message err)
    (define list_ctx (continuation-mark-set->context (exn-continuation-marks err)))
    (define list_msg (map (lambda (trace)
        (define proc_name (if (vector? trace)
            (vector-ref trace 0)
            (car trace)
        ))
        (define src_loc (if (vector? trace)
            (vector-ref trace 1)
            (cdr trace)
        ))
        (define realm_name (if (vector? trace)
            (vector-ref trace 2)
            #f
        ))
        ; 格式化错误信息
        ; realm:procedure:srcfile:line
        (format "\t~a:~a:~a:~a"
            realm_name
            proc_name
            (if (eq? #f src_loc)
                #f
                (srcloc-source src_loc)
            )
            (if (eq? #f src_loc)
                #f
                (srcloc-line src_loc)
            )
        )
    ) list_ctx))

    (string-append (exn-message err) "\n"
        (apply string_join "\n" list_msg)
    )
)

(define (log_message logger level message [err undefined])
    (current-logger logger)
    (date-display-format 'iso-8601)
    (define thread_name (object-name (current-thread)))
    (define msg_err (if (eq? err undefined)
        message
        (format "~a~n~a" message (exn_trace_message err))
    ))
    (log-message logger level (jsexpr->string (hasheq
        'thread (symbol->string thread_name)
        'message msg_err
        'datetime (date->string (current-date) #t)
    )))
)

(define (log_debug logger message [err undefined])
    (log_message logger 'debug message err)
)

(define (log_info logger message [err undefined])
    (log_message logger 'info message err)
)

(define (logger_adapter log_rec)

    ; 提取日志中消息内容
    (define list_match (regexp-match #rx"^[a-z_]+: (({.*})|(.*))$" (hash-ref log_rec 'message)))

    (define log_payload_str (if (or (eq? list_match #f) (eq? (list-ref list_match 2) #f))
        "{}"
        (list-ref list_match 2)
    ))
    (define log_payload (string->jsexpr log_payload_str))

    (define log_message (cond
        [(eq? list_match #f) (hash-ref log_rec 'message)]
        [(eq? (list-ref list_match 2) #f) (list-ref list_match 1)]
        [else (hash-ref log_payload 'message)]
    ))
    ; 日志线程
    (define log_thread (if (or (eq? list_match #f) (eq? (list-ref list_match 2) #f))
        "none"
        (hash-ref log_payload 'thread)
    ))
    ; 日志时间
    (date-display-format 'iso-8601)
    (define log_datetime (if (or (eq? list_match #f) (eq? (list-ref list_match 2) #f))
        (date->string (current-date) #t)
        (hash-ref log_payload 'datetime)
    ))

    (define log_rec_eject (hash-copy log_rec))
    (hash-set*! log_rec_eject
        'message log_message
        'thread log_thread
        'datetime log_datetime
    )
    (for ([adapter (list logger_adapter_stdout)])
        (adapter log_rec_eject)
    )
)

(define (logger_adapter_stdout log_rec)
    (display (hash-ref log_rec 'datetime) (current-output-port))
    (display " [" (current-output-port))
    (display (hash-ref log_rec 'thread) (current-output-port))
    (display "] " (current-output-port))
    (display (hash-ref log_rec 'level) (current-output-port))
    (display " " (current-output-port))
    (display (hash-ref log_rec 'logger) (current-output-port))
    (display " - " (current-output-port))
    (displayln
        (hash-ref log_rec 'message)
        (current-output-port)
    )
)

; log_rec: {
;     level: string
;     message: string
;     logger: string
;     thread: string
;     datetime: string
; }
(define (logger_loop)
    (println "日志线程启动")
    (define log_thr (thread (lambda ()
        (do (
                [thr_rec (thread-try-receive) (thread-try-receive)]
                [log_vec (sync/timeout 0.5 logger_root_rec) (sync/timeout 0.5 logger_root_rec)]
                ; 线程中断变量, 保留 mailbox 中收到的中断消息
                [thr_interrupt #f
                    (or
                        thr_interrupt
                        (and
                            (not (eq? thr_rec #f))
                            (eq? (hash-ref thr_rec 'type) "thread_interrupt")))
                ]
            )
            (
                ; 线程已经中断, 且没有日志输入. 则退出.
                (and
                    (eq? log_vec #f)
                    thr_interrupt
                )
            )
            (cond
                [(not (eq? log_vec #f))
                    (logger_adapter (hasheq
                        'level (vector-ref log_vec 0)
                        'message (vector-ref log_vec 1)
                        'logger (vector-ref log_vec 3)))
                ]
            )
        )
    )))

    ; 保留线程信息, 以便使用全局停止函数全部停止.
    (config_value_push m_cfg_ctx "list_thread" log_thr)

    (lambda ()
        (thread-send log_thr
            (hasheq
                'type "thread_interrupt"
                'payload (make-hasheq))
            (lambda()
                (current-logger logger_root)
                (log-error "日志线程终止失败"))
        )
        (thread-wait log_thr)
    )
)

(define (logger_stop)
    (define list_thr (config_value m_cfg_ctx "list_thread" empty))
    (log_info logger_lib (format "日志线程停止, 数量 ~a" (length list_thr)))

    (for ([log_thr list_thr]
            #:when (not (thread-dead? log_thr))
        )
        (thread-send log_thr
            (hasheq
                'type "thread_interrupt"
                'payload (make-hasheq))
            (lambda()
                (current-logger logger_root)
                (log-error "日志线程终止失败"))
        )
        (thread-wait log_thr)
    )
)