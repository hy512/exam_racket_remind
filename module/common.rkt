#lang racket

(require racket/undefined)
(require web-server/http)
(require uuid)
(require data/maybe)

(require "../dao/dao.rkt")
(require "../lib/cache.rkt")
(require "../lib/http.rkt")
(require "../lib/exception.rkt")
(require "../lib/logger.rkt")
(require "../lib/config.rkt")
(require "../lib/type.rkt")

(provide
    request_handler_chain
    request_before_database
    request_after_database
    request_after_session
    request_before_attrs_bind
    request_before_validator
    request_before_validator_body
    request_before_validator_body_list
    request_before_validator_querystring
    request_before_validator_session
    remind_request_handler_before_chain
    remind_request_handler_after_chain
    remind_schedule_request_handler_before_chain
    remind_schedule_request_handler_after_chain
)

; request_before_validator 简化 :data_collector
(define (request_before_validator_body . validations)
    (keyword-apply request_before_validator
        (list '#:data_collector)
        (list (lambda (req_ctx)
            (define body_data (struct_request_context-body_json req_ctx))
            (log_info logger_module_session (format "~a" body_data))
            (if (hash? body_data)
                body_data
                (raise_routine_exception (format "参数错误请求体"))
            )
        ))
        validations
    )
)

; 验证 body 数据中的 list 成员
(define (request_before_validator_body_list list_key . validations)
    (lambda (req_ctx)

        (define body_data (struct_request_context-body_json req_ctx))

        (if (hash? body_data)
            body_data
            (raise_routine_exception (format "参数错误请求体"))
        )

        (define sym_list_key (if (string? list_key)
            (string->symbol list_key)
            list_key
        ))
        (log_info logger_module_session (format "~a" body_data))

        ; 验证 list
        ((request_before_validator
            #:data_collector (lambda (req_ctx) body_data)
            (cons sym_list_key list?)
        ) req_ctx)

        ; 验证各个参数值
        (for ([val (hash-ref body_data list_key)])
            ((keyword-apply request_before_validator
                (list '#:data_collector)
                (list (lambda (req_ctx) val))
                validations
            ) req_ctx)
        )
    )
)

; request_before_validator 简化 :data_collector
(define (request_before_validator_session . validations)
    (keyword-apply request_before_validator_rt
        (list '#:data_collector)
        (list (lambda (req_ctx key)
            (session_get req_ctx key)
        ))
        validations
    )
)

; request_before_validator 简化 :data_collector
(define (request_before_validator_querystring . validations)
    (keyword-apply request_before_validator_rt
        (list '#:data_collector)
        (list (lambda (req_ctx key)
            (let ([qs_val (request_querystring_first req_ctx key undefined)])
                (if (eq? qs_val undefined)
                    (raise_routine_exception (format "参数 ~a 错误" key))
                    (just qs_val)
                )
            )
        ))
        validations
    )
)

; 用于绑定数据到 request_context attributes
(define (request_before_attrs_bind attrs)
    (lambda (req_ctx)
        (for ([key (hash-keys attrs)])
            (request_context_attribute_save req_ctx (symbol->string key) (hash-ref attrs key))
        )
    )
)

; 一个验证器 before
; (#:data_collector: (req_ctx) => hash,
;   ...pair: [key: any, validate: (val: any) => boolean]
; ) => 
;   (req: request, req_ctx: struct_request_context) => any
(define (request_before_validator
        #:data_collector data_collector
        . validations
    )
    (lambda (req_ctx)
        (define data (data_collector req_ctx))

        (andmap (lambda (validation)
            (define key (car validation))
            (define validator (cdr validation))
            (cond
                [(not (hash-has-key? data key))
                    (raise_routine_exception (format "参数 ~a 错误" key))]
                [(not (validator (hash-ref data key)))
                    (raise_routine_exception (format "参数 ~a 错误" key))]
                [else #t]
            )
        ) validations)
    )
)

; 一个验证器 before, 实时获取数据; real time
; (#:data_collector: (req_ctx, key: string) => maybe<any>,
;   ...pair: [key: any, validate: (val: any) => boolean]
; ) => 
;   (req_ctx: struct_request_context) => any
(define (request_before_validator_rt #:data_collector data_collect . validations)
    (lambda (req_ctx)
        (log_debug logger_module_common "request_before_validator_rt -- begin")

        (andmap (lambda (validation)
            (define key (car validation))
            (define validator (cdr validation))

            (define maybe_val (data_collect req_ctx key))

            (cond
                [(nothing? maybe_val)
                    (raise_routine_exception (format "参数 ~a 缺失" key))]
                [(not (validator (from-just! maybe_val)))
                    (raise_routine_exception (format "参数 ~a 错误" key))]
                [else #t]
            )
        ) validations)
        (log_debug logger_module_common "request_before_validator_rt -- end")
    )
)

(define (request_before_metrics req_ctx)
    (request_context_attribute_save req_ctx "request_time" (current-seconds))
)

(define (request_before_session req_ctx)
    ; 查询 session-id
    (define session_id (request_header_find req_ctx "x-rem-token"))
    (set! session_id (if (string_blank session_id)
        (uuid-string)
        session_id
    ))

    (request_context_attribute_save req_ctx "session_id" session_id)
)

(define (request_before_cache req_ctx)
    (define cache_ctx (make_cache_context (config_value g_cfg_ctx "cache")))
    (request_context_attribute_save req_ctx "cache_ctx" cache_ctx)
)

(define (request_after_cache req_ctx)
    (define cache_ctx (request_context_attribute req_ctx "cache_ctx"))
    (if (eq? cache_ctx 'null)
        empty
        (destory_cache_context cache_ctx)
    )
)

(define (request_before_database req_ctx)
    (define db_ctx (make_dao_context (config_value g_cfg_ctx "data_source")))
    (request_context_attribute_save req_ctx "db_ctx" db_ctx)
)

(define (request_after_database req_ctx)
    (define db_ctx (request_context_attribute req_ctx "db_ctx"))
    (if (eq? db_ctx 'null)
        empty
        (destory_dao_context db_ctx)
    )
)

; 检查 response 是合适对象
(define (request_after_response req_ctx)
    (define resp (struct_request_context-response req_ctx))

    (set-struct_request_context-response! req_ctx (if (response? resp)
        resp
        (result_failure "响应数据类型错误")
    ))
)

; 重新设置 session 有效期
; * keys: list<string>, 需要重新设置 session 的 key 值
(define (request_after_session . keys)
    (lambda (req_ctx)
        (for ([s_key keys])
            (define s_val (session_get req_ctx s_key))
            (if (nothing? s_val)
                void
                (session_set req_ctx s_key (from-just! s_val))
            )
        )
    )
)

; 模块通常 before after
(define remind_request_handler_before_chain (request_handler_chain
    #:logger logger_module_session
    request_before_metrics
    request_before_session
    request_before_cache
    request_before_database
))

(define remind_request_handler_after_chain (request_handler_chain
    #:exn_separate #t
    #:logger logger_module_session
    request_after_cache
    request_after_database
    request_after_response
))

; 模块定时通常 before falter
(define remind_schedule_request_handler_before_chain (request_handler_chain
    #:logger logger_module_session
    request_before_metrics
    request_before_cache
    request_before_database
))

(define remind_schedule_request_handler_after_chain (request_handler_chain
    #:exn_separate #t
    #:logger logger_module_session
    request_after_cache
    request_after_database
))
