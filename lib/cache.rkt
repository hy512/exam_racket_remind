#lang racket

(require uuid)

(require "./cache_header.rkt")
(require "./exception.rkt")
(require "./logger.rkt")
(require (prefix-in memcached_ "./cache_memcached.rkt"))

(provide
    make_cache_context
    destory_cache_context
    cache_put
    cache_get
    cache_name_set
)

(define (make_cache_context params)
    (define type (hash-ref params 'type))
    (log_info logger_lib_cache (format "构建缓存 '~a'" type))

    (define conn (cond
        [(string=? type "memcached") (memcached_make_cache_context params)]
        [else (raise_routine_exception (format "~a 未实现缓存类型" type))]
    ))

    (define cache_ctx (struct_cache_context))
    (set-struct_cache_context-connection! cache_ctx conn)
    (set-struct_cache_context-parameters! cache_ctx params)
    cache_ctx
)

(define (cache_name_set cache_ctx cache_name)
    (set-struct_cache_context-cache_name! cache_ctx cache_name)
)

; @returns: void
(define (cache_put cache_ctx key value)
    (define params (struct_cache_context-parameters cache_ctx))
    (define type (hash-ref params 'type))

    (cond
        [(string=? type "memcached") (memcached_cache_put cache_ctx key value)]
        [else (raise_routine_exception (format "~a 未实现缓存类型" type))]
    )
)

; returns: maybe<any>
(define (cache_get cache_ctx key)
    (define params (struct_cache_context-parameters cache_ctx))
    (define type (hash-ref params 'type))

    (cond
        [(string=? type "memcached") (memcached_cache_get cache_ctx key)]
        [else (raise_routine_exception (format "~a 未实现缓存类型" type))]
    )
)

(define (destory_cache_context cache_ctx)
    (define params (struct_cache_context-parameters cache_ctx))
    (define type (hash-ref params 'type))

    (log_info logger_lib_cache (format "销毁缓存 '~a'" type))

    (cond
        ; 不需要释放资源
        [(string=? type "memcached") (void)]
        [else (raise_routine_exception (format "~a 未实现缓存类型" type))]
    )
)
