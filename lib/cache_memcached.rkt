#lang racket

(require net/memcached)
(require json)
(require data/maybe)
(require racket/undefined)

(require "./cache_header.rkt")
(require "./logger.rkt")
(require "./exception.rkt")

(provide 
    make_cache_context
    cache_get
    cache_put
)

(define (make_cache_context params)
    (define params_memcached (hash-ref params 'memcached))
    (define conn (memcached (hash-ref params_memcached 'ip) (hash-ref params_memcached 'port)))
    conn
)

(define (cache_key cache_name key)
    (string->bytes/utf-8 (string-append cache_name ":" key))
)

(define (cache_value_serialize value)
    (define wrap (cond
        [(bytes? value) (hasheq '@type "bytes" '@value value)]
        [(string? value) (hasheq '@type "string" '@value value)]
        [else (hasheq '@type "hash" '@value value)]
    ))
    (string->bytes/utf-8 (jsexpr->string wrap))
)

(define (cache_value_deserialize value)
    (define wrap (string->jsexpr (bytes->string/utf-8 value)))
    (define type (hash-ref wrap '@type))
    (cond
        [(string=? type "bytes") (hash-ref wrap '@value)]
        [(string=? type "string") (hash-ref wrap '@value)]
        [(string=? type "hash") (hash-ref wrap '@value)]
        [else (raise_routine_exception (format "未知序列化类型 ~a" (hash-ref wrap '@type)))]
    )
)


; @param value
(define (cache_put cache_ctx key value)
    (define conn (struct_cache_context-connection cache_ctx))
    (define cache_name (struct_cache_context-cache_name cache_ctx))
    (define cache_name_cfg (cache_params_by_name cache_ctx cache_name))
    (log_debug logger_lib_cache (format "缓存配置 ~a" (jsexpr->string cache_name_cfg)))
    (define expiration (hash-ref cache_name_cfg 'expiration))
    ; 过期时间, 毫秒转秒
    ; (set! expiration (if (eq? expiration 0)
    ;     expiration
    ;     (/ expiration 1000)
    ; ))

    (memcached-set! conn
        (cache_key cache_name key)
        (cache_value_serialize value)
        #:expiration expiration)
)

(define (cache_get cache_ctx key)
    (define conn (struct_cache_context-connection cache_ctx))
    (define cache_name (struct_cache_context-cache_name cache_ctx))

    ; (define cac_val2 (memcached-get conn (cache_key cache_name key)))
    ; (log_debug logger_lib_cache (format "缓存查询--1 ~a" cache_key_))
    ; (log_debug logger_lib_cache (format "缓存查询--2 ~a" (key? cache_key_)))
    ; (log_debug logger_lib_cache (format "缓存查询--3 ~a" (cas? (memcached-get conn cache_key_))))
    ; (log_debug logger_lib_cache (format "缓存查询 ~a" (bytes? (memcached-get conn cache_key_))))
    ; (log_debug logger_lib_cache (format "缓存查询 ~a" (void? (memcached-get conn cache_key_))))
    ; (log_debug logger_lib_cache (format "缓存查询 ~a" (eq? #f (memcached-get conn (cache_key cache_name key)))))

    (define pair_val (let-values (
            [(cac_val cas_val) (memcached-get conn (cache_key cache_name key))]
        )
        (cons cac_val cas_val)
    ))

    (define cac_val (car pair_val))

    (if (eq? #f cac_val)
        nothing
        (just (cache_value_deserialize cac_val))
    )
)
