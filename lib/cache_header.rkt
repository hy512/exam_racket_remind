#lang racket

(require "./type.rkt")

(provide
    struct_cache_context
    struct_cache_context-cache_name
    struct_cache_context-connection
    struct_cache_context-parameters
    set-struct_cache_context-connection!
    set-struct_cache_context-parameters!
    set-struct_cache_context-cache_name!
    cache_params_by_name
)

(struct struct_cache_context (
    [cache_name #:auto #:mutable]
    [parameters #:auto #:mutable]
    [connection #:auto #:mutable]
))

; 通过缓存名称获取缓存参数
(define (cache_params_by_name cache_ctx name)
    (define cache_cfg (struct_cache_context-parameters cache_ctx))
    (define names_cfg (if (hash-has-key? cache_cfg 'names)
        (hash-ref cache_cfg 'names)
        (make-hasheq)
    ))
    (define name_sym (string->symbol name))
    (if (hash-has-key? names_cfg name_sym)
        (hash_assign (make-weak-hasheq) (hash-ref cache_cfg 'config) (hash-ref names_cfg name_sym))
        (hash-ref cache_cfg 'config)
    )
)