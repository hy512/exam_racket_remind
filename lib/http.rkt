#lang racket

(require net/url)
(require web-server/http)
(require json)
(require racket/undefined)

(require "./cache.rkt")
(require "./logger.rkt")
(require "./exception.rkt")


(provide
    result_success
    result_failure
    request_querystring
    request_querystring_first
    request_header
    request_header_contenttype
    request_header_find
    make_request_context
    struct_request_context
    struct_request_context-body_json
    struct_request_context-response
    set-struct_request_context-response!
    request_context_attribute
    request_context_attribute_save
    request_handler_chain
    request_handler_wrapper
    session_set
    session_get
)

; attributes: hash, 存放自定义数据
(struct struct_request_context (
    [list_header #:auto #:mutable]
    [list_querystring #:auto #:mutable]
    [body_json #:auto #:mutable]
    [attributes #:auto #:mutable]
    [request #:auto #:mutable]
    [response #:auto #:mutable]
)
    #:auto-value undefined)

; 建立一个请求数据负载对象
(define (make_request_context req)
    (define req_ctx (struct_request_context))
    (define list_header (request_header req))
    (set-struct_request_context-list_header! req_ctx list_header)
    (set-struct_request_context-list_querystring! req_ctx (request_querystring req))

    (set-struct_request_context-body_json! req_ctx 
        (if (and
                (regexp-match? #rx"^application/json" (request_header_contenttype list_header))
                (not (eq? (request-post-data/raw req) #f))
            )
            (string->jsexpr (bytes->string/utf-8 (request-post-data/raw req)))
            'null
        )
    )
    (set-struct_request_context-attributes! req_ctx (hasheq))
    (set-struct_request_context-request! req_ctx req)
    req_ctx
)

; 构造空对象
(define (make_request_context_empty)
    (define req_ctx (struct_request_context))
    (set-struct_request_context-list_header! req_ctx empty)
    (set-struct_request_context-list_querystring! req_ctx empty)
    (set-struct_request_context-body_json! req_ctx 'null)
    (set-struct_request_context-attributes! req_ctx (hasheq))

    req_ctx
)

; 查询属性
; * key: string
(define (request_context_attribute req_ctx key [def 'null])
    (define attrs (struct_request_context-attributes req_ctx))
    (define key_s (string->symbol key))
    (if (hash-has-key? attrs key_s)
        (hash-ref attrs key_s)
        def
    )
)

; 设置属性
; * key: string
(define (request_context_attribute_save req_ctx key val)
    (define attrs (struct_request_context-attributes req_ctx))
    (define key_s (string->symbol key))
    (set-struct_request_context-attributes! req_ctx (hash-set attrs key_s val))
)

; 查找 list 元组的第一个值
; * returns: string | any
(define (list_tuple_first list_tuple key [def ""])
    (define index_key (index-where list_tuple (lambda (qs_pair)
        (string=? (list-ref qs_pair 0) key)
    )))

    (if (not (eq? #f index_key))
        (second (list-ref list_tuple index_key))
        def
    )
)

; 嵌套参数值
; list<[key: string, value: string]>
(define (request_querystring req)
    ; 补协议和主机部分, 便于解析 url
    (define req_url (request-uri req))
    (define qs (url-query req_url))

    (define qs_list (map
        (lambda (pair)
            (list (symbol->string (car pair)) (cdr pair))
        )
        qs)
    )

    qs_list
)

; 查询 request_querystring 处理结果样式数据参数
; * qs_list: req_ctx|list<[key, val]>
; * def: string, 默认值
; * returns: string
(define (request_querystring_first p_qs_list key [def ""])
    ; 参数调整
    (define qs_list (if (struct_request_context? p_qs_list)
        (struct_request_context-list_querystring p_qs_list)
        p_qs_list
    ))

    (define index_key (index-where qs_list (lambda (qs_pair)
        (string=? (list-ref qs_pair 0) key)
    )))

    (if (not (eq? #f index_key))
        (second (list-ref qs_list index_key))
        def
    )
)

; 解析请求头
; * returns: list<[key: string, value: string]>
(define (request_header req)
    (define list_header (request-headers/raw req))
    (map (lambda (header)
        (list (bytes->string/utf-8 (header-field header)) (bytes->string/utf-8 (header-value header)))
    ) list_header)
)

; @returns: string
(define (request_header_find req_ctx header)
    (define list_header (struct_request_context-list_header req_ctx))
    (list_tuple_first list_header header)
)

; * returns: string
(define (request_header_contenttype list_header)
    (list_tuple_first list_header "Content-Type")
)

; 解析 body 请求体
(define (request_body_json req)
    empty
)

; 封装运行
; before 和 after 一定执行. before 失败不执行 handler.
; req: request, 因有些场景需要这个执行模型, 所以可以传 empty_context 构建空 req_ctx
; before, handler, after: (req: request, req_ctx) => resp: response
(define (request_handler_wrapper req handler
    #:before [before void] #:after [after void]
    #:empty_context [empty_context #f]
    #:logger [logger logger_lib_http])

    ; 异常捕获环境, 构造 req_ctx
    (with-handlers (
            [exn? (lambda (err)
                (current-logger logger)
                (log-info (exn-message err))
                (result_failure (exn-message err))
            )]
        )

        (define req_ctx (if empty_context
            (make_request_context_empty)
            (make_request_context req)
        ))

        ; 异常捕获环境, 运行 before, handler
        (define resp_data 
            (with-handlers (
                    [routine_exception? (lambda (err)
                        (result_failure (exn-message err))
                    )]
                    [exn? (lambda (err)
                        (log_info logger "request_handler_wrapper" err)
                        (result_failure (exn-message err))
                    )]
                )
                (before req_ctx)
                (handler req_ctx)
            )
        )
        (set-struct_request_context-response! req_ctx resp_data)

        ; 异常捕获环境, 运行 after
        (with-handlers (
                [exn? (lambda (err)
                    (current-logger logger)
                    (log-info (exn-message err))
                    (result_failure (exn-message err))
                )]
            )
            (after req_ctx)
        )
        ; 响应数据
        (struct_request_context-response req_ctx)
    )
)

; 通常 after 处理 exn_separate #t, 一个 after 异常其他 after 继续执行.
; before exn_separate #f, 一个 before 异常其他 before 不处理
(define (request_handler_chain #:exn_separate [exn_separate #f] #:logger [logger logger_lib_http] . handlers)
    (lambda (req_ctx)
        (for ([handler handlers])
            (if exn_separate
                (with-handlers (
                        [exn? (lambda (err)
                            (current-logger logger)
                            ; (log-info (exn-message err))
                            (log-error (format "~a" err))
                            (result_failure (exn-message err))
                        )]
                    )
                    (handler req_ctx)
                )
                (handler req_ctx)
            )
            
        )
    )
)

(define (result_success data [msg "OK"] [code 0])
    (response/jsexpr (hasheq
        'data data
        'msg msg
        'code code
    ))
)

(define (result_failure msg [data (hasheq)] [code 1])
    (response/jsexpr (hasheq
        'data data
        'msg msg
        'code code
    ))
)

; 设置 session
(define (session_set req_ctx key value)
    (define cache_ctx (request_context_attribute req_ctx "cache_ctx"))
    (define session_id (request_context_attribute req_ctx "session_id"))
    (cache_name_set cache_ctx "session")
    (cache_put cache_ctx (string-append session_id "_" key) value)
)

; returns: maybe<any>
(define (session_get req_ctx key)
    (define cache_ctx (request_context_attribute req_ctx "cache_ctx"))
    (define session_id (request_context_attribute req_ctx "session_id"))
    (cache_name_set cache_ctx "session")
    (cache_get cache_ctx (string-append session_id "_" key))
)

