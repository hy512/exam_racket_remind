#lang racket

(require racket/undefined)
(require "./logger.rkt")

(provide
    string_blank
    string_not_blank
    string_join
    function_fork
    hash_value_or_else
    hash_assign
    hash_vals_are_any_blank
)

(define (hash_value_or_else hsh key [els undefined])
    (if (hash-has-key? hsh key)
        (hash-ref hsh key)
        els
    )
)

; 判断 hash 中所有 key 都是空的
(define (hash_vals_are_any_blank hsh . keys)
    (ormap (lambda (key)
        (define val (if (hash-has-key? hsh key)
            (hash-ref hsh key)
            undefined
        ))

        (cond
            [(eq? undefined undefined) #t]
            [(and (string? val) (string_blank val)) #t]
            [(and (list? val) (null? val))]
            [else (null? val)]
        )
    )
        keys)
)

; 检查 str 是否为空白
(define (string_blank str)
    (cond
        [(eq? (string-length str) 0) #t]
        [else
            (andmap char-whitespace? (string->list str))
        ]
    )
)
(define (string_not_blank str)
    (not (string_blank str))
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


; 反向 rest 移除最后一个元素将
(define (list_rest_r lst)
    (drop-right lst 1)
)

; 要求第一个值是可变 hash
; hashes: list<hash>
(define (hash_assign hsh . hashes)
    (for ([h hashes])
        (for ([key (hash-keys h)])
            (hash-set! hsh key (hash-ref h key))
        )
    )
    hsh
)

; 以相同参数调用多个函数进行处理
; 串行处理
(define (function_fork #:exn_separate [exn_separate #f] #:logger [logger logger_lib] join_func . handlers)
    (lambda args
        (apply join_func (map (lambda (handler)
            (if exn_separate
                (with-handlers (
                        [exn? (lambda (err)
                            (current-logger logger)
                            (log-info (exn-message err))
                            err
                        )]
                    )
                    (apply handler args)
                )
                (apply handler args)
            )
        )
            handlers
        ))
    )
)
