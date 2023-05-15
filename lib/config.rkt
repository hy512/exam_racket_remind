#lang racket

(require racket/file)
(require json)

(provide
    g_cfg_ctx
    make_config_context
    config_value
    config_value_save
    config_value_push
    config_load_file_json
)

; 防依赖循环
; lib/type.rkt
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

; data: hash, 存放配置数据
; {
;     // 程序目录 
;     home: string
; }
(struct struct_config_context (
    [data #:mutable]
))

(define g_cfg_ctx (struct_config_context (hasheq
    'http (hasheq
        'port 18000
        'host "0.0.0.0"
    )
)))

(define (make_config_context)
    (struct_config_context (hasheq))
)

(define (config_value cfg_ctx key [def 'null])
    (define data (struct_config_context-data cfg_ctx))
    (define key_s (string->symbol key))

    (if (hash-has-key? data key_s)
        (hash-ref data key_s)
        def
    )
)

(define (config_value_save cfg_ctx key val)
    (define data (struct_config_context-data cfg_ctx))
    (define key_s (string->symbol key))

    (set-struct_config_context-data! cfg_ctx (hash-set data key_s val))
)

; 往 list 数据追加值
; 会覆盖原有值, 如果已有值不是 list
(define (config_value_push cfg_ctx key val)
    (define data (struct_config_context-data cfg_ctx))
    (define key_s (string->symbol key))

    (define lst (if (hash-has-key? data key_s)
        (hash-ref data key_s)
        empty
    ))
    (set! lst (if (list? lst)
        lst
        empty
    ))

    (set-struct_config_context-data! cfg_ctx (hash-set data key_s (append lst (list val))))
)

; 从 json 文件读取内容
(define (config_load_file_json cfg_ctx filename)
    (define data (struct_config_context-data cfg_ctx))

    (define data_merged (hash_assign (make-weak-hasheq)
        data
        (string->jsexpr (bytes->string/utf-8 (file->bytes (build-path filename))))))

    (set-struct_config_context-data! cfg_ctx data_merged)

    data_merged
)