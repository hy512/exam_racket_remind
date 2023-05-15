#lang racket

(require db)

(require "../lib/logger.rkt")
(require "../lib/config.rkt")
(require "../lib/exception.rkt")

(provide
    make_dao_context
    destory_dao_context
    struct_dao_context
    struct_dao_context-connection
    database_schema_create
    result_map
    result_map_r
    sql_tran
    loop_batch_pages
)

(struct struct_dao_context (
    [parameters #:auto #:mutable]
    [connection #:auto #:mutable]
    [attributes #:auto #:mutable]
))

(define (thread_place_name)
    (string-append "pls:sqlite3:" (symbol->string (object-name (current-thread))))
)

(define (conn_sqlite params)
    (define params_sqlite (cond
        [(hash-has-key? params 'sqlite) (hash-ref params 'sqlite)]
        [else (raise_routine_exception "data_source.sqlite 参数不足")]
    ))

    (define place_name_str (thread_place_name))

    (log_info logger_dao (format "初始化数据库连接, place '~s', file '~s'"
        place_name_str
        (hash-ref params_sqlite 'filename)
    ))

    (define place_name (string->symbol place_name_str))

    (define cfg_filename (if (absolute-path? (hash-ref params_sqlite 'filename))
        (string->path (hash-ref params_sqlite 'filename))
        (build-path (config_value g_cfg_ctx "home") (hash-ref params_sqlite 'filename))
    ))

    (sqlite3-connect
        #:database (path->string cfg_filename)
        ; #:use-place 'place
    )
)

; * params: {
;    type: "sqlite"
;    sqlite?: {
;        filename: string
;    }
; }
(define (make_dao_context params)
    (define dao_ctx (struct_dao_context))
    (define conn (cond
        [(string=? (hash-ref params 'type) "sqlite")
            (conn_sqlite params)
        ]
        [else (raise (routine_exception (format "未实现的数据源 '~s'" (hash-ref params 'data_source))))]
    ))
    (set-struct_dao_context-parameters! dao_ctx params)
    (set-struct_dao_context-connection! dao_ctx conn)
    dao_ctx
)

(define (destory_dao_context dao_ctx)
    (define params (struct_dao_context-parameters dao_ctx))
    (define params_sqlite (hash-ref params 'sqlite))
    (define place_name_str (thread_place_name))

    (log_info logger_dao (format "关闭数据库连接, place '~s', file '~s'"
        place_name_str
        (hash-ref params_sqlite 'filename)
    ))

    (disconnect (struct_dao_context-connection dao_ctx))
)

; 创建数据库表
(define (database_schema_create)
    (define cfg_ds (config_value g_cfg_ctx "data_source"))
    (define dao_ctx (make_dao_context cfg_ds))
    (define conn (struct_dao_context-connection dao_ctx))

    (define cfg_schemata (hash-ref cfg_ds 'schema))

    ; 读取数据并执行
    (for ([cfg_schema cfg_schemata])
        (define cfg_schema_abs (if (absolute-path? cfg_schema)
            (string->path cfg_schema)
            (build-path (config_value g_cfg_ctx "home") cfg_schema)
        ))

        (log_info logger_dao (format "初始化数据库, 使用 '~s'" (path->string cfg_schema_abs)))
        (define str_schema (bytes->string/utf-8 (file->bytes cfg_schema_abs)))
        (in-query conn str_schema)
    )

    (destory_dao_context dao_ctx)
)

; 查询属性
; * key: string
(define (dao_context_attribute dao_ctx key [def 'null])
    (define attrs (struct_dao_context-attributes dao_ctx))
    (define key_s (string->symbol key))
    (if (hash-has-key? attrs key_s)
        (hash-ref attrs key_s)
        def
    )
)

; 设置属性
; * key: string
(define (dao_context_attribute_save dao_ctx key val)
    (define attrs (struct_dao_context-attributes dao_ctx))
    (define key_s (string->symbol key))
    (set-struct_dao_context-attributes! dao_ctx (hash-set attrs key_s val))
)

; 映射查询结果
; * rows: list
(define (result_map columns rows)
    (map (lambda (row)
        (make-hasheq (map (lambda (col val)
            (cons (string->symbol col) val)
        )
            columns
            (if (vector? row) (vector->list row) row)
        ))
    ) rows)
)

; 反转映射
(define (result_map_r hsh columns)
    (map (lambda (key)
        (hash-ref hsh (string->symbol key))
    ) columns)
)

; 事务中执行 func, 异常时执行 onerr
; func: (dao_ctx)->void
; onerr: (dao_ctx, exn)->void
(define (sql_tran dao_ctx func [onerr void])

    (define conn (struct_dao_context-connection dao_ctx))

    (with-handlers (
            [exn? (lambda (err)
                (if (eq? void onerr)
                    (onerr)
                    (onerr dao_ctx err)
                )
            )]
        )

        (call-with-transaction conn
            (lambda ()(func dao_ctx))
            #:isolation 'repeatable-read
        )
    )
)

; 循环分页读取
; identy: T
; supplier: (dao_ctx, page_size, page_number) -> list<E>
; func: (T, list<E>) -> E
(define (loop_batch_pages
    dao_ctx max_loop max_size supplier identity_v func)

    (define loop_counter 1)
    (define page_number 1)

    (define loop_break #f)
    (do
        ([list_rows (supplier dao_ctx max_size page_number)])
        (loop_break)

        (set! identity_v (func identity_v list_rows))

        ; 退出条件设置
        (set! loop_break (or
            ; 到达最后一页
            (not (eq? (length list_rows) max_size))
            ; 循环次数上限
            (and
                (not (eq? -1 max_loop))
                (>= loop_counter max_loop)
            )
        ))
        ; 累加查询次数
        (set! loop_counter (+ loop_counter 1))
    )

    identity_v
)
