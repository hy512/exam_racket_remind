#lang racket

(require data/maybe)
(require db)

(require "../dao.rkt")
(require "../../lib/type.rkt")
(require "../../lib/logger.rkt")

(provide
    find_userwww_by_account
    find_userwww_by_uuid
    save_userwww
)

(define columns_userwww (list "id"
    "user_uuid" "user_account" "user_name"
    "last_login_time" "create_time" "update_time" "delete_time" "deleted"))
(define columns_userwww_sql (apply string_join ", " columns_userwww))
(define columns_userwww_sql_save (apply string_join ", " (cdr columns_userwww)))

; 映射查询结果
; * row: vector
(define (result_map_userwww row)
    (make-hasheq (map (lambda (col val)
        (cons (string->symbol col) val)
    ) columns_userwww (if (vector? row) (vector->list row) row)))
)

; 反转映射
(define (result_map_r_userwww hsh [columns columns_userwww])
    (map (lambda (key)
        (hash-ref hsh (string->symbol key))
    ) columns)
)

; userwww: hash
; returns: void
(define (save_userwww dao_ctx userwww)
    (define conn (struct_dao_context-connection dao_ctx))

    (log_info logger_dao_userwww (format "INSERT INTO rem_userwww (~a) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" columns_userwww_sql_save))
    (log_info logger_dao_userwww (format "~a" (result_map_r_userwww userwww (cdr columns_userwww))))

    (define prep_userwww_save (prepare conn (format "INSERT INTO rem_userwww (~a) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" columns_userwww_sql_save)))

    (query-exec conn
        (bind-prepared-statement prep_userwww_save (result_map_r_userwww userwww (cdr columns_userwww)))
    )
)


; 通过用户账号查找用户
; * returns: maybe<hash>
(define (find_userwww_by_account dao_ctx user_account)
    (define conn (struct_dao_context-connection dao_ctx))

    (log_info logger_dao_userwww (format "select ~a from rem_userwww where user_account = ? limit 1" columns_userwww_sql))
    (log_info logger_dao_userwww (format "~a" (list user_account)))

    (define prep_userwww (prepare conn (format "select ~a from rem_userwww where user_account = ? limit 1" columns_userwww_sql)))
    (define list_userwww (query-rows conn
        (bind-prepared-statement prep_userwww (list user_account))
    ))
    (set! list_userwww (map result_map_userwww list_userwww))

    (if (empty? list_userwww)
        nothing
        (just (first list_userwww))
    )
)

; 通过用户 uuid 查找用户
; * returns: maybe<hash>
(define (find_userwww_by_uuid dao_ctx user_uuid)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "select ~a from rem_userwww where user_uuid = ? limit 1" columns_userwww_sql))
    (define list_args (list user_uuid))

    (log_info logger_dao_userwww str_sql)
    (log_info logger_dao_userwww list_args)

    (define prep_userwww (prepare conn str_sql))
    (define list_userwww (query-rows conn
        (bind-prepared-statement prep_userwww list_args)
    ))
    (set! list_userwww (map result_map_userwww list_userwww))

    (if (empty? list_userwww)
        nothing
        (just (first list_userwww))
    )
)
