#lang racket

(require data/maybe)
(require db)

(require "../dao.rkt")
(require "../../lib/type.rkt")
(require "../../lib/logger.rkt")

(provide
    find_userwww_by_user_uuid_auth_type_not_deleted
    save_userwww_auth
)

(define columns_userwww_auth (list "id"
    "user_id" "user_uuid" "auth_type" "login_pass"
    "create_time" "update_time" "delete_time" "deleted"
))
(define columns_userwww_auth_sql (apply string_join ", " columns_userwww_auth))
(define columns_userwww_auth_sql_save (apply string_join ", " (cdr columns_userwww_auth)))

; userww_auth hash
; returns: void
(define (save_userwww_auth dao_ctx userww_auth)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "INSERT INTO rem_userwww_auth (~a) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" columns_userwww_auth_sql_save))
    (define list_args (result_map_r userww_auth (cdr columns_userwww_auth)))

    (log_info logger_dao_userwww_auth str_sql)
    (log_info logger_dao_userwww_auth (format "~a" list_args))

    (apply query-exec conn str_sql list_args)
)

; 通过 uuid 和认证类型查找
; * returns: maybe<hash>
(define (find_userwww_by_user_uuid_auth_type_not_deleted dao_ctx user_uuid auth_type)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "select ~a from rem_userwww_auth where user_uuid = ? and auth_type = ? and deleted = 0 limit 1" columns_userwww_auth_sql))
    (define list_args (list user_uuid auth_type))

    (log_info logger_dao_userwww_auth str_sql)
    (log_info logger_dao_userwww_auth (format "~a" list_args))

    (define list_userwww_auth (apply query-rows conn str_sql list_args))
    (set! list_userwww_auth (result_map columns_userwww_auth list_userwww_auth))

    (if (empty? list_userwww_auth)
        nothing
        (just (first list_userwww_auth))
    )
)