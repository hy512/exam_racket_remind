#lang racket

(require data/maybe)
(require db)

(require "../dao.rkt")
(require "../helper.rkt")
(require "../../lib/type.rkt")
(require "../../lib/logger.rkt")

(provide
    find_calendar_activity_by_page
    find_calendar_activity_by_ids
    find_calendar_activity_by_user_id_orderby_id_desc
    save_calendar_activity
)

(define columns_list (list "id"
    "create_time" "update_time" "delete_time" "deleted"
    "user_id" "user_uuid"
    "title" "content" "remind_type" "remind_cron" "activity_begin_date" "activity_end_date"
    "calendar_type"))
(define columns_select (apply string_join ", " columns_list))
(define columns_insert (apply string_join ", " (cdr columns_list)))


(define (find_calendar_activity_by_page dao_ctx page_size page_number)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "select ~a from rem_calendar_activity where deleted = 0 order by id desc limit ? offset ?" columns_select))
    (define pair_offset (offset page_size page_number))
    (define list_args (list (car pair_offset) (cdr pair_offset)))

    (log_info logger_dao_calendar_activity str_sql)
    (log_info logger_dao_calendar_activity list_args)

    (define prep_sql (prepare conn str_sql))
    (define list_rows (query-rows conn
        (bind-prepared-statement prep_sql list_args)
    ))

    (result_map columns_list list_rows)
)

(define (find_calendar_activity_by_ids dao_ctx list_id)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "select ~a from rem_calendar_activity where deleted = 0 and id in (~a) order by id desc"
        columns_select
        (string-join (map (λ (int_id) "?") list_id) ", ")
    ))
    (define list_args list_id)

    (log_info logger_dao_calendar_activity str_sql)
    (log_info logger_dao_calendar_activity list_args)

    (define prep_sql (prepare conn str_sql))
    (define list_rows (query-rows conn
        (bind-prepared-statement prep_sql list_args)
    ))

    (result_map columns_list list_rows)
)

(define (find_calendar_activity_by_user_id_orderby_id_desc dao_ctx page_size page_number user_id user_uuid)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "select ~a from rem_calendar_activity where deleted = 0 AND user_id = ? AND user_uuid = ? order by id desc limit ? offset ?" columns_select))
    (define pair_offset (offset page_size page_number))
    (define list_args (list user_id user_uuid (car pair_offset) (cdr pair_offset)))

    (log_info logger_dao_calendar_activity str_sql)
    (log_info logger_dao_calendar_activity list_args)

    (define prep_sql (prepare conn str_sql))
    (define list_rows (query-rows conn
        (bind-prepared-statement prep_sql list_args)
    ))

    (result_map columns_list list_rows)
)

; returns: void
(define (save_calendar_activity dao_ctx hash_calendar_activity)
    (define conn (struct_dao_context-connection dao_ctx))

    (define str_sql (format "INSERT INTO rem_calendar_activity (~a) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" columns_insert))
    (define list_args (result_map_r hash_calendar_activity (cdr columns_list)))

    (log_info logger_dao_userwww_auth str_sql)
    (log_info logger_dao_userwww_auth (format "~a" list_args))

    (define prep_sql (prepare conn str_sql))
    (query-exec conn (bind-prepared-statement prep_sql list_args))
)

