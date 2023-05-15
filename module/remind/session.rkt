#lang racket

(require data/maybe)
(require uuid)

(require "../../dao/dao.rkt")
(require "../../lib/logger.rkt")
(require "../../lib/http.rkt")
(require "../../lib/type.rkt")
(require "../../lib/exception.rkt")
(require "../../dao/userwww/userwww.rkt")
(require "../../dao/userwww/userwww_auth.rkt")
(require "../../constant/database.rkt")
(require "../../constant/userwww_auth.rkt")
(require "../common.rkt")

(provide session_create)


(define (session_create req)
    (request_handler_wrapper req session_create_inner
        #:before (request_handler_chain
            remind_request_handler_before_chain
            (request_before_validator_body
                (cons 'auth_type string?)
                (cons 'user_account string?)
                (cons 'auth_type string_not_blank)
                (cons 'user_account string_not_blank)
            )
        )
        #:after remind_request_handler_after_chain
    )

)

(define (session_create_inner req_ctx)
    (define db_ctx (request_context_attribute req_ctx "db_ctx"))
    (define body_data (struct_request_context-body_json req_ctx))
    (define userwww_maybe (find_userwww_by_account db_ctx (hash-ref body_data 'user_account)))
    (define auth_type (hash-ref body_data 'auth_type))

    (log_debug logger_module_session (format "~a ~a" userwww_maybe (nothing? userwww_maybe)))

    (if (nothing? userwww_maybe)
        ; (result_failure "认证失败")
        ; 没有时进行新增
        (session_create_new_userwww req_ctx)
        (session_create_auth req_ctx (from-just! userwww_maybe) auth_type)
    )

    (define userwww (request_context_attribute req_ctx "userwww"))
    (define userwww_auth (request_context_attribute req_ctx "userwww_auth"))

    ; 登记认证记录

    ; 创建会话信息
    (session_set req_ctx "userwww" userwww)
    (session_set req_ctx "userwww_auth" userwww_auth)

    ; 返回数据
    (define session_id (request_context_attribute req_ctx "session_id"))
    (result_success (hasheq
        'session_id session_id
        'user_name (hash-ref userwww 'user_name)
    ))
)

; 进行认证
(define (session_create_auth req_ctx userwww auth_type)

    (define userwww_auth (cond
        [(string=? auth_type auth_type_user_account) (session_create_auth_user_account req_ctx userwww)]
        [else (raise_routine_exception (format "认证类型 '~a' 错误" auth_type))]
    ))

    (request_context_attribute_save req_ctx "userwww" userwww)
    (request_context_attribute_save req_ctx "userwww_auth" userwww_auth)
)

; 账号认证, 认证通过时返回使用的认证记录.
; @returns hash<userwww_auth>
(define (session_create_auth_user_account req_ctx userwww)
    (define dao_ctx (request_context_attribute req_ctx "db_ctx"))

    (define userwww_auth_maybe (find_userwww_by_user_uuid_auth_type_not_deleted dao_ctx (hash-ref userwww 'user_uuid) auth_type_user_account))
    (if (nothing? userwww_auth_maybe)
        (raise_routine_exception "认证失败")
        (from-just! userwww_auth_maybe)
    )
)

; 新接入, 新增用户并进行认证
; @returns, hash<userwww>
(define (session_create_new_userwww req_ctx)
    (define dao_ctx (request_context_attribute req_ctx "db_ctx"))
    (define request_time (request_context_attribute req_ctx "request_time"))
    (define body_data (struct_request_context-body_json req_ctx))
    (define user_uuid (uuid-string))
    (define user_account (hash-ref body_data 'user_account))

    (sql_tran dao_ctx (lambda (dao_ctx)
        (log_debug logger_module_session (format "新增用户 ~a" user_account))

        ; 新增用户信息
        (save_userwww dao_ctx (hasheq 'user_uuid user_uuid
            'user_account (hash-ref body_data 'user_account)
            'user_name "新用户"
            'last_login_time request_time
            'create_time request_time
            'update_time request_time
            'delete_time 0
            'deleted database_boolean_false
        ))
        ; 查找录入信息
        (define userwww_maybe (find_userwww_by_uuid dao_ctx user_uuid))
        (define userwww (from-just! userwww_maybe))
        ; 新增认证信息
        (save_userwww_auth dao_ctx (hasheq
            'user_id (hash-ref userwww 'id)
            'user_uuid (hash-ref userwww 'user_uuid)
            'auth_type auth_type_user_account
            'login_pass (hash_value_or_else body_data 'login_pass "")
            'create_time request_time
            'update_time request_time
            'delete_time 0
            'deleted database_boolean_false
        ))
        ; 查找录入的认证
        (define userwww_auth_maybe (find_userwww_by_user_uuid_auth_type_not_deleted dao_ctx
            (hash-ref userwww 'user_uuid)
            auth_type_user_account))
        (define userwww_auth (from-just! userwww_auth_maybe))

        (request_context_attribute_save req_ctx "userwww" userwww)
        (request_context_attribute_save req_ctx "userwww_auth" userwww_auth)
    ) (lambda (dao_ctx err)
        ; (log_info logger_module_session (format "~a" err))
        (request_context_attribute_save req_ctx "error" err)
    ))

    ; 抛出异常或返回用户信息
    (if (eq? 'null (request_context_attribute req_ctx "error"))
        (request_context_attribute req_ctx "userwww")
        (raise (request_context_attribute req_ctx "error"))
    )
)

; (define (session_create_by_account req)
; )
