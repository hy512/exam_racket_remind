#lang racket

(provide
    auth_type_login_pass
    auth_type_user_account
)

; 密码认证, 需要正确密码.
(define auth_type_login_pass "login_pass")
; 账号认证, 输入正确账号就行.
(define auth_type_user_account "user_account")
