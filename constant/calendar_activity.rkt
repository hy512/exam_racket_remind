#lang racket

(provide
    calendar_type_lunar
    calendar_type_solar
)

; 阴历
(define calendar_type_lunar "lunar")
; 阳历
(define calendar_type_solar "solar")

; 无需提醒
(define remind_type_not "not")
(define remind_type_email "email")
(define remind_type_message "message")
; (define remind_type_once "once")
; (define remind_type_daliy "daily")
; (define remind_type_weekly "weekly")
; (define remind_type_monthly "monthly")
; (define remind_type_crontab "crontab")
