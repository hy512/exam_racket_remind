#lang racket

; (require "../module/remind/calendar_activity.rkt")

(provide
    schedule_remind
)

(define schedule_remind (list
    ; (hasheq
    ;     'title "日历提醒定时"
    ;     ; 'cron "0,10,20,30,40,50 * * * * *"
    ;     'cron "0-59/5 * * * * *"
    ;     'handler calendar_activity_handler
    ;     'attrs (hasheq
    ;         ; 加载到内存的数量
    ;         'sche_size 30
    ;         ; 百分数, 每个 cron 生成总数 sche_size 的多少条记录, 实现单个 cron 连续执行
    ;         'sche_factor 5
    ;     )
    ; )
))
