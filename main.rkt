#lang racket

(require "service/http.rkt")
(require "lib/config.rkt")
(require "hook/hook.rkt")

(define (main)

    ; 设置 home 为当前目录
    (config_value_save g_cfg_ctx "home"
        (path->string (path-only (path->complete-path (find-system-path 'run-file))))
    )

    ; 加载配置文件
    (config_load_file_json g_cfg_ctx
        (path->string (build-path (config_value g_cfg_ctx "home") "config.json"))
    )

    ; http 启动
    (http_start (hasheq
        'on_start func_service_start
        'on_stop func_service_stop
    ))
)

(main)
