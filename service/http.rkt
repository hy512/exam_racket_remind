#lang racket

(require
    web-server/servlet
    web-server/servlet-env)
(require web-server/dispatch)
(require web-server/http)

(require "../module/module.rkt")
(require "../lib/config.rkt")
(require "../lib/logger.rkt")

(provide http_start)

(define (start req)
    (current-logger logger_service_http)
    (log-debug (url->string (request-uri req)))

    ; (define rsp (call-with-exception-handler
    ;     (lambda (err) (println err))
    ;     (lambda ()
    ;         (calendar-dispatch req))))
    ; (println rsp)
    ; rsp
    (module-dispatch req)
)

; 开启 http 服务, 将阻塞当前线程
(define (http_start args)

    ((hash-ref args 'on_start))
    (current-logger logger_service_http)

    (define args_http (config_value g_cfg_ctx "http"))

    (log-info (format "应用启动 ~a:~a"
        (hash-ref args_http 'host)
        (hash-ref args_http 'port)
    ))

    (serve/servlet start
        #:listen-ip (hash-ref args_http 'host)
        #:port (hash-ref args_http 'port)
        #:launch-browser? #f
        #:servlet-regexp #rx"")
    ((hash-ref args 'on_stop))
)
