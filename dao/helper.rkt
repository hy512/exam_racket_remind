#lang racket

(provide
    offset
)

; 分页偏移调整
(define (offset page_size page_number)
    ; page_number 减去 1, 通常基于 1 开始.
    (set! page_number (if (number? page_number)
        (- page_number 1)
        0
    ))

    ; 错误值
    (set! page_number (if (integer? page_number)
        page_number
        0
    ))
    (set! page_size (if (integer? page_size)
        page_size
        15
    ))
    ; 最小值
    (set! page_number (if (< page_number 0)
        0
        page_number
    ))
    (set page_size (if (< page_size 1)
        page_size
        1
    ))
    ; 最大值
    (set! page_number (if (> page_number 9999999)
        9999999
        page_number
    ))
    (set! page_size (if (> page_size 9999)
        9999
        page_size
    ))
    (cons page_size (* page_number page_size))
)
