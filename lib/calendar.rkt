#lang racket

(require 2htdp/batch-io)
(require racket/runtime-path)
(require racket/date)

(provide
    calender
    calender_lunar_to_solar
    calender_lunar_to_solar_epoch_seconds
)

; 资源目录
(define-runtime-path res_file_dir "calendar")

; 判断年份是否为闰年
(define (is_leap_year year)
    (or 
        (and
            (eq? (modulo year 4) 0)
            (not (eq? (modulo year 100) 0))
        )
        (eq? (modulo year 400) 0)
    )
)

; 有效的 Gregorian 日期 (公历) (阳历)
; return: boolean
(define (is_gregorian year month day)
    (and (>= year 0)
        (>= month 0) (<= month 12)
        (>= day 0)
        (<= day (days_of_month year month))
    )
)

; 月份中的天数
(define (days_of_month year month)
    (define leap_year (is_leap_year year))
    (cond
        [(member month (list 1 3 5 7 8 10 12)) 31]
        [(member month (list 4 6 9 11)) 30]
        ; 闰年
        [(and (equal? month 2) leap_year) 29]
        ; 平常年
        [(and (equal? month 2) (not leap_year)) 28]
        ; 不可达
        [else 0]
    )
)

; 计算参数日期正午时的 julian day
; return: number, 无 "JD" 字符前缀
; https://www.aavso.org/jd-calculator
; 1000-01-01T00:00:00
; 2086302.50000
; 1000-01-01T12:00:00
; 2086303.00000
; https://baike.baidu.com/item/%E5%84%92%E7%95%A5%E6%97%A5/693474?fr=aladdin
(define (julian_day year month day)
    (define a (floor (/ (- 14 month) 12)))
    (define y (- (+ year 4800) a))
    (define m (- (+ month (* 12 a)) 3))
    (- (+ 
            day
            (floor (/ (+ 2 (* 153 m)) 5))
            (* 365 y)
            (floor (/ y 4))
            (floor (/ y 400))
        )
        (floor (/ y 100))
        32045
    )
)

; 计算星期几
; return: 1-7
; wekk: 20000101 为星期六
; julian day: 20000101T120000 为 2451545
; julian day: 20000101T000000 为 2451544.5
(define (day_of_week year month day)
    ; 20000101 星期六, 加到星期天
    ; 调整为星期天的天数
    (define julianday_sunday (+ 2451544.5 1))
    (define julianday (- (julian_day year month day) 0.5))
    (define diff_days (abs (- julianday_sunday julianday)))
    (define mod_week (modulo diff_days 7))
  
    (if (= 0 mod_week)
        7
        (if (>= julianday julianday_sunday)
            mod_week
            (- 7 mod_week)
        )
    )
)

; https://www.zhuxianfei.com/c/28041.html
(define (julian_day2 year month day)
    (define c (if (is_gregorian year month day)
        (-
            (+ 2 (/ year 400))
            (/ year 100)
        )
        0
    ))
    (define y (if (<= month 2) (- year 1) year))
    (define m (if (<= month 2) (+ month 12) month))
    (define d (+ day 0.5000115740))

    (-
        (+
            (floor (+
                (/ 
                    (* 1461 (+ 4716 y))
                    4
                )
                0.01
            ))
            (/
                (* 153 (+ m 1))
                5
            )
            d
            c
        )
        1524.5
    )
)

; list<{
;     year: string
;     month: string
;     day_of_month: string
;     day_of_week: string
; }>
(define (calender_solar year month)
    (define days (days_of_month year month))
    (define list_solar empty)
    (for ([i (range 1 (+ 1 days))])
        (set! list_solar (append list_solar (list (hasheq
            'year year
            'month month
            'day_of_month i
            'day_of_week (day_of_week year month i)
        ))))
    )
    list_solar
)

; 时间戳指示的公历日期意味着农历的相同日期
; 所以先把时间戳的日期提取出来作为农历日期, 在将农历日期转为公历日期
; 再将公历日期转为时间戳
(define (calender_lunar_to_solar_epoch_seconds epoch_seconds)
    (define date_lunar (seconds->date epoch_seconds))
    (define calendar_solar (calender_lunar_to_solar
        (date-year date_lunar)
        (date-month date_lunar)
        (date-day date_lunar)
    ))
    (define date_solar (make-date
        (date-second date_lunar)
        (date-minute date_lunar)
        (date-hour date_lunar)
        (string->number (hash-ref calendar_solar 'day_of_month))
        (string->number (hash-ref calendar_solar 'month))
        (string->number (hash-ref calendar_solar 'year))
        (string->number (hash-ref calendar_solar 'day_of_week))
    ))
    (date->seconds date_solar)
)

; 农历转公历
; * returns: hash<{
;     year: string
;     month: string
;     day_of_month string
;     day_of_week: string
; }>
(define (calender_lunar_to_solar year month day_of_month)
    (define list_calendar (calender_lunar_by_solar year month))
    (define ret_calendar (findf (lambda (calendar)
        (define calendar_lunar (hash-ref calendar 'lunar))
        (and
            (string=? year (hash-ref calendar_lunar 'year))
            (string=? month (hash-ref calendar_lunar 'month))
            (string=? day_of_month (hash-ref calendar_lunar 'day_of_month))
        )
    ) list_calendar))
    
    (if (eq? #f ret_calendar)
        (raise (make-exn:fail:unsupported (format "未被支持的农历 ~a-~a-~a" year month day_of_month)))
        (hash-ref ret_calendar 'solar)
    )
)

; 通过公历年月获取当月农历
; list<{
;     solar: {
;         year: string
;         month: string
;         day_of_month: string
;         day_of_week: string
;     }
;     lunar: {
;         year: string
;         month: string
;         day_of_month: string
;         day_of_week: string
;         solar_term: string
;     }
; }>
(define (calender_lunar_by_solar year [month 0])
    ; 农历数据存放
    (define lunar_file_path (build-path
        res_file_dir
        (format "T~sc.csv" year)
    ))

    (define list_lunar_csv (if (file-exists? lunar_file_path)
        (rest (read-csv-file (path->string lunar_file_path)))
        empty
    ))

    ; 正月位置
    (define index_lunar_january (index-where list_lunar_csv
        (lambda (lunar_csv)
            (string=? (list-ref lunar_csv 1) "正月")
        )
    ))

    ; 格式化数据
    (define list_calender (if (not index_lunar_january)
        empty
        (calender_lunar_csv2list list_lunar_csv year)
    ))

    ; 过滤参数月份数据
    (filter (lambda (date)
        (define date_solar (hash-ref date 'solar))
        (and
            (string=? (hash-ref date_solar 'year) (number->string year))
            (or
                (equal? 0 month)
                (string=? (hash-ref date_solar 'month) (number->string month))
            )
        )
    ) list_calender)
)

; 公曆與農曆日期對照表
; https://www.hko.gov.hk/tc/gts/time/conversion1_text.htm#
; Python 农历公历算法转换
; https://zhuanlan.zhihu.com/p/57261062
; list<{
;     solar: {
;         year: string
;         month: string
;         day_of_month: string
;         day_of_week: string
;     }
;     lunar: {
;         year: string
;         month: string
;         day_of_month: string
;         day_of_week: string
;         solar_term: string
;     }
; }>
(define (calender_lunar_csv2list list_lunar_csv year)
    (define lunar_months (list "正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"))
    (define list_week (list "星期一" "星期二" "星期三" "星期四" "星期五" "星期六" "星期日"))

    ; 正月位置
    ; 要求一定有值, 调用方确定好检查
    (define index_lunar_january (index-where list_lunar_csv
        (lambda (lunar_csv)
            (string=? (list-ref lunar_csv 1) "正月")
        )
    ))

    (define list_calender empty)
    ; 调整格式, 农历数据
    ; 正序把今年年份月份补上
    (define lunar_year year)
    (define lunar_month "")
    (for ([lunar_csv (take-right list_lunar_csv (- (length list_lunar_csv) index_lunar_january))])

        (define lunar_day (list-ref lunar_csv 1))
        (define list_match (regexp-match #rx"^([0-9]+)年([0-9]+)月([0-9]+)日$" (list-ref lunar_csv 0)))

        (set! lunar_year (if (string=? lunar_day "正月") year lunar_year))
        (set! lunar_month (if (member lunar_day lunar_months) lunar_day lunar_month))
        (set! list_calender (append list_calender (list
            (hasheq
                'solar (hasheq
                    'year (list-ref list_match 1)
                    'month (list-ref list_match 2)
                    'day_of_month (list-ref list_match 3)
                    'day_of_week (+ 1 (index-of list_week (list-ref lunar_csv 2)))
                )
                'lunar (hasheq
                    'year lunar_year
                    'month lunar_month
                    'day_of_month lunar_day
                    'day_of_week (list-ref lunar_csv 2)
                    'solar_term (list-ref lunar_csv 3)
                )
            )
        )))
    )
    ; 倒序把去年年份月份补上
    (set! lunar_month "十二月")
    (set! lunar_year (- year 1))
    (define index_month (index-of lunar_months lunar_month))
    (for ([lunar_csv (reverse (take list_lunar_csv index_lunar_january))])

        (define lunar_day (list-ref lunar_csv 1))
        (define list_match (regexp-match #rx"^([0-9]+)年([0-9]+)月([0-9]+)日$" (list-ref lunar_csv 0)))

        (set! list_calender (append (list
            (hasheq
                'solar (hasheq
                    'year (list-ref list_match 1)
                    'month (list-ref list_match 2)
                    'day_of_month (list-ref list_match 3)
                    'day_of_week (+ 1 (index-of list_week (list-ref lunar_csv 2)))
                )
                'lunar (hasheq
                    'year lunar_year
                    'month lunar_month
                    'day_of_month lunar_day
                    'day_of_week (list-ref lunar_csv 2)
                    'solar_term (list-ref lunar_csv 3)
                )
            )
        ) list_calender))
        ; 定位到上一月, 返序找到了本月一号
        (set! index_month (if (member lunar_day lunar_months) (- index_month 1) index_month))
        (set! lunar_month (list-ref lunar_months index_month))
    )

    list_calender
)

; 查询日历
; * year: number
; * month: number
; * returns
; list<{
;     solar: {
;         year: string
;         month: string
;         day_of_month: string
;         day_of_week: string
;     }
;     lunar?: {
;         year: string
;         month: string
;         day_of_month: string
;         day_of_week: string
;         solar_term: string
;     }
; }>
(define (calender year month)
    (define list_date (calender_lunar_by_solar year month))
    ; 阴历无数据, 则计算阳历
    (if (empty? list_date)
        (map (lambda (date_solar)
            (hasheq 'solar date_solar)
        ) (calender_solar year month))
        list_date
    )
)
