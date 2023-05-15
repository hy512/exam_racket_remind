#lang racket

(require web-server/dispatch)

(provide module-dispatch module-url)

(require "common/resp.rkt")
(require "common/calendar.rkt")
(require "remind/session.rkt")
; (require "remind/calendar_activity.rkt")


(define-values (module-dispatch module-url)
    (dispatch-rules
        [("calendar" "list") #:method "get" calendar-list]
        [("session" "create") #:method "post" session_create]
        ; [("calendar_activity" "list") #:method "get" calendar_activity_list]
        ; [("calendar_activity" "save") #:method "post" calendar_activity_save]
        [else http_status_forbidden]
    )
)
