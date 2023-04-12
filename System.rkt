#lang racket

(define (system name)(make-system (cons name null) null null null null
                                  (string-append(number->string(date-day (seconds->date(current-seconds)))) "/"
                                                (number->string(date-month (seconds->date(current-seconds)))) "/"
                                                (number->string(date-year (seconds->date(current-seconds)))) " "
                                                (number->string(date-hour (seconds->date(current-seconds)))) ":"
                                                (number->string(date-minute (seconds->date(current-seconds)))))))

(define (make-system name drive user carpetas papelera fecha) (list name drive user carpetas papelera fecha))

(define get-name car)

(define get-drive cadr)

(define get-user caddr)

(define get-carpetas cadddr)

(define get-papelera fifth)

(define get-fecha sixth)