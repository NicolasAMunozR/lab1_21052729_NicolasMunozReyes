#lang racket

(define file(lambda (nombre tipo contenido)(list nombre tipo contenido )))

(define add-file(lambda (system)
                  (lambda (datos)
                    (make-system(get-name system)(get-drive system)(get-user system)
                                (append (list-ref system 3)
                                        (list (append(list (first (string->list (list-ref(list-ref system 0)2)))
                                                           (string-append(list-ref(list-ref system 0)2) (list-ref datos 0)))
                                                     (list datos (string-append
                                                                  (number->string(date-day (seconds->date(current-seconds)))) "/"
                                                                  (number->string(date-month (seconds->date(current-seconds)))) "/"
                                                                  (number->string(date-year (seconds->date(current-seconds)))) " "
                                                                  (number->string(date-hour (seconds->date(current-seconds)))) ":"
                                                                  (number->string(date-minute (seconds->date(current-seconds)))))
                                                           (list-ref(list-ref system 0)1)null))))(get-papelera system)(get-fecha system)))))
