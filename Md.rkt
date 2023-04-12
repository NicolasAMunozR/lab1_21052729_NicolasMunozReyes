#lang racket

(define md (lambda(system)
             (lambda(nam)
               (if(null? (list-ref system 3))
                  (make-system(get-name system)(get-drive system)(get-user system)
                              (list(append (list (first (string->list (list-ref(list-ref system 0)2)))
                                                 (string-append(list-ref(list-ref system 0)2)nam"/") (list "null"))
                                           (list (string-append(number->string(date-day (seconds->date(current-seconds)))) "/"
                                                               (number->string(date-month (seconds->date(current-seconds)))) "/"
                                                               (number->string(date-year (seconds->date(current-seconds)))) " "
                                                               (number->string(date-hour (seconds->date(current-seconds)))) ":"
                                                               (number->string(date-minute (seconds->date(current-seconds)))))
                                                 (list-ref(list-ref system 0)1))))(get-papelera system)(get-fecha system))
                  (if(member(string-append(list-ref(list-ref system 0)2)nam"/")(flatten(list-ref system 3)))
                     (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                     (make-system(get-name system)(get-drive system)(get-user system)
                                 (append (list-ref system 3)(list (append(list (first (string->list (list-ref(list-ref system 0)2)))
                                                                               (string-append(list-ref(list-ref system 0)2)nam"/") (list "null"))
                                                                         (list (string-append(number->string(date-day (seconds->date(current-seconds)))) "/"
                                                                                             (number->string(date-month (seconds->date(current-seconds)))) "/"
                                                                                             (number->string(date-year (seconds->date(current-seconds)))) " "
                                                                                             (number->string(date-hour (seconds->date(current-seconds)))) ":"
                                                                                             (number->string(date-minute (seconds->date(current-seconds)))))
                                                                               (list-ref(list-ref system 0)1)))))(get-papelera system)(get-fecha system)))))))
