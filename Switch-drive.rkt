#lang racket

(define (make-let lett)(string lett))

(define switch-drive(lambda(system)(lambda(letter)
                                    (if(and(not(null?(list-ref(list-ref system 0)1)))(and(member letter(flatten(get-drive system)))
                                                                                         (equal?(length(flatten(list-ref system 0)))2)))
                                       (make-system (flatten(cons(get-name system)
                                                                 (string-append(make-let letter)":/")))
                                                    (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                       (if(not(member letter(get-drive system)))
                                          (make-system(get-name system)(get-drive system)(get-user system)
                                                      (get-carpetas system)(get-papelera system)(get-fecha system))
                                          (if(not(null?(list-ref(list-ref system 0)2)))
                                             (make-system (flatten(cons(cdr(cons(get-name system)
                                                                                (remove(list-ref(list-ref system 0)2)(get-name system))))
                                                                       (string-append(make-let letter)":/")))
                                                          (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                             (make-system(get-name system)(get-drive system)(get-user system)
                                                         (get-carpetas system)(get-papelera system)(get-fecha system))))))))