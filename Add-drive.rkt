#lang racket

(define (make-drive letra nombre capacidad)(list letra nombre capacidad))
(define add-drive (lambda(system)
                  (lambda(letra nombre capacidad)
                    (if(null? (list-ref system 1))
                       (make-system (get-name system)(flatten(cons(make-drive letra nombre capacidad)(get-drive system)))
                                    (get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       (if(member letra (flatten(list-ref system 1)))
                          (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                          (make-system (get-name system)(flatten(cons(get-drive system)(make-drive letra nombre capacidad)))
                                       (get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))))))
