#lang racket

(define logout (lambda (system)
                 (if(equal? (length(get-name system)) 3)
                    (make-system(cons (list-ref(list-ref system 0)0)null)
                                (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                    (if(equal? (length(get-name system)) 2)
                       (make-system(cons (list-ref(list-ref system 0)0)null)
                                   (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))

