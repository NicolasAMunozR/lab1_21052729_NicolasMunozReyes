#lang racket

(define md (lambda(system)
             (lambda(nam)
               (if(null? (list-ref system 3))
                  (make-system(get-name system)(get-drive system)(get-user system)
                              (flatten(cons(get-carpetas system)
                                           (string-append(list-ref(list-ref system 0)2)nam"/"))))
                  (if(member(string-append(list-ref(list-ref system 0)2)nam"/")
                            (flatten(list-ref system 3)))
                     (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                     (make-system(get-name system)(get-drive system)(get-user system)
                                 (flatten(cons(get-carpetas system)
                                              (string-append(list-ref(list-ref system 0)2)nam"/")))))))))