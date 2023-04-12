#lang racket

(define (system name)(make-system (cons name null) null null null))

(define (make-system name drive user carpetas) (list name drive user carpetas))

(define get-name car)

(define get-drive cadr)

(define get-user caddr)

(define get-carpetas cadddr)

;---------------------------------------------------------------------------------------------------

(define (run system o) (o system))

;---------------------------------------------------------------------------------------------------

(define (make-drive letra nombre capacidad)(list letra nombre capacidad))

(define add-drive(lambda(system)
                   (lambda(letra nombre capacidad)
                     (if(null? (list-ref system 1))
                        (make-system (get-name system)(flatten(cons(make-drive letra nombre capacidad)(get-drive system)))
                                     (get-user system)(get-carpetas system))
                        (if(member letra (flatten(list-ref system 1)))
                           (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                           (make-system (get-name system)(flatten(cons(get-drive system)(make-drive letra nombre capacidad)))
                                        (get-user system)(get-carpetas system)))))))

;--------------------------------------------------------------------------------------------------

(define (make-user use) (list use))

(define add-user (lambda(system)
                   (lambda(use)
                     (if(null? (list-ref system 2))
                        (make-system(get-name system)(get-drive system)(flatten(cons(get-user system)(make-user use)))
                                    (get-carpetas system))
                        (if (member use (flatten(list-ref system 2)))
                            (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                            (make-system(get-name system)(get-drive system)
                                        (flatten(cons(get-user system)(make-user use)))(get-carpetas system)))))))

;--------------------------------------------------------------------------------------------------

(define login (lambda(system)
                (lambda(userN)
                  (if(not(null?(cdr(list-ref system 0))))
                     (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                     (if(and(member userN (get-user system))(not(member userN (get-name system))))
                        (make-system(flatten(cons(get-name system)(make-user userN)))(get-drive system)
                                    (get-user system)(get-carpetas system))
                        (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)))))))

;--------------------------------------------------------------------------------------------------

(define logout (lambda (system)
                 (if(null?(cdr(get-name system)))
                    (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                    (make-system(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)1)(get-name system))))
                                (get-drive system)(get-user system)(get-carpetas system)))))

;--------------------------------------------------------------------------------------------------

(define (make-let lett)(string lett))

(define switch-drive(lambda(system)
                      (lambda(letter)
                        (if(and(not(null?(list-ref(list-ref system 0)1)))
                               (and(member letter(flatten(get-drive system)))(equal?(length(flatten(list-ref system 0)))2)))
                           (make-system (flatten(cons(get-name system)(string-append(make-let letter)":/")))(get-drive system)
                                        (get-user system)(get-carpetas system))
                           (if(not(member letter(get-drive system)))
                              (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                              (if(not(null?(list-ref(list-ref system 0)2)))
                                 (make-system (flatten(cons(cdr(cons(get-name system)
                                                                    (remove(list-ref(list-ref system 0)2)(get-name system))))
                                                           (string-append(make-let letter)":/")))
                                              (get-drive system)(get-user system)(get-carpetas system))
                                 (make-system(get-name system)(get-drive system)(get-user system)
                                             (get-carpetas system))))))))

;--------------------------------------------------------------------------------------------------

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

;--------------------------------------------------------------------------------------------------

(define cd (lambda(system)
             (lambda(d)
               (if (equal? "/" d)
                   (make-system(flatten(cons(cdr(cons(get-name system)
                                                     (remove(list-ref(list-ref system 0)2)(get-name system))))
                                            (string-append(string-join
                                                           (take(string-split(list-ref(list-ref system 0)2)"/")1)"/")"/")))
                               (get-drive system)(get-user system)(get-carpetas system))
                   (if (member d (flatten(list-ref system 3)))
                       (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                 (get-name system)))) d))
                                   (get-drive system)(get-user system)(get-carpetas system))
                       (if(member (string-append(list-ref(list-ref system 0)2)d"/") (flatten(list-ref system 3)))
                          (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                    (get-name system))))
                                                   (string-append(list-ref(list-ref system 0)2)d"/")))
                                      (get-drive system)(get-user system)(get-carpetas system))
                          (if(equal? ".." d)
                             (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                       (get-name system))))
                                                      (string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")
                                                                          (- (length(string-split(list-ref(list-ref system 0)2)"/"))1))"/")"/")))
                                         (get-drive system)(get-user system)(get-carpetas system))
                             (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)))))))))

;--------------------------------------------------------------------------------------------------
