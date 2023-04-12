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

;---------------------------------------------------------------------------------------------------

(define (run system o) (o system))

;---------------------------------------------------------------------------------------------------

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

;--------------------------------------------------------------------------------------------------

(define (make-user use) (list use))

(define register (lambda(system)
                   (lambda(use)
                     (if(null? (list-ref system 2))
                        (make-system(get-name system)(get-drive system)(flatten(cons(get-user system)(make-user use)))
                                    (get-carpetas system)(get-papelera system)(get-fecha system))
                        (if (member use (flatten(list-ref system 2)))
                            (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                            (make-system(get-name system)(get-drive system)
                                        (flatten(cons(get-user system)(make-user use)))(get-carpetas system)(get-papelera system)(get-fecha system)))))))

;--------------------------------------------------------------------------------------------------

(define login (lambda(system)(lambda(userN)
                            (if(not(null?(cdr(list-ref system 0))))
                               (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                               (if(and(member userN (get-user system))(not(member userN (get-name system))))
                                  (make-system(flatten(cons(get-name system)(make-user userN)))(get-drive system)
                                              (get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                  (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))))))

;--------------------------------------------------------------------------------------------------

(define logout (lambda (system)
                 (if(equal? (length(get-name system)) 3)
                    (make-system(cons (list-ref(list-ref system 0)0)null)
                                (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                    (if(equal? (length(get-name system)) 2)
                       (make-system(cons (list-ref(list-ref system 0)0)null)
                                   (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))

;--------------------------------------------------------------------------------------------------

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

;--------------------------------------------------------------------------------------------------

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

;--------------------------------------------------------------------------------------------------

(define cd (lambda(system)
             (lambda(d)
               (if (equal? "/" d)
                   (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                               (get-name system))))
                                            (string-append(string-join
                                                           (take(string-split(list-ref(list-ref system 0)2)"/")1)"/")"/")))
                               (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                   (if (member d (flatten(list-ref system 3)))
                       (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                 (get-name system))))d))
                                   (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       (if(member (string-append(list-ref(list-ref system 0)2)d"/") (flatten(list-ref system 3)))
                          (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                    (get-name system))))
                                                   (string-append(list-ref(list-ref system 0)2)d"/")))
                                      (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                          (if(equal? ".." d)
                             (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                       (get-name system))))
                                                      (string-append(string-join
                                                                     (take(string-split(list-ref(list-ref system 0)2)"/")
                                                                          (- (length(string-split(list-ref(list-ref system 0)2)"/"))1))"/")"/")))
                                         (get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                             (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))))))))

;--------------------------------------------------------------------------------------------------

(define (remplazar lista posicion nombre-nuevo)
  (map (lambda (elemento i)
         (if (= i posicion) nombre-nuevo elemento))
       lista
       (range 0 (length lista))))

(define format(lambda (system)
                (lambda (letter nombre)
                  (if (member letter (get-drive system))
                      (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))
                                               (string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")1)"/")"/")))
                                  (remplazar(flatten(list-ref system 1))(+(index-of (flatten(list-ref system 1))
                                                                                    (findf (lambda (x) (equal? x letter))
                                                                                           (flatten(list-ref system 1))))1) nombre)
                                  (get-user system)(filter (lambda (sublista) (not (member letter sublista)))(list-ref system 3))
                                  (get-papelera system)(get-fecha system))
                      (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))

;--------------------------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------------------------

(define del (lambda (system)
              (lambda (conte)
                (if(equal? conte "*.*")
                   (make-system(get-name system)(get-drive system)(get-user system)
                               (filter (lambda (x) (< (length x) 6)) (list-ref system 3))
                               (filter (lambda (x) (>= (length x) 6)) (list-ref system 3))(get-fecha system))
                   (if (member conte (flatten (list-ref system 3)))
                       (make-system(get-name system)(get-drive system)(get-user system)
                                   (foldr (lambda (e acc)
                                            (if (and (list? e) (member conte (flatten e)))
                                                acc(cons e acc) ))'()(get-carpetas system))
                                   (foldr (lambda (e acc)(if (and (list? e) (member conte (flatten e)))(cons e acc)acc))
                                          '()(get-carpetas system))(get-fecha system))
                       (if(and(equal? (car(string-split conte ".")) "*")(member (last (string-split conte "."))
                                                                                (flatten(list-ref system 3))))
                          (make-system(get-name system)(get-drive system)(get-user system)
                                      (filter (lambda (sublst)
                                                (not (ormap
                                                      (lambda (x)(and (string? x)(regexp-match?
                                                                                  (regexp(string-append "."conte"$"))
                                                                                  x))) sublst)))
                                              (get-carpetas system))(filter (lambda (x)(not (member x(filter
                                                                        (lambda(sublst)(not (ormap (lambda (x)
                                                         (and (string? x)(regexp-match?(regexp (string-append "."conte"$"))
                                                                                       x))) sublst)))(get-carpetas system)))))
                                                                            (list-ref system 3))(get-fecha system))
                          (if(member (last (string-split conte "."))(flatten(list-ref system 3)))
                             (make-system(get-name system)(get-drive system)(get-user system)
                                         (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)
                                         (regexp-match? (regexp (last (string-split conte "*"))) (car(caddr sublst)))
                                         (regexp-match? (regexp (first (string-split conte "*"))) (car(caddr sublst)))))
                                                                            sublst)))(list-ref system 3))
                                         (filter (lambda (x) (not (member x  (filter (lambda (sublst)
                                                            (not(ormap (lambda (x)(and (string? x)
                                         (regexp-match? (regexp (last (string-split conte "*"))) (car(caddr sublst)))
                                         (regexp-match? (regexp (first (string-split conte "*"))) (car(caddr sublst)))))
                                                                       sublst)))(list-ref system 3)))))(list-ref system 3))
                                         (get-fecha system))
                             (if(and(lambda (x)  (string-contains? (second x) conte))(string-contains?
                                                                                      (third (list-ref system 0)) conte))
                                (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)
                                                                                          (get-name system))))
                                                         (string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")
                                                                                        (- (length(string-split(list-ref(list-ref system 0)2)"/"))1))"/")"/")))
                                            (get-drive system)(get-user system)(filter (lambda (x) (not (string-contains? (second x) conte)))
                                                                                       (list-ref system 3))
                                            (filter (lambda (x) (not (member x  (filter (lambda (x) (not (string-contains? (second x) conte)))
                                                                                        (list-ref system 3)))))(list-ref system 3))
                                            (get-fecha system))
                                (if(lambda (x)  (string-contains? (second x) conte))
                                   (make-system(get-name system)(get-drive system)(get-user system)
                                               (filter (lambda (x) (not (string-contains? (second x) conte))) (list-ref system 3))
                                               (filter (lambda (x) (not (member x  (filter (lambda (x) (not (string-contains?
                                                                                                             (second x) conte)))
                                                                                           (list-ref system 3)))))(list-ref system 3))
                                               (get-fecha system))
                                   (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)
                                               (get-fecha system)))))))))))

;----------------------------------------------------------------------------------------------------