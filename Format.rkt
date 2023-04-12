#lang racket

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
