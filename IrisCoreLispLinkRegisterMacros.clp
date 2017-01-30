;------------------------------------------------------------------------------
; Link Register macro operations 
;------------------------------------------------------------------------------
(defrule lower::pop-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents pop
                                 lr
                                 ?stack)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                pop
                                iv0
                                ?stack)
                       (mk-move-op ?n
                                   lr
                                   iv0)))

(defrule lower::push-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents push
                                 ?stack
                                 lr)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-move-op ?n
                                   iv0
                                   lr)
                       (mk-list ?n
                                push 
                                ?stack 
                                iv0)))

(defrule lower::store-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents st
                                 ?address
                                 lr)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-move-op ?n
                                   iv0
                                   lr)
                       (mk-list ?n
                                st
                                ?address
                                iv0)))

(defrule lower::load-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents ld
                                 lr
                                 ?address)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                ld
                                iv0
                                ?address)
                       (mk-move-op ?n
                                   lr
                                   iv0)))

(defrule lower::make-swap-lr-register-lr-first
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents swap
                                 lr
                                 ?register)
                       (name ?name)
                       (parent ?p))
         (object (is-a register)
                 (name ?register))
         =>
         (unmake-instance ?f)
         (mk-container ?name
                       ?p
                       (mk-move-op ?name
                                   iv0
                                   lr)
                       (mk-list ?name
                                swap
                                iv0
                                ?register)
                       (mk-move-op ?name
                                   lr
                                   iv0)))

(defrule lower::make-swap-lr-register-lr-second
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (contents swap
                                 ?register
                                 lr)
                       (name ?name)
                       (parent ?p))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f 
                          (contents swap
                                    lr
                                    ?register)))

