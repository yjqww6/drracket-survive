#lang racket
(require drracket/tool framework)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase2 void)

    (define table (make-hasheq))
    (define lock (make-semaphore 1))
    (define-syntax-rule (define-locked (id . fmls) body ...)
      (namespace-set-variable-value!
       'id
       (λ fmls
         (call-with-semaphore lock (λ () body ...)))))

    (define (rep-mixin %)
      (class %
        (super-new)

        (define/override (evaluate-from-port port complete-program? cleanup)
          (super evaluate-from-port port complete-program?
                 (if complete-program?
                     (λ ()
                       (cleanup)
                       (define-locked ($set! k v) (hash-set! table k v))
                       (define-locked ($get k) (hash-ref table k))
                       (define-locked ($clear!) (hash-clear! table))
                       (define-locked ($del! k) (hash-remove! table k)))
                     cleanup)))))

    (drracket:get/extend:extend-interactions-text rep-mixin)

    (define phase1 void)
    ))
