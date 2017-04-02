;; Standard macros

(define-syntax let
  (syntax-rules ()
    ((let ((var expr) ...) body ...)
     ((lambda (var ...) body ...) expr ...))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and t1 t2 ...)
     (if t1 (and t2 ...) #f))))

;; Call this as `(bind 3 => (lambda(x) (* x x)))`
(define-syntax bind
  (syntax-rules (=>)
    ((bind a => b) (if a (b a) #f))))

(define-syntax delay
  (syntax-rules ()
    ((wrap x) (lambda () x))))

(define-syntax force
  (syntax-rules ()
    ((force x) (x))))
