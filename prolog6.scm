;; begin prolog-6a.scm

;; utility functions
(define (first x) (car x))
(define (rest x) (cdr x))

(define (AppendInefficient list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (AppendInefficient (cdr list1) list2))))

(define (AppendInefficient3 list1 list2 list3)
  (AppendInefficient list1 (AppendInefficient list2 list3)))

(define result_ '())
(define (clear_result) (set! result_ '()))
(define (append_to_result lis) (set! result_ (cons lis result_)))
(define (get_result) result_)
(define (display_result) (display (get_result)))

;;;;

(define link list)
(define L_l car)
(define L_g cadr)
(define L_r caddr)
(define L_e cadddr)
(define (L_n x) (car (cddddr x)))


(define (L_c x) (cadr (cddddr x)))


(define (clear_r x)
  (set-car! (cddr x) '(())))


(define (back6 l g r e n c whole-db)
  (cond
    ((and (pair? g)
          (pair? r))
      (prove6 l g (cdr r) e n c whole-db))
    ((pair? l)
      (prove6 (L_l l)
              (L_g l)
              (cdr (L_r l))
              (L_e l)
              (L_n l)
              (L_c l)
	      whole-db))))


;; g == goals
;; r == rules (factbase)
(define (prove6 l g r e n c whole-db)
  
;;   (newline) (display "prove6") (newline)
;; ;  (display "l = ") (display l) (newline)
;;   (display "g = ") (display g) (newline)
;;   (display "r = ") (display r) (newline)
;;   (display "e = ") (display e) (newline)
;; ;  (display "n = ") (display n) (newline)
;; ;  (display "c = ") (display c) (newline)
;; ;  (display "w = ") (display whole-db) (newline)
;;   (newline)
  
  (cond
    ((null? g)
      (let ((next_result (print-frame e)))
	(append_to_result next_result))
      (back6 l g r e n c whole-db))
    ((eq? '! (car g))
      (clear_r c)
      (prove6 c (cdr g) r e n c whole-db))
    ((eq? 'r! (car g))
      (prove6 l (cddr g) r e n (cadr g) whole-db))
    ((null? r)
      (if (null? l)
          #t
          (back6 l g r e n c whole-db)))
    ((foreign? (car g))
     (call-foreign (car g) e)
     (prove6 l (cdr g) r e n c whole-db))
    ((foreign? (car r))
     (call-foreign (car r) e)
     (prove6 l g (cdr r) e n c whole-db))
    (else
      (let ((a  (copy (car r) n)))
        (let ((e* (unify (car a) (car g) e)))
          (if e*
              (prove6 (link l g r e n c)
                      (AppendInefficient3 (cdr a) `(r! ,l) (cdr g))
                      whole-db
                      e*
                      (+ 1 n)
                      l
		      whole-db)
              (back6 l g r e n c whole-db))))
      )))

(define empty '((bottom)))

;(define var '?) ; removed for transpilation
(define name cadr)
(define time cddr)

(define (var? x)
  (and (pair? x)
       (string? (car x))
       (string=? "?" (car x))))

;; manually rewritten named let
(define (lookup_loop e id tm)
    (cond ((not (pair? (caar e)))
	   #f)
	  ((and (eq? id (name (caar e)))
		(eqv? tm (time (caar e))))
	   (car e))
	  (else
	   (lookup_loop (cdr e) id tm))))

(define (lookup v e)
    (let ((id (name v))
          (tm  (time v)))
      (lookup_loop e id tm)))
;;; end rewrite

(define (value x e)
  (cond ((foreign? x)
	 (call-foreign x e))
	((var? x)
	 (let ((v (lookup x e)))
           (if v
               (value (cadr v) e)
               x)))
	(else x)))

(define (copy x n)
  (cond
    ((not (pair? x)) x)
    ((var? x) (AppendInefficient x n))
    (else
      (cons (copy (car x) n)
            (copy (cdr x) n)))))

(define (bind x y e)
  (cons (list x y) e))

(define (unify x1 y1 e)
  (let ((x (value x1 e))
        (y (value y1 e)))
    (cond
      ((eq? x y) e)
      ((var? x) (bind x y e))
      ((var? y) (bind y x e))
      ((or (not (pair? x))
           (not (pair? y))) #f)
      (else
        (let ((e* (unify (car x) (car y) e)))
          (and e* (unify (cdr x) (cdr y) e*)))))))


(define (resolve x e)
  (cond ((not (pair? x)) x)
        ((var? x)
          (let ((v (value x e)))
            (if (var? v)
                v
                (resolve v e))))
        (else
          (cons
            (resolve (car x) e)
            (resolve (cdr x) e)))))

(define (has_bindings_Q_ ee)
  (pair? (cdr ee)))

(define (get_var_name_from_binding ee)
  (cadaar ee))

(define (get_binding_value_from_binding ee e)
  (resolve (caar ee) e))

(define (no_timestamp_binding_Q_ ee)
  (null? (time (caar ee))))

(define (get_rest_of_bindings ee)
  (cdr ee))

(define (print_frame_helper ee all_bindings accumulator)
  (cond ((has_bindings_Q_ ee)
	 (let ((var_name (get_var_name_from_binding ee))
	       (binding_value (get_binding_value_from_binding ee all_bindings))
	       (remaining_bindings (get_rest_of_bindings ee)))
           (cond ((no_timestamp_binding_Q_ ee)
		  (print_frame_helper remaining_bindings 
				      all_bindings 
				      (cons 
				       (cons var_name binding_value)
				       accumulator)))
		 (else 
		  (print_frame_helper remaining_bindings 
				      all_bindings 
				      accumulator)))))
        (else accumulator)))

(define (print-frame e)
  (let ((final_result (print_frame_helper e e '())))
    final_result))


(define db
  '(
    ((some 0))
    ((some 10))
    ((some 20))
    ((some 30))
    ((eq ("?" X) ("?" X)))
    ((neq ("?" X) ("?" Y))
     (eq ("?" X) ("?" Y)) ! fail)
    ((neq ("?" X) ("?" Y)))
   ))

(define goals '((some ("?" X))
		  (some ("?" Y))
		  (neq ("?" X) ("?" Y))
		  (eq ("?" X) ("@" "add" ("?" X) ("?" Y)))))


(define (resolveArgs a bindings)
  (resolveArgsHelper a '() bindings))

(define (resolveArgsHelper args accumulator bindings)
  (cond ((null? args)
	 accumulator)
	(else
	 (resolveArgsHelper (cdr args) 
			      (AppendInefficient accumulator
						 (list (value (car args) bindings)))
			      bindings))))


(define (foreign? expr)
  (and (pair? expr)
       (string? (car expr))
       (string=? "@" (car expr))))

(define (call-foreign expr bindings)
  (let ((func (cadr expr))
	(args (cddr expr)))

    (cond ((string=? "unity" func)
	   (car args))

	  ((string=? "add" func)
	   (let ((resolved-args (resolveArgs args bindings)))
	     ; (display "add bindings ") (display bindings) (newline)
	     ; (display "add args ") (display args) (newline)
	     ; (display "add resolved-args ") (display resolved-args) (newline)
	     ; (newline)
	     (+ (car resolved-args) (cadr resolved-args))))

	  ((string=? "display" func)
	   (let ((a (value (car args) bindings)))
	     (display a)))
	  
	  ((string=? "newline" func)
	   (newline))
	  
	  (else (error "call-foreign called with unknown operator" func)))))

; 9-slide PROVE
(clear_result)
(newline)  
(newline)  
(prove6 '() goals db empty 1 '() db)
(display_result)
(newline)  
(newline)

;; end prolog-6a.scm
