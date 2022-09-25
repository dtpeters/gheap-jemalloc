; ----
; GDB jemalloc-5.3.0 Plugin for the standalone jemalloc for gnu/linux
; Version: 1.0.0
; License: GPLv3
; ----
(use-modules (gdb) (ice-9 format) (oop goops))
(define println (lambda (str) (format "~a~%" str)))
;(define root_node (parse-and-eval "root_node"))
;(define a0 (parse-and-eval "a0"))
;(define b0 (parse-and-eval "b0"))
;NOTE: value-print to convert values to printable forms
(define neat-print (lambda (ls)
					(println (string-join ls))))
(define neat-eval (lambda (expression)
					(value-print (parse-and-eval expression))))

; ==== symbol related ========================================
;setup for future symbol changes

(define-class <jemalloc-symbols-5.3.0> ()
			  arena
			  slabcur
			  nbins
			  narenas)
(define jemalloc-syms (make <jemalloc-symbols-5.3.0> #:arena "a0"))

(define SYM_arena (slot-ref jemalloc-syms 'arena))
(define SYM_arena_bin "bins")
(define SYM_slabcur "slabcur")
(define arena-bins (format #f "~a.~a" SYM_arena SYM_arena_bin))

(define SYM_nbins_total "nbins_total")
(define SYM_narenas_total "narenas_total")

; returns a gdb:value, this should represent an address
(define arena-bins-at (lambda (idx) (parse-and-eval (format #f "~a[~a]" arena-bins idx))))
(define arena-bins-field-at (lambda (idx field) (parse-and-eval (format #f "~a[~a].~a" arena-bins idx field))))
(define arena-bins-slab-field-at (lambda (idx slab_subfield) (parse-and-eval (format #f "~a[~a].~a.~a" arena-bins idx SYM_slabcur slab_subfield))))
;==============================================================

(define nbins_total (lambda ()
						  (neat-eval SYM_nbins_total)))

(define narenas_total (lambda ()
							(neat-eval (format #f "~a->repr" SYM_narenas_total))))

(define alltostr (lambda (ls)
				   (string-join (map (lambda (e)
									   (if (value? e)
										 (value-print e)
										 (format #f "~a" e))
									   ) ls))))

(define show-all-slabs (lambda (x)
						 (if (< x (string->number (nbins_total)))
						   (begin
							 (format #t "bin[~a]: ~%" x)
							 (format #t " `-> slabcur: ~a ~%" (value-print (arena-bins-field-at x SYM_slabcur)))
							 (let ((slabaddr (parse-and-eval (format #f "a0.bins[~a].slabcur" x))))
							   (if (not (= (value->integer slabaddr) 0))
								 (begin
								   (format #t "     `-> e_addr: ~a ~%" (value-print (arena-bins-slab-field-at x "e_addr")))
								   (format #t "     `-> e_bits: 0x~x ~%" (value->integer (arena-bins-slab-field-at x "e_bits")))
								   )
								 ))
							 (show-all-slabs (+ x 1))))))

; ===== command registration section =====
(let ((newcmd (make-command "jearena"
							#:command-class COMMAND_OBSCURE
							#:doc "Show the jemalloc arena struct + address"
							#:invoke (lambda (self arg from-tty)
									   (execute (format #f "p -object on -array on -pretty on -- *~a" arena-bins))
									   (neat-print (list SYM_arena ": " (neat-eval SYM_arena) )))
									   )))
  (register-command! newcmd))
(register-command! (make-command "jeslabs" 
								 #:command-class COMMAND_OBSCURE 
								 #:doc "Display all slabs(runs) currently defined in the arena"
								 #:invoke (lambda (self arg from-tty) (show-all-slabs 0))))
(register-command! (make-command "je_narenas_total" #:command-class COMMAND_OBSCURE #:invoke (lambda (self arg tty) (println (narenas_total)))))
(register-command! (make-command "je_nbins_total" #:command-class COMMAND_OBSCURE #:invoke (lambda (self arg tty) (println (nbins_total)))))
