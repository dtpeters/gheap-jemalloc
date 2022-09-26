; -----
; This file is part of gheap-jemalloc and is lincensed under the gplv3+ license
; See the COPYING file for details
;
; GDB jemalloc-5.3.0 Plugin for the standalone jemalloc for gnu/linux
; Version: 1.0.0
; License: GPLv3
; Author: Ernest Deak
;
;   ,
;  -+-
;   1
; -----
(use-modules (gdb) (ice-9 format) (oop goops))

(define println (lambda (str) (format #t "~a~%" str)))
(define neat-print (lambda (ls)
                     (println (string-join ls))))
(define neat-eval (lambda (expression)
                    (value-print (parse-and-eval expression))))

(define MAJOR_VERSION 1)
(define MINOR_VERSION 0)
(define PATCH_VERSION 0)
(define VERSION_STRING (format #f "~a.~a.~a" MAJOR_VERSION MINOR_VERSION PATCH_VERSION))

; ==== symbol related ========================================{{{
;setup for future and past symbol changes
(define get-jemalloc-version (lambda () "5.3.0")) ; hardcoded for now. will be replaced by some function that checks for the version

; === jemalloc's edata structure === {{{
(define-class <edata-generic> ()
              (e_addr #:init-value "ERROR")
              (e_bits #:init-value "ERROR"))

(define-class <edata-5.3.0> (<edata-generic>)
              (e_addr #:init-value "e_addr")
              (e_bits #:init-value "e_bits"))

; e_bits specific functions{{{
(define e_bits-arena_ind
  (lambda (b)
    (bit-extract b 0 12)))

(define e_bits-slab
  (lambda (b)
    (bit-extract b 12 13)))

(define e_bits-commited
  (lambda (b)
    (bit-extract b 13 14)))

(define e_bits-pai
  (lambda (b)
    (bit-extract b 14 15)))

(define e_bits-zeroed
  (lambda (b)
    (bit-extract b 15 16)))

(define e_bits-guarded
  (lambda (b)
    (bit-extract b 16 17)))

(define e_bits-state
  (lambda (b)
    (bit-extract b 17 20)))

(define e_bits-szind
  (lambda (b)
    (bit-extract b 20 28)))

(define e_bits-nfree
  (lambda (b)
    (bit-extract b 28 38)))

(define e_bits-bin_shard
  (lambda (b)
    (bit-extract b 38 44)))

(define-class <edata-bitfield-struct> ()
              (arena_ind)
              (slab)
              (commited)
              (pai)
              (zeroed)
              (guarded)
              (state)
              (szind)
              (nfree)
              (bin_shard))

; --------------------------------------------------------------------------------------------------------------------------------------------
; indexes of individual bits for e_bits bitfield because it was kind of hard to keep track of just in my head
; explanation of these letters can be found in jemalloc's source: jemalloc-5.3.0/include/jemalloc/internal/edata.h
;
;                                                                                                           15 14 13 12 11 10  9 8     76543210
;                                                                                                            z  p  c  b  a  a  a a     aaaaaaaa
;                                                       31 30 29 28 27 26 25 24    23 22 21 20 19 18 17 16
;                                                        f  f  f  f  i  i  i  i     i  i  i  i  t  t  t  g  
; 47 46 45 44 43 42 41 40    39 38 37 36 35 34 33 32
;  0  0  0  0  s  s  s  s     s  s  f  f  f  f  f  f 
; --------------------------------------------------------------------------------------------------------------------------------------------

;}}}

(define get-edata-for-version
  (lambda (v)
    (cond
      (#nil (make <edata-5.3.0>))
      ((equal? v "5.3.0") (make <edata-5.3.0>)))))

(define-method (edata_t-sym (edata <edata-generic>) (sym <symbol>))
               (slot-ref edata sym))

(define edata_s (get-edata-for-version (get-jemalloc-version)))

;}}}

; === jemalloc specific symbols  and functions ===={{{
(define-class <jemalloc-symbols-generic> ()
              (arena #:init-keyword #:arena)
              (slabcur #:init-keyword #:slabcur)
              (bins #:init-keyword #:bins)
              (nbins #:init-keyword #:nbins)
              (narenas #:init-keyword #:narenas))

(define-method (je-sym (je <jemalloc-symbols-generic>) (sym <symbol>))
               (slot-ref je sym))

(define-class <jemalloc-symbols-5.3.0> (<jemalloc-symbols-generic>))
(define je (make-instance <jemalloc-symbols-5.3.0>
                          #:arena "a0"
                          #:slabcur "slabcur"
                          #:bins "bins"
                          #:nbins "nbins_total"
                          #:narenas "narenas_total"))
;}}}

; Symbol defines {{{
(define SYM_arena (je-sym je 'arena))
(define SYM_arena_bin (je-sym je 'bins))
(define SYM_slabcur (je-sym je 'slabcur))
(define arena-bins (format #f "~a.~a" SYM_arena SYM_arena_bin))

(define SYM_nbins_total (je-sym je 'nbins))
(define SYM_narenas_total (je-sym je 'narenas))

(define SYM_edata_t "edata_t")

(define bin_struct_size_bytes 224)
(define bin_struct_size_qwords 28)
;}}}

; === defines for symbols with formats ==={{{

; returns a gdb:value, this should represent an address
(define arena-bins-at (lambda (idx) (parse-and-eval (format #f "~a[~a]" arena-bins idx))))

(define arena-bins-field-at (lambda (idx field) (parse-and-eval (format #f "~a[~a].~a" arena-bins idx field))))
(define arena-bins-slab-field-at (lambda (idx slab_subfield) (parse-and-eval (format #f "~a[~a].~a.~a" arena-bins idx SYM_slabcur slab_subfield))))
(define arena-slab-info (lambda (slabaddr) (execute (format #f "p -pretty on -- *(edata_t *) ~a" slabaddr))))
;==============================================================
;}}}

;}}}

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

(define slab-field-ref
  (lambda (slabaddr field_str) ; slabaddr is expected to be just an integer or hex number representing an address, field_str is just the field as a string
    (let ((slabfmt (format #f "((~a *) ~a)" SYM_edata_t slabaddr)))
      (parse-and-eval (format #f "~a.~a" slabfmt field_str)))))

(define DEBUG #t)

(define dbg
  (lambda (str form)
    (if (eq? DEBUG #t)
      (begin (format #t "~a~%" str) (form))
      (form))))

(define-syntax slabfield
  (syntax-rules (-> as)
    ((slabfield slabaddr -> fieldname as vprinttype)
     (vprinttype (slab-field-ref slabaddr fieldname)))
    ))

(define show-slab-info
  (lambda (slabaddr)
    (format #t "     `-> e_addr: ~a ~%" (slabfield slabaddr -> "e_addr" as value-print))
    (format #t "     `-> e_bits: 0x~x ~%" (slabfield slabaddr -> "e_bits" as value->integer))
    (format #t "         | ~30a: 0x~x ~%" "ARENA INDEX" (e_bits-arena_ind (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "SLAB" (e_bits-slab (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "COMMITED" (e_bits-commited (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "PAI" (e_bits-pai (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "ZEROED" (e_bits-zeroed (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "GUARDED" (e_bits-guarded (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "STATE" (e_bits-state (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: ~d ~%" "USABLE SIZE CLASS INDEX" (e_bits-szind (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: ~d ~%" "FREE REGIONS IN SLAB" (e_bits-nfree (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         | ~30a: 0x~x ~%" "BIN SHARD" (e_bits-bin_shard (slabfield slabaddr -> "e_bits" as value->integer)))
    (format #t "         `----------------------------------- ~%")
    ))

(define show-all-slabs (lambda (x)
                         (if (< x (string->number (nbins_total)))
                           (begin
                             (format #t "bin[~a]: ~%" x)
                             (format #t " `-> slabcur: ~a ~%" (value-print (arena-bins-field-at x SYM_slabcur)))
                             (let ((slabaddr (parse-and-eval (format #f "a0.bins[~a].slabcur" x))))
                               (if (not (= (value->integer slabaddr) 0))
                                 (show-slab-info slabaddr)
                                 ))
                             (show-all-slabs (+ x 1))))))

; ===== command registration section ====={{{
(let ((newcmd (make-command "jearena"
                            #:command-class COMMAND_OBSCURE
                            #:doc "Show the jemalloc arena struct + address"
                            #:invoke (lambda (self arg from-tty)
                                       (execute (format #f "p -object on -array on -pretty on -- *~a" SYM_arena))
                                       (neat-print (list SYM_arena ": " (neat-eval SYM_arena) )))
                            )))
  (register-command! newcmd))
(register-command! (make-command "jeslabs" 
                                 #:command-class COMMAND_OBSCURE 
                                 #:doc "Display all slabs(runs) currently defined in the arena with their information"
                                 #:invoke (lambda (self arg from-tty) (show-all-slabs 0))))

(register-command! (make-command "jeslab"
                                 #:command-class COMMAND_OBSCURE
                                 #:doc "Show information about a slab"
                                 #:invoke (lambda (self arg from-tty)
                                            (show-slab-info arg))))

(register-command! (make-command "je_narenas_total" #:command-class COMMAND_OBSCURE #:invoke (lambda (self arg tty) (println (narenas_total)))))
(register-command! (make-command "je_nbins_total" #:command-class COMMAND_OBSCURE #:invoke (lambda (self arg tty) (println (nbins_total)))))

;}}}
