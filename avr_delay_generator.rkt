#lang racket
; avr_delay_generator.rkt
; (c) 2010 R. Altenburg
;
; This code was designed to generate reasonably 
; accurate delay loops that operate with a minimal
; amount of instructions.
;
; Do not rely on the timing estimates for any high degree
; of precision. Interrupts are not disabled and different
; chips and clock sources have varying degrees of stability. 


(provide/contract
  [delay  (-> (and/c positive? rational?) 
              (and/c positive? rational?)
              output-port?)]
  )
 
; convert a base-10 integer to list of digit values in a given base
(define (base10->radix intb10 radix [retvar '()])
  (if (zero? intb10)
      (reverse retvar)
      (base10->radix (floor (/ intb10 radix)) radix
                     (append retvar (list (remainder intb10 radix))))))

; generate the avr-asm code
(define (generate-delay-code blocks final count out) 
  (display (format "delay_~a:\n" count) out) ; label the proceedure
  
  (when (zero? count) ; clear the registers on the first pass
    (for-each (λ (x)
                (display (format "\tclr r~a\n" (+ 1 x)) out)) 
              (build-list blocks values)))
  (unless (zero? final) ; when there are steps needed in this level
    (display (format "\tldi r16 ~a\n"  final) out) ; load r16
    (display (format "dlb_~a:\t\t\n" count) out)   ; label the loop
    
    (for-each (λ (x)
                (display (format "\tdec r~a\n" (+ 1 x)) out)    ; dec
                (display (format "\tbrne dlb_~a\n" count) out)) ; brne
              (build-list blocks values))  
    (display (format "\tdec r16\n") out)
    (display (format "\tbrne dlb_~a\n" count) out)
    )
  
  (when (zero? blocks) (display (format "\tret\n") out)) ; print ret at the end
  
  )

; delay
; takes seconds and processor speed in Hz
; returns an output-port containing the avr assembly code
(define (delay sec cpu-hz)
  (define out (open-output-string))
  (let* [(final
          (base10->radix
           (- (inexact->exact (ceiling (/ (* sec cpu-hz) 3))) 3) 256))
         (stage (build-list (length final) values))
         (total 0)
         ]     
    
    (display (format "; AVR Delay loop for ~a sec @ ~a MHz\n" 
                     sec
                     (/ cpu-hz 1000000)) out)
    ; display timing info
    (for-each (λ (blk fin stg)
                (let [(cycles (+ 1 (* fin (* 3 (expt 256 blk)))))]
                  (when (zero? stg)(set! cycles (+ cycles blk))) ; time for clrs
                  (when (zero? blk) (set! cycles (+ cycles 4))) ; time for ret (16-bit pc)
                  (display (format "; delay_~a: ~a cycles (~a sec)~n"
                                   stg cycles 
                                   (exact->inexact (/ cycles cpu-hz))) out)
                  (set! total (+ total cycles))
                  cycles
                  )
                
                ) (reverse stage) final stage)
    (display (format "; total delay: ~a cycles (~a sec)~n~n"
                     total
                     (exact->inexact (/ total cpu-hz))
                     ) out)
    ; ----- end of timing info -----
    
    ; display the code  
    (for-each (λ (rs f s)
                (generate-delay-code rs f s out))
                (reverse stage) final stage )
    )
  out
  )

;; for testing...
;(display (bytes->string/utf-8 (get-output-bytes (delay 0.5 1000000))))