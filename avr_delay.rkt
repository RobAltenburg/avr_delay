#lang racket/gui
; avr_delay.rkt
; (c) 2010 R. Altenburg
;

(require "avr_delay_generator.rkt")

;-------------------------------------------------- main (gui)
(define frame (new frame% [label "AVR_delay"]
                   [width 350]
                   [height 480]))
(define outer-vp (new vertical-panel% [parent frame]
                      [vert-margin 10]
                      [horiz-margin 10]))
(define top-hp (new horizontal-panel% [parent outer-vp]))
(define top-vp-l (new vertical-panel% [parent top-hp]))   
(define top-vp-r (new vertical-panel% [parent top-hp]))                      
(define editor (new editor-canvas%
                    [parent outer-vp] 
                    [min-height 450]))

(define txt-processor-speed
  (new text-field%
       [label "Processor Speed (MHz)"]
       [parent top-vp-l]
       [init-value "1"]))
(define txt-delay
  (new text-field%
       [label "Delay (seconds)           "]
       [parent top-vp-l]
       [init-value "0.5"]
       [style '(single)]))

; generate code button
(define button-go 
  (new button% [label "Generate Code"] [parent top-vp-r]
       (callback (lambda (button event)
                   (send txt-results select-all)
                   (send txt-results delete 0 (send txt-results get-end-position))
                   (send txt-results insert 
                         (bytes->string/utf-8 (get-output-bytes 
(delay (string->number (send txt-delay get-value))
                          (* 1000000 (string->number (send txt-processor-speed get-value))))  #t) #\?) 0)
                   (send editor scroll-to 0 0 0 0 #t)
                   ))))

; copy text button
(define button-copy (new button% [label "Copy All"] [parent top-vp-r]
                         (callback (lambda (button event)
                                     (send txt-results select-all)
                                     (send txt-results copy)
                                     (send editor scroll-to 0 0 0 0 #t)
                                     ))))

(define txt-results (new text%))
(send editor set-editor txt-results)

(send frame show #t)

