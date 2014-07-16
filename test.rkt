#lang racket
; test.rkt
(require "main.rkt")

; In this example, we play an 8-bit sine wave generated from a bytestring.
(define sinewave
  (list->bytes
   (for/list ([i (in-range 0 (* 2 pi) 0.07)])
     (inexact->exact
      (floor
       (+ 128
          (* (sin i) 64)))))))

; First, we must open the sound device and create a context for it:
;; Initialize OpenAL (see the OpenAL guide)
(define device (open-device #f))
(define context (create-context device))
(set-current-context context)

; Once we have a device context, we must create a buffer to hold our sound data.
;; Make our OpenAL buffer
(define buffer (car (gen-buffers 1)))
;; Copy the bytes to this buffer
(buffer-data buffer
             AL_FORMAT_MONO8
             sinewave
             44100)

;; Make our OpenAL source
(define source (car (gen-sources 1)))

;; Bind our buffer to the source -- 8-bit mono
(set-source-buffer! source buffer)

;; Loop forever (without this, the source will play just once and then stop)
(set-source-looping! source AL_TRUE)

;; Start playing
(play-source source)

;; Wait until we're fisished playing
(sleep 5)

;; Clean up
(delete-sources! (list source))
(delete-buffers! (list buffer))
(set-current-context #f)
(destroy-context! context)
(close-device! device)

#|
(define device (open-device #f))
(open-capture-device device 44100 AL_FORMAT_STEREO16 400)
(close-capture-device! device)
|#
