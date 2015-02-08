#lang racket
(require "../main.rkt")

(define device (open-device #f))
(define context (create-context device))
(set-current-context context)
(define buffers (gen-buffers 1))
(printf "Buffers: ~a\n" buffers)
(printf "Make Buffer Error: ~a\n" (get-last-error device))

(define buffer (car buffers))

; simple square wave or something
(buffer-data buffer AL_FORMAT_STEREO8 #"ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppPppppppppppppppppppppppppppppppp" 44100)
(printf "Data Buffer Error: ~a\n" (get-last-error device))

(define sources (gen-sources 1))
(printf "Sources: ~a\n" sources)

(printf "Sources Error: ~a\n" (get-last-error device))

(set-source-buffer! (car sources) buffer)
(printf "Bind Buffer Error: ~a\n" (get-last-error device))

(play-source (car sources))
(printf "Play Error: ~a\n" (get-last-error device))
(set-source-looping! (car sources) AL_TRUE)
(printf "Looping Error: ~a\n" (get-last-error device))

(sleep 1)
(printf "Source state: ~a\n" (source-source-state (car sources)))



(set-current-context #f)
(destroy-context! context)
(close-device! device)



