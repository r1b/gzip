(module gzip (open-gzip-compressed-input-port open-gzip-compressed-output-port)
  (import (chicken base)
          (chicken bitwise)
          (chicken blob)
          (chicken file posix)
          (chicken foreign)
          (chicken format)
          (chicken io)
          (chicken port)
          (chicken time)
          scheme
          srfi-13
          zlib)
  #>
  #include <zlib.h>
  <#

  (define-foreign-variable Z_NULL unsigned-c-string)

  (define (crc32 checksum buf size)
    ((foreign-lambda unsigned-long "crc32" unsigned-long unsigned-c-string unsigned-int)
     checksum buf size))

  (define (port->fname port)
    (let ((fname (port-name port)))
      (if (string-suffix? ".gz" fname) (string-drop-right fname 3) fname)))

  ; TODO: pull this out into a pack macro
  (define (pack-u32 n)
    (list->string (map integer->char
                       (list (bitwise-and n #x000000ff)
                             (arithmetic-shift (bitwise-and n #x0000ff00) -8)
                             (arithmetic-shift (bitwise-and n #x00ff0000) -16)
                             (arithmetic-shift (bitwise-and n #xff000000) -24)))))

  (define (write-gzip-header output-port level)
    (begin
      ; magic bytes
      (write-string "\x1f\x8b" #f output-port)
      ; compression method (always DEFLATE)
      (write-string "\x08" #f output-port)
      ; flags
      (if (regular-file? output-port)
          (write-string "\x08" #f output-port)
          (write-string "\x00" #f output-port))
      ; mtime (little-endian)
      (write-string (pack-u32 (current-seconds)) #f output-port)
      (write-string (case level
                      ((1) "\x02")
                      ((9) "\x04")
                      (else "\x00"))
                    #f output-port)
      ; unknown os
      (write-string "\xff" #f output-port)
      ; fname
      (when (regular-file? output-port)
          (fprintf output-port "~A\x00" (port->fname output-port)))))

  (define (write-gzip-trailer checksum size output-port)
    (begin
      ; crc-32
      (write-string (pack-u32 checksum) #f output-port)
      ; total bytes written
      (write-string (pack-u32 size) #f output-port)))

  (define (open-gzip-compressed-output-port #!optional (output-port (current-output-port))
                                            #!key (level 9))
    (let ((deflate-port (open-zlib-compressed-output-port output-port
                                                          level: level
                                                          window-bits: -15))
          (buf "")
          (checksum (crc32 0 Z_NULL 0))
          (size 0))
      (begin
        (write-gzip-header output-port level)
        (flush-output output-port)
        (make-output-port
          (lambda (s)  ; write
            (begin
              (set! buf (string-append buf s))
              (set! size (string-length buf))
              (set! checksum (crc32 checksum buf size))
              (write-string s #f deflate-port)))
          (lambda ()  ; close
            (begin
              (close-output-port deflate-port)
              (write-gzip-trailer checksum size output-port)
              (flush-output output-port)))))))

  (define (open-gzip-compressed-input-port #!optional (input-port (current-input-port)))
    42))
