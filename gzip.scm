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

  ; flag bits
  (define FTEXT 0)
  (define FHCRC 1)
  (define FEXTRA 2)
  (define FNAME 3)
  (define FCOMMENT 4)

  (define-foreign-variable Z_NULL unsigned-c-string)

  (define (crc32 checksum buf size)
    ((foreign-lambda unsigned-long "crc32" unsigned-long unsigned-c-string unsigned-int)
     checksum buf size))

  (define (port->fname port)
    (let ((fname (port-name port)))
      (if (string-suffix? ".gz" fname) (string-drop-right fname 3) fname)))

  ; TODO: use endian-blob? tbh i find it cumbersome..
  ;
  ; (define (pack-u32 n)
  ;  (byte-blob->string (endian-blob-object (u32vector->endian-blob (u32vector n) LSB))))
  ;
  ; or...write a pack macro

  (define (pack-u32 n)
    (list->string (map integer->char
                       (list (bitwise-and n #x000000ff)
                             (arithmetic-shift (bitwise-and n #x0000ff00) -8)
                             (arithmetic-shift (bitwise-and n #x00ff0000) -16)
                             (arithmetic-shift (bitwise-and n #xff000000) -24)))))

  (define (unpack-u32 s)
    (let ((bv (map char->integer (string->list s))))
      (+ (list-ref bv 0)
         (arithmetic-shift (list-ref bv 1) 8)
         (arithmetic-shift (list-ref bv 2) 16)
         (arithmetic-shift (list-ref bv 3) 24))))

  (define (unpack-u16 s)
    (let ((bv (map char->integer (string->list s))))
      (+ (list-ref bv 0)
         (arithmetic-shift (list-ref bv 1) 8))))

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

  (define (read-gzip-header input-port)
    (let ((magic (read-string 2 input-port)))
      (and (not (eof-object? magic))
           (begin
             (when (not (string=? magic "\x1f\x8b"))
               (error "Invalid magic bytes" magic))
             (let ((method (read-string 1 input-port)))
               (when (not (= method "\x08"))
                 (error "Invalid compression method" method)))
             (letrec ((flags (string->number (read-string 1 input-port)))
                      (read-c-string
                        (lambda ()
                          (unless (string=? (read-string 1 input-port) "\x00")
                            (read-c-string)))))
               (begin
                 (when (bit->boolean flags FEXTRA)
                   (let ((extra-length (unpack-u16 (read-string 2 input-port))))
                     (read-string extra-length input-port)))
                 (when (bit->boolean flags FNAME)
                   (read-c-string))
                 (when (bit->boolean flags FCOMMENT)
                   (read-c-string))
                 (when (bit->boolean flags FHCRC)
                   (read-string 2 input-port))
                 #t)))))))

  (define (read-gzip-trailer buf input-port) 42)

  (define (open-gzip-compressed-input-port #!optional (input-port (current-input-port)))
    (let* ((inflate-port (open-zlib-compressed-input-port input-port))
           (buf "")
           (checksum (crc32 0 Z_NULL 0))
           (size 0))
      (begin
        (and (read-gzip-header input-port)
             (make-input-port
               (lambda ()  ; read-char
                 (let ((char (read-char inflate-port)))
                   (begin
                     (if (eof-object? char)
                         (begin
                           (read-gzip-trailer buf input-port)
                           (set! buf "")
                           (when (read-gzip-header input-port)
                             (set! char (read-char inflate-port))
                             (set! buf (string-append buf (write-string 1 char)))))
                         (set! buf (string-append buf (write-string 1 char))))
                     char)))
               (lambda ()  ; char-ready?
                 (or (char-ready? inflate-port)
                     (begin
                       (read-gzip-trailer buf input-port)
                       (set! buf "")
                       (read-gzip-header))))
               (lambda ()  ; close
                 (close-input-port inflate-port))))))))
