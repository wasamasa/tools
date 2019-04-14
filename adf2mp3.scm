(import scheme)
(import (chicken base))
(import (chicken bitwise))
(import (chicken blob))
(import (chicken file posix))
(import (chicken foreign))
(import (chicken io))
(import (chicken process-context))

;; naive solution takes 12s on 50M
(define (convert in-file out-file)
  (with-output-to-file out-file
    (lambda ()
      (with-input-from-file in-file
        (lambda ()
          (let loop ()
            (when (not (eof-object? (peek-char)))
              (write-byte (bitwise-xor (read-byte) 34))
              (loop))))))))

;; better solution takes 0.07s on 50M
(define buffer-size 4096)
(define buffer (make-blob buffer-size))

(define xor-blob
  (foreign-lambda* void ((blob buffer) (size_t count) (unsigned-byte value))
    "for (int i = 0; i < count; ++i) buffer[i] ^= value;"))

(define (convert in-file out-file)
  (let ((in (file-open in-file open/rdonly))
        (out (file-open out-file
                        (+ open/wronly open/creat open/trunc)
                        (+ perm/irusr perm/iwusr))))
    (let loop ()
      (let ((count (cadr (file-read in buffer-size buffer))))
        (when (positive? count)
          (xor-blob buffer count 34)
          (file-write out buffer count)
          (loop))))
    (file-close in)
    (file-close out)))

(define (main)
  (when (not (= (length (command-line-arguments)) 2))
    (display "Usage: adf2mp3 <infile> <outfile>\n" (current-error-port))
    (exit 1))
  (apply convert (command-line-arguments)))

(main)
