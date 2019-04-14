(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken file posix))
(import (chicken format))
(import (chicken process-context))
(import (chicken sort))
(import (srfi 1))
(import sxml-serializer)

(define (ls dir)
  (receive (directories files) (partition directory? (directory dir))
    (let ((directories (map (lambda (d) (format "~a/" d)) directories)))
      (append (sort directories string-ci<?)
              (sort files string-ci<?)))))

(define (html dir)
  (serialize-sxml
   `(html
     (head
      (title "Index"))
     (body
      (ul
       ,@(map (lambda (entry) `(li (a (@ (href ,entry)) ,entry)))
              (ls dir)))))))

(define (main)
  (when (not (< (length (command-line-arguments)) 2))
    (fprintf (current-error-port) "usage: ~a [dir]" (program-name)))
  (if (null? (command-line-arguments))
      (display (html (current-directory)))
      (display (html (car (command-line-arguments))))))

(main)
