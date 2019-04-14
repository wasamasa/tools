(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken format))
(import (chicken pathname))
(import (chicken process-context))
(import (chicken string))
(import ssax)
(import sxpath)

(define (die message)
  (fprintf (current-error-port) "~a\n" message))

(when (null? (command-line-arguments))
  (die "usage: fb2rename <path>..."))

(define fb2-ns "http://www.gribuser.ru/xml/fictionbook/2.1")

(for-each
 (lambda (path)
   (call-with-input-file path
     (lambda (in)
       (let* ((book (ssax:xml->sxml in `((#f . ,fb2-ns))))
              (author (string-intersperse
                       ((sxpath "/FictionBook/description/title-info/author/*/text()") book)))
              (title ((sxpath "string(/FictionBook/description/title-info/book-title)") book))
              (filename (format "~a - ~a.fb2" author title))
              (new-path (make-pathname (pathname-directory path) filename)))
         (when (or (not author) (not title))
           (die (format "couldn't determine author or title, skipping ~a" path)))
         (printf "~a -> ~a\n" path new-path)
         (rename-file path new-path)))))
 (command-line-arguments))
