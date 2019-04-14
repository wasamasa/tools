(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken file))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import (srfi 1))
(import (srfi 69))
(import (except sql-de-lite error-message))
(import getopt-long)

(define (xdg-path environment-variable fallback path)
  (let ((home (get-environment-variable environment-variable)))
    (if (and home (eqv? (string-ref home 0) #\/))
        (string-append home path)
        (string-append (get-environment-variable "HOME") fallback path))))

(define db-file
  (xdg-path "XDG_DATA_HOME" "/.local/share" "/firefox/places.sqlite"))

(define (error-message status message . args)
  (with-output-to-port (current-error-port)
    (lambda () (apply print message args)))
  (exit status))

(define (sanity-check)
  (when (not (file-exists? db-file))
    (error-message 1 "No DB file at " db-file))
  (when (not (call-with-database db-file schema))
    (error-message 1 "No schema found for DB")))

(define (where-query column terms)
  (if (null? terms)
      '("")
      (let* ((params (map (lambda (term) (format "%~a%" term)) terms))
             (fragment (format "~a LIKE ?" column))
             (q (string-intersperse (make-list (length terms) fragment)
                                    " AND ")))
        (cons (format " AND ~a" q) params))))

(define (query-history db url-terms title-terms)
  (let* ((url-query (where-query "url" url-terms))
         (title-query (where-query "title" title-terms))
         (q (format "SELECT url FROM moz_places WHERE url LIKE 'http%'~a"
                    (string-append (car url-query) (car title-query))))
         (params (append (cdr url-query) (cdr title-query))))
    (apply query fetch-column (sql db q) params)))

(define (string-starts-with? needle haystack)
  (let ((index (substring-index needle haystack)))
    (and index (zero? index))))

(define (munch string . prefixes)
  (let loop ((prefixes prefixes))
    (if (pair? prefixes)
        (let ((prefix (car prefixes)))
          (if (string-starts-with? prefix string)
              (substring string (string-length prefix) (string-length string))
              (loop (cdr prefixes))))
        string)))

(define (munch-url url)
  (munch (munch url "http://" "https://") "www."))

(define (url-domain url)
  (let ((url (munch-url url)))
    (substring url 0 (substring-index "/" url))))

(define (group-by items proc)
  (let ((table (make-hash-table equal? equal?-hash)))
    (for-each
     (lambda (item)
       (let ((key (proc item))
             (update-proc (lambda (value) (cons item value)))
             (default '()))
         (hash-table-update!/default table key update-proc default)))
     items)
    table))

(define (url<? a b)
  (if (not (= (string-length a) (string-length b)))
      (< (string-length a) (string-length b))
      (string<? a b)))

(define (sort-urls urls)
  (let* ((grouped (hash-table->alist (group-by urls url-domain)))
         (presorted (sort grouped (lambda (a b) (string<? (car a) (car b))))))
    (append-map (lambda (group) (sort (cdr group) url<?)) presorted)))

(define options
  '((title
     "Search term for title"
     (value #t)
     (single-char #\t))
    (help
     "Prints this help"
     (single-char #\h))))

(define usage-hint
  "Usage: browserhistory [--title <title term>]... [<url term>]...\n")

(define (main)
  (sanity-check)
  (when (null? (command-line-arguments))
    (error-message 1 "No terms given\n" usage-hint))
  (let* ((opts (getopt-long (command-line-arguments) options))
         (help? (alist-ref 'help opts))
         (url-terms (alist-ref '@ opts))
         (title-terms (map cdr (filter (lambda (opt) (eq? (car opt) 'title))
                                       opts))))
    (when help?
      (error-message 0 usage-hint (usage options)))
    (when (and (null? url-terms) (null? title-terms))
      (error-message 1 "At least one url or title term must be specified\n"
                     usage-hint))
    (let ((urls (call-with-database db-file
                  (lambda (db)
                    (query-history db url-terms title-terms)))))
      (for-each print (sort-urls urls)))))

(main)
