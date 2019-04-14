(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken file posix))
(import (chicken format))
(import (chicken io))
(import (chicken irregex))
(import (chicken port))
(import (chicken process))
(import (chicken process-context))
(import (chicken string))
(import (srfi 1))
(import scsh-process)
(import sql-de-lite)

(define (xdg-path environment-variable fallback path)
  (let ((home (get-environment-variable environment-variable)))
    (if (and home (eqv? (string-ref home 0) #\/))
        (string-append home path)
        (string-append (get-environment-variable "HOME") fallback path))))

(define urls-file (xdg-path "XDG_CONFIG_HOME" "/.config" "/newsboat/urls"))
(define db-file (xdg-path "XDG_DATA_HOME" "/.local/share" "/newsboat/cache.db"))

(define (die message . args)
  (with-output-to-port (current-error-port)
    (lambda () (apply print message args)))
  (exit 1))

(define (table-columns db table)
  (query fetch-column
         (sql db "SELECT name FROM pragma_table_info(?);") table))

(define (schema-check db)
  (let ((rss-feed-cols (table-columns db "rss_feed"))
        (rss-item-cols (table-columns db "rss_item")))
    (and (lset<= string=? '("rssurl") rss-feed-cols)
         (lset<= string=? '("title" "url" "feedurl" "unread") rss-item-cols))))

(define (sanity-check)
  (when (not (file-exists? urls-file))
    (die "No URLs file at " urls-file))
  (when (not (file-exists? db-file))
    (die "No DB file at " db-file))
  (when (not (call-with-database db-file schema))
    (die "No schema found for DB"))
  (when (not (call-with-database db-file schema-check))
    (die "Unexpected schema")))

(define (add-feed! url)
  (let ((fd (file-open urls-file (+ open/wronly open/append))))
    (file-write fd (string-append url "\n"))
    (file-close fd)))

(define (reload-feeds!)
  (run (newsboat "-x" "reload") (> "/dev/null")))

(define (unread-feeds db)
  (query fetch-all
         (sql db "SELECT url, title FROM rss_item WHERE unread = 1;")))

(define (print-unread!)
  (for-each
   (lambda (row) (apply printf "~a ~a\n" row))
   (call-with-database db-file unread-feeds)))

(define (catch-up-with-all! db)
  (exec (sql db "UPDATE rss_item SET unread = 0 WHERE unread = 1;")))

(define (known-feed? db feed-url)
  (query fetch-value
         (sql db "SELECT 1 FROM rss_feed WHERE rssurl = ?;") feed-url))

(define (catch-up-with-feed! db feed-url)
  (when (not (known-feed? db feed-url))
    (die "Unknown feed url"))
  (exec (sql db "UPDATE rss_item SET unread = 0 WHERE unread = 1 AND feedurl = ?;") feed-url))

(define (catch-up! #!optional feed-url)
  (if (not feed-url)
      (call-with-database db-file catch-up-with-all!)
      (call-with-database db-file (cut catch-up-with-feed! <> feed-url))))

(define (db-feed-urls)
  (call-with-database db-file
    (lambda (db) (query fetch-column (sql db "SELECT rssurl FROM rss_feed;")))))

(define (url? line)
  (and-let* ((index (substring-index "http" line 0)))
    (zero? index)))

(define (file-feed-urls)
  (let* ((lines (call-with-input-file urls-file read-lines))
         (url-lines (filter url? lines)))
    (map (lambda (line) (car (string-split line))) url-lines)))

(define ie10-user-agent "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)")
(define custom-user-agent "linux:newsboatctl:0.0.1 (wasamasa)")

(define (http-header-lines header pattern subexp)
  (filter-map
   (lambda (line)
     (let ((match (irregex-search pattern line)))
       (if match
           (irregex-match-substring match subexp)
           #f)))
   header))

(define (dbg thing)
  (display "XXX: ")
  (write thing)
  (newline)
  thing)

(define (http-status-codes header)
  (map string->number
       (http-header-lines header "^HTTP/[0-9](.[0-9])? ([0-9]+) " 2)))

(define (http-locations header)
  (http-header-lines header "^Location: (.*)$" 1))

(define (check-feed url)
  (receive
   (in out pid)
   (process "curl" (list "-siLA" custom-user-agent url))
   (close-output-port out)
   (let ((lines (read-lines in)))
     (close-input-port in)
     (if (null? lines)
         #f
         (let ((status-codes (http-status-codes lines)))
           (map (lambda (status)
                  (cond
                   ((< status 300) (list 'ok status))
                   ((< status 400) (list 'redirect status (http-locations lines)))
                   (else (list 'error status))))
                status-codes))))))

(define (check-feeds!)
  (for-each (lambda (url)
              (let ((status-codes (check-feed url)))
                (if status-codes
                    (map (lambda (status)
                           (let ((type (car status))
                                 (code (cadr status)))
                             (case type
                               ((ok) (print code " OK: " url))
                               ((redirect) (apply print code " Redirect(s): "
                                                  (list-ref status 2)))
                               ((error) (print code " Error: " url)))))
                         status-codes)
                    (print "Failed checking url: " url))))
            (file-feed-urls)))

(define prog "newsboatctl")
(define usage
  (format "usage:
~a add-feed <url>
~a reload-feeds
~a print-unread
~a catch-up [url]
~a check-feeds" prog prog prog prog prog))

(define (main)
  (sanity-check)
  (when (null? (command-line-arguments))
    (die usage))
  (let ((command (string->symbol (car (command-line-arguments))))
        (args (cdr (command-line-arguments))))
    (case command
      ((add-feed)
       (when (null? args)
         (die "add-url command requires an URL argument"))
       (add-feed! (car args)))
      ((reload-feeds) (reload-feeds!))
      ((print-unread) (print-unread!))
      ((catch-up)
       (if (null? args)
           (catch-up!)
           (catch-up! (car args))))
      ((check-feeds) (check-feeds!))
      (else (die "Invalid command given")))))

(main)
