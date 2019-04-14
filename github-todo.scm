(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken io))
(import (chicken process-context))
(import (srfi 13))
(import http-client)
(import medea)
(import uri-common)

(define token-file
  (format "~a/.github.credentials" (get-environment-variable "HOME")))

(define (access-token)
  (alist-ref 'access-token
             (call-with-input-file token-file
               (lambda (in) (read-string #f in)))))

(define (retrieve-issues)
  (let ((response
         (with-input-from-request
          (string-append "https://api.github.com/search/issues?q="
                         "involves:wasamasa+state:open+sort:updated"
                         "&per_page=100&access_token=" (access-token))
          #f
          read-string)))
    (read-json response)))

(define (transform-issues issues)
  (map (lambda (elt)
         (let* ((url (alist-ref 'html_url elt))
                (path (uri-path (uri-reference url)))
                (repository (format "~A/~A" (cadr path) (list-ref path 2)))
                (title (alist-ref 'title elt))
                (user (alist-ref 'user elt))
                (author (alist-ref 'login user))
                (body (alist-ref 'body elt)))
           ;; apparently github uses DOS-style line endings, that's
           ;; why I'm using these to to avoid confusing Emacs
           (format "# [~A: ~A](~A)\r\n\r\n~A wrote:\r\n\r\n~A\r\n\r\n"
                   repository title url author body)))
       (vector->list (alist-ref 'items issues))))

(define (serialize-issues body)
  (with-output-to-file (format "~a/notes/github.md" (get-environment-variable "HOME"))
    (lambda () (write-string (string-concatenate body)))))

(define (main)
  (serialize-issues (transform-issues (retrieve-issues))))

(main)
