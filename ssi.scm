(import scheme)
(import (chicken base))
(import (chicken io))
(import (chicken irregex))
(import (chicken process-context))
(import (chicken string))

(define ssi-re
  '(: "<!--#" (* space)
      (submatch (* (~ space))) (* space)
      (submatch (*? any)) (* space)
      "-->"))

(define quoted-string-re '(: bos "'" (submatch (* any)) "'" eos))

(define (unquote-string string)
  (let ((without-quotes (irregex-replace quoted-string-re string 1)))
    ;; HACK: won't work on \n
    (string-translate without-quotes "\\")))

(define (ssi-args->list input)
  (let ((args (string-split input)))
    (map (lambda (arg)
           (let ((kv (string-split arg "=")))
             (cons (car kv) (unquote-string (cadr kv)))))
         args)))

(define (uppercase string)
  (list->string (map char-upcase (string->list string))))

(define (ssi-match->string match)
  (let ((directive (irregex-match-substring match 1))
        (args (ssi-args->list (irregex-match-substring match 2))))
    (when (not (equal? directive "echo"))
      (error "unsupported directive"))
    (let ((var (alist-ref "var" args equal?)))
      (when (not var)
        (error "no var argument given"))
      (cond
       ((get-environment-variable (uppercase var))
        => (lambda (env-default) env-default))
       ((alist-ref "default" args equal?)
        => (lambda (default) default))
       (else
        (error "no default found"))))))

(define (convert in-file out-file)
  (let ((contents (with-input-from-file in-file read-string)))
    (with-output-to-file out-file
      (lambda ()
        (display (irregex-replace/all ssi-re contents ssi-match->string))))))

(define (main)
  (when (not (= (length (command-line-arguments)) 2))
    (display "Usage: ssi <infile> <outfile>\n" (current-error-port))
    (exit 1))
  (apply convert (command-line-arguments)))

(main)
