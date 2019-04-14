(import scheme)
(import (chicken base))
(import (chicken process-context))
(import (srfi 69))
(import nanomsg)
(import scsh-process)

(define (get-credential key)
  (let ((pass (run/string (pass ,key) (> 2 "/dev/null"))))
    (if (zero? (string-length pass)) ; pass failed?
        #f
        pass)))

(define credentials (make-hash-table string=? string-hash))

(define allowed-credentials
  '("mail/hurrus.durrus@gmail.com"
    "mail/v.schneidermann@gmail.com"
    "mail/bevuta.com"
    "mail/mailbox.org"
    "remote/c4/wasamasa"))

(define (load-credentials!)
  (for-each
   (lambda (key)
     (let ((value (get-credential key)))
       (when (not value)
         (error "failed retrieving credential"))
       (hash-table-set! credentials key value)))
   allowed-credentials))

(define socket #f)
(define url "ipc:///tmp/passjar")

(define (recv)
  (let loop ()
    (let* ((key (nn-recv socket))
           (value (hash-table-ref/default credentials key #f)))
      (if value
          (nn-send socket value)
          (nn-send socket ""))
      (loop))))

(define (send message)
  (nn-send* socket message 0) ; this one will error if there's no daemon
  (nn-recv socket))

(define (main)
  (set! socket (nn-socket 'pair))
  (set! (nn-socket-sndtimeo socket) 1)
  (if (null? (command-line-arguments)) ; daemon?
      (begin
        (load-credentials!)
        (nn-bind socket url)
        (recv))
      (begin
        (nn-connect socket url)
        (let ((value (send (car (command-line-arguments)))))
          (if (zero? (string-length value))
              (exit 1)
              (display value))))))

(main)
