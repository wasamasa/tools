(import scheme)
(import (chicken base))
(import (chicken port))
(import (chicken pretty-print))
(import (chicken process-context))
(import http-client)
(import medea)

(when (not (= (length (command-line-arguments)) 1))
  (with-output-to-port (current-error-port)
    (lambda () (print "Usage: ./push <message>")))
  (exit 1))

(pp
 (with-input-from-request "https://api.pushover.net/1/messages.json"
                          `((token . ,(get-environment-variable "API_TOKEN"))
                            (user . ,(get-environment-variable "API_USER"))
                            (message . ,(car (command-line-arguments))))
                          read-json))
