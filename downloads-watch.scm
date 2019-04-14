(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken irregex))
(import (chicken pathname))
(import (chicken process-context))
(import (srfi 18))
(import inotify)
(import scsh-process)

(define downloads-dir
  (make-pathname (get-environment-variable "HOME") "downloads"))
(define remote-dir "box:torrents/autoadd")

(define torrent-re '(: ".torrent" eos))
(define timeout 1)

(init!)
(on-exit clean-up!)
(add-watch! downloads-dir '(close-write moved-to))

(let loop ()
  (let* ((event (next-event!))
         (flags (event-flags event))
         (file (event-name event))
         (path (make-pathname downloads-dir file)))
    (when (and (= (length flags) 1) ; file written / moved
               (irregex-search torrent-re file)
               (file-exists? path)) ; file might have disappeared
      (print path)
      (thread-sleep! timeout)
      (print "uploading file...")
      (run (rsync "--remove-source-files" ,path ,remote-dir))))
  (loop))
