(import scheme)
(import (chicken base))
(import (chicken irregex))
(import (chicken format))
(import (srfi 18))
(import inotify)
(import scsh-process)

(init!)
(on-exit clean-up!)
(add-watch! "/dev" '(create))

(define re '(or (: "sd" alpha (? num))
                (: "mmcblk" num "p" num)))

(define timeout 3)

(let loop ()
  (let* ((event (next-event!))
         (device (event-name event))
         (path (format "/dev/~a" device)))
    (when (irregex-match re device)
      (printf "Attempting to mount ~a at ~a\n" device path)
      (thread-sleep! timeout)
      (run (udisksctl mount "--block-device" ,path "--no-user-interaction"))))
  (loop))