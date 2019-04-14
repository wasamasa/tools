(import scheme)
(import (chicken base))
(import (chicken condition))
(import (chicken file))
(import (chicken file posix))
(import (chicken format))
(import (chicken io))
(import (chicken irregex))
(import (chicken pathname))
(import (chicken port))
(import (chicken process-context))
(import (chicken string))
(import getopt-long)
(import image-dimensions)
(import magic)
(import natural-sort)
(import scsh-process)

(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

(natural-sort-preprocessor string-downcase)

(define work-dir "/tmp/mcp")
(define unpack-dir "/tmp/mcp/unpack")
(define trash-dir "/tmp/mcp/trash")
(define flatten-dir "/tmp/mcp/flatten")
(define output-zip "/tmp/mcp/output.zip")

(define-syntax run!
  (syntax-rules ()
    ((_ epf)
     (receive
         (status out err)
         (run/collecting (1 2) epf (> "/dev/null"))
       (close-input-port out)
       (when (not (zero? status))
         (display (read-string #f err))
         (close-input-port err))))))

(define (check-leftover-files!)
  (when (directory-exists? work-dir)
    (printf "Existing working directory detected, delete files? [y/N] ~!")
    (let ((input (read-line)))
      (if (and (> (string-length input) 0)
               (memv (string-ref input 0) '(#\y #\Y)))
          (delete-directory work-dir 'recursive)
          (exit 1)))))

(define (files-in directory)
  (find-files directory test: (lambda (path) (not (directory? path)))))

(define (string-starts-with? needle haystack)
  (let ((index (substring-index needle haystack)))
    (and index (zero? index))))

(define (image? path)
  (string-starts-with? "image/" (identify path 'mime-type)))

(define (archive? path)
  (irregex-match "application/(zip|x-rar)" (identify path 'mime-type)))

(define (with-current-directory path thunk)
  (let ((old (current-directory)))
    (dynamic-wind
        (lambda () (change-directory path))
        thunk
        (lambda () (change-directory old)))))

(define (trash-file! path)
  (when (directory? path)
    (error "Trying to trash a directory"))
  (let ((to (make-pathname trash-dir (pathname-strip-directory path))))
    (move-file path to 'clobber)))

(define (unnest-inner-archives!)
  (for-each (lambda (path)
              (when (archive? path)
                (print "Archive detected!")
                (with-current-directory (pathname-directory path)
                  (lambda ()
                    (run! (aunpack ,path))
                    (trash-file! path)))))
            (files-in unpack-dir)))

(define (purge-non-image-files!)
  (for-each (lambda (path)
              (when (not (image? path))
                (trash-file! path)))
            (files-in unpack-dir)))

(define (pad-width size)
  (define (log10 n) (/ (log n) (log 10)))
  (max 2 (add1 (inexact->exact (floor (log10 size))))))

(define (left-pad s len char)
  (let ((cur-len (string-length s)))
    (if (< cur-len len)
        (string-append (make-string (- len cur-len) char) s)
        s)))

(define (flatten-image-paths!)
  (let* ((paths (natural-sort (files-in unpack-dir)))
         (digits (pad-width (length paths))))
    (let loop ((paths paths)
               (i 0))
      (when (pair? paths)
        (let* ((path (car paths))
               (index (left-pad (number->string i) digits #\0))
               (old-name (pathname-strip-directory path))
               (new-name (format "~a_~a" index old-name)))
          (move-file path (make-pathname flatten-dir new-name) 'clobber))
        (loop (cdr paths) (add1 i))))))

(define (ensure-portrait-direction!)
  (for-each (lambda (path)
              (let* ((dimensions (call-with-input-file path image-dimensions))
                     (width (car dimensions))
                     (height (cadr dimensions)))
                (when (> width height)
                  (run! (mogrify -rotate 270 ,path)))))
            (files-in flatten-dir)))

(define kobo-width 1080)
(define kobo-height 1440)

(define (optimize-images!)
  (let ((geometry (format "~ax~a>" kobo-width kobo-height)))
    ;; TODO: figure out how to do fast rotate/downscale/desaturate
    ;; TODO: test jpegoptim/pngquant
    (for-each (lambda (path)
                (run! (mogrify -resize ,geometry -type GrayScale
                               -depth 8 -strip ,path)))
              (files-in flatten-dir))))

(define (create-archive!)
  (with-current-directory flatten-dir
    (lambda ()
      (run! (apack "../output.zip" ".")))))

(define (rename-archive! directory file digit title)
  (let* ((from (make-pathname work-dir "output.zip"))
         (new-name (if title
                       (make-pathname title (format "~a #~a.cbz" title digit))
                       (format "~a #~a.cbz" (pathname-file file) digit)))
         (to (make-pathname directory new-name)))
    (create-directory (pathname-directory to) 'parents)
    (move-file from to 'clobber)))

(define (process! directory file digit title optimize?)
  (create-directory unpack-dir 'parents)
  (create-directory trash-dir 'parents)
  (create-directory flatten-dir 'parents)
  (print "Extracting files...")
  (run! (aunpack -X ,unpack-dir ,file))
  (print "Unnesting inner archives...")
  (unnest-inner-archives!)
  (print "Purging non-image files...")
  (purge-non-image-files!)
  (print "Flattening image paths...")
  (flatten-image-paths!)
  (print "Ensuring portrait direction...")
  (ensure-portrait-direction!)
  (when optimize?
    (print "Optimizing images...")
    (optimize-images!))
  (print "Creating archive...")
  (create-archive!)
  (print "Renaming archive...")
  (rename-archive! directory file digit title)
  (print "Cleaning up...")
  (delete-directory work-dir 'recursive))

(define usage-hint "Usage: mcp [options] [file ...]\n")

;; TODO: test https://wiki.call-cc.org/eggref/5/optimism

(define options
  `((directory
     "Change output directory"
     (required #f)
     (value #t)
     (single-char #\C))
    (title
     "Title of the manga"
     (required #f)
     (value #t)
     (single-char #\t))
    (start
     "Start counting from i"
     (required #f)
     (value (required "i")
            (predicate ,string->number)
            (transformer ,string->number))
     (single-char #\s))
    (optimize
     "Optimize images"
     (required #f)
     (value #f)
     (single-char #\O))
    (help
     "Prints this help"
     (required #f)
     (value #f)
     (single-char #\h))))

(define (print-error . args)
  (with-output-to-port (current-error-port)
    (lambda () (apply print args))))

(define (main)
  (let* ((opts
          (condition-case
           (getopt-long (command-line-arguments) options)
           (e (exn)
              (print-error
               (format "Error: ~a: ~a\n"
                       ((condition-property-accessor 'exn 'message) e)
                       ((condition-property-accessor 'exn 'arguments) e))
               usage-hint (usage options)))))
         (help? (alist-ref 'help opts))
         (directory (or (alist-ref 'directory opts) "."))
         (title (alist-ref 'title opts))
         (start (or (alist-ref 'start opts) 1))
         (optimize? (alist-ref 'optimize opts))
         (files (alist-ref '@ opts)))
    (when help?
      (print usage-hint (usage options))
      (exit 0))
    (when (null? files)
      (print-error "No input file(s) specified\n" usage-hint)
      (exit 1))
    (check-leftover-files!)
    (let* ((file-count (sub1 (+ (length files) start)))
           (width (pad-width file-count)))
      (let loop ((files files)
                 (i 0))
        (when (pair? files)
          (let ((file (car files))
                (digit (left-pad (number->string (+ i start)) width #\0)))
            (printf "Processing file #~a (~a/~a): ~a\n"
                    digit (+ i start) file-count file)
            (process! directory file digit title optimize?)
            (loop (cdr files) (add1 i))))))))

(main)
