#lang racket/base

(require racket/cmdline
	 racket/string
	 charterm
	 db
	 "input.rkt")

(define db-conn '())
(define db-table '())
(define db-row-to-consider '())

(define (call-db data-row srch limit) 
  (let/ec cursor-jump 
    (let cursor-loop
	((result  (call-db-with-current-input srch))
	 (cp data-row))
      (if (or (null? result) (= cp limit))
	  (cursor-jump)
	  (and (charterm-cursor 1 cp)
	       (charterm-display (car result))
	       (cursor-loop (cdr result) (+ cp 1)))))))



(define (call-db-with-current-input srch)
  (let* ((strn
	  (string-trim (bytes->string/utf-8 (%charterm:demo-input-bytes srch))))
	 (quer (query-rows db-conn
			   (string-append "select * from " db-table " where " db-row-to-consider "  like $1")
			   
			   (string-append "%" strn "%" ))))
    (map (lambda (x)
	   (string->bytes/utf-8 (vector-ref x 1)))
	 quer)))



(define (clear-sane col-count srch)
  (begin (charterm-clear-screen)
	 (charterm-cursor 1 1)
	 (charterm-inverse)
	 (charterm-display
	  (%charterm:string-pad-or-truncate " DB-CLI"
					    col-count))
	 (charterm-normal)
	 (charterm-inverse)
	 (charterm-display #"Search: ")
	 (charterm-normal)
	 (%charterm:demo-input-redraw srch)))

(define (print-table-list)
  (map (lambda (x)
	 (if (string? x)
	     (and (display x)
		  (newline))
	     (newline)))
       (table-list)))

(define (table-list)
  (let ((quer (query db-conn "SELECT name FROM sqlite_master WHERE type = 'table' ")))
    (map (lambda (x)
	   (vector-ref x 0))
	 (rows-result-rows quer))))

(define (schema-list)
  (let ((quer (query db-conn (string-append "PRAGMA table_info(" db-table ")"))))
    (map (lambda (x)
	   (string->bytes/utf-8 (vector-ref x 1)))
	 (rows-result-rows quer))))

(define (refresh-columns)
  (let/ec col-jump
    (let col-loop ((s (schema-list))
		   (cp 4))
      (if (null? s)
	  (col-jump)
	  (and (charterm-cursor 4 cp)
	       (charterm-display (car s))
	       (col-loop (cdr s) (+ cp 1)))))))

(define (db-setup)
  (let/ec setup-ec
    (let ((srch (%charterm:make-demo-input 10 2 18 #"")))
      (charterm-clear-screen)
      (charterm-inverse)
      (charterm-cursor 1 1)
      (charterm-display #"Search which column?: ")
      (charterm-normal)
      (begin
	(let loop-fast-next-key ()
	  (refresh-columns)
	  (let ((keyinfo (charterm-read-keyinfo #:timeout 1)))
	    (if keyinfo
		(let ((keycode (charterm-keyinfo-keycode keyinfo)))
		  (if (char? keycode)
		      (let ((key-num (char->integer keycode)))
			(if (<= 32 key-num 126)
			    (begin (%charterm:demo-input-insert-byte srch key-num)
				   (loop-fast-next-key))
			    (loop-fast-next-key)))
		  
		      (case keycode
			((backspace)
			 (%charterm:demo-input-backspace srch)
			 (loop-fast-next-key))
			((return)
			 (set! db-row-to-consider (bytes->string/utf-8 (%charterm:demo-input-bytes srch)))
			 (setup-ec)))))
			
		       (loop-fast-next-key))))))))



(provide db-cli)
(define (db-cli #:tty (tty #f)
		#:escape? (escape? #t))
  
  (let ((data-row 4)
	(srch (%charterm:make-demo-input 10 2 18 #"")))
    
    (with-charterm
     (let ((ct (current-charterm)))
       (let/ec done-ec
	       (db-setup)
	       (let loop-remember-read-screen-size ((last-read-col-count 0)
						    (last-read-row-count 0))
		 (let loop-maybe-check-screen-size ()
		   (let*-values (((read-col-count read-row-count)
				  (if (or (equal? 0 last-read-col-count)
					  (equal? 0 last-read-row-count)
					  (not (charterm-byte-ready?)))
				      (charterm-screen-size)
				      (values last-read-col-count
					      last-read-row-count)))
				 ((read-screen-size? col-count row-count)
				  (if (and read-col-count read-row-count)
				      (values #t
					      read-col-count
					      read-row-count)
				      (values #f
					      (or read-col-count 80)
					      (or read-row-count 24))))
				 ((read-screen-size-changed?)
				  (not (and (equal? read-col-count
						    last-read-col-count)
					    (equal? read-row-count
						    last-read-row-count)))))
		     (if read-screen-size-changed?
			 (begin (charterm-clear-screen)
				(charterm-cursor 1 1)
				(charterm-inverse)
				(charterm-display
				 (%charterm:string-pad-or-truncate " DB-CLI"
								   col-count))
				(charterm-normal)
				(charterm-inverse)
				(charterm-display #"Search: ")
				(charterm-normal)
				(%charterm:demo-input-redraw srch)
				(loop-remember-read-screen-size read-col-count
								read-row-count))
			 (begin
			   (let loop-fast-next-key ()
			     (let ((keyinfo (charterm-read-keyinfo #:timeout 1)))
			       (if keyinfo
				   (let ((keycode (charterm-keyinfo-keycode keyinfo)))
				     (if (char? keycode)
					 (let ((key-num (char->integer keycode)))
					   (if (<= 32 key-num 126)
					       (begin (%charterm:demo-input-insert-byte srch key-num)
						      (clear-sane col-count srch)
						      (call-db data-row srch row-count)
						      (loop-fast-next-key))
					       (loop-fast-next-key)))
					 
					 (case keycode
					   ((backspace)
					    (%charterm:demo-input-backspace srch)
					    (clear-sane col-count srch)
					    (call-db data-row srch row-count)
					    (loop-fast-next-key))
					   ((return)
					    (clear-sane col-count srch)
					    (call-db data-row srch row-count)
					    (loop-fast-next-key))
					   ((escape)
					    (if escape?
						(begin
						  (charterm-clear-screen)
						  (charterm-display
						   "You have left DB-CLI.")
						  (charterm-newline)
						  (done-ec))
						(loop-fast-next-key)))
					   (else (loop-fast-next-key)))))
				   (begin
				     (loop-maybe-check-screen-size)))))))))))))))

(provide main)
(define (main . args)
  (let ((tty     #f)
	(escape? #t))
    (command-line #:program "(DB-CLI)"
		  #:once-each
		  (("--tty") arg "The TTY to use (default: /dev/tty)." (set! tty arg))
	        
		  (("--escape"    "-e") "Esc key quits program (default)." (set! escape? #t))
		  (("--no-escape" "-n") "Esc key does not quit program."   (set! escape? #f))
		  (("--print-schema" "-p") "Print Schema" (set! db-table "schema"))
		  (("--database" "-d") arg "Database to access." (set! db-conn
								   (sqlite3-connect #:database arg)))
		  #:multi
		  (("--table" "-t") table "Table to access." (set! db-table table)))
		  
    
    (check-start  #:tty     tty
	     #:escape? escape?)))

(define (check-start #:tty tty #:escape? escape)
  (if (string=? db-table "schema")
      (print-table-list)
      (db-cli #:tty tty #:escape? escape)))
      
(main)
