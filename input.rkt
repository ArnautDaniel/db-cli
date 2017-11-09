;;;proper credit to http://www.neilvandyke.org/racket/charterm/
;;;The majority of this is his demo code for charterm
;;;Used it because it provides initial support for input boxes
#lang racket/base
(require charterm)

(provide %charterm:string-pad-or-truncate)
(define (%charterm:string-pad-or-truncate str width)
  (let ((len (string-length str)))
    (cond ((= len width) str)
          ((< len width) (string-append str (make-string (- width len) #\space)))
          (else (substring str 0 width)))))

(provide %charterm:bytes-pad-or-truncate)
(define (%charterm:bytes-pad-or-truncate bstr width)
  (let ((len (bytes-length bstr)))
    (cond ((= len width) bstr)
          ((< len width)
           (let ((new-bstr (make-bytes width 32)))
             (bytes-copy! new-bstr 0 bstr)
             new-bstr))
          (else (subbytes bstr 0 width)))))
(provide %charterm:demo-input
	 %charterm:demo-input-bytes)
(define-struct %charterm:demo-input
  (x y width bytes used cursor)
  #:mutable)

(provide %charterm:make-demo-input)
(define (%charterm:make-demo-input x y width bstr)
  (let ((new-bstr (%charterm:bytes-pad-or-truncate bstr width))
        (used     (min (bytes-length bstr) width)))
    (make-%charterm:demo-input x
                               y
                               width
                               new-bstr
                               used
                               used)))

(provide %charterm:demo-input-redraw)
(define (%charterm:demo-input-redraw di)
  (charterm-cursor (%charterm:demo-input-x di)
                   (%charterm:demo-input-y di))
  (charterm-normal)
  (charterm-underline)
  (charterm-display (%charterm:demo-input-bytes di)
                    #:width (%charterm:demo-input-width di))
  (charterm-normal))

(provide %charterm:demo-input-put-cursor)
(define (%charterm:demo-input-put-cursor di)
  ;; Note: Commented-out debugging code:
  ;;
  ;; (and #t
  ;;      (begin (charterm-normal)
  ;;             (charterm-cursor (+ (%charterm:demo-input-x     di)
  ;;                                   (%charterm:demo-input-width di)
  ;;                                   1)
  ;;                                (%charterm:demo-input-y di))
  ;;             (charterm-display #" cursor: "
  ;;                               (%charterm:demo-input-cursor di)
  ;;                               #" used: "
  ;;                               (%charterm:demo-input-used di))
  ;;             (charterm-clear-line-right)))
  (charterm-cursor (+ (%charterm:demo-input-x      di)
                      (%charterm:demo-input-cursor di))
                   (%charterm:demo-input-y di)))

(define (%charterm:demo-input-cursor-left di)
  (let ((cursor (%charterm:demo-input-cursor di)))
    (if (zero? cursor)
        (begin (charterm-bell)
               (%charterm:demo-input-put-cursor di))
        (begin (set-%charterm:demo-input-cursor! di (- cursor 1))
               (%charterm:demo-input-put-cursor di)))))

(define (%charterm:demo-input-cursor-right di)
  (let ((cursor (%charterm:demo-input-cursor di)))
    (if (= cursor (%charterm:demo-input-used di))
        (begin (charterm-bell)
               (%charterm:demo-input-put-cursor di))
        (begin (set-%charterm:demo-input-cursor! di (+ cursor 1))
               (%charterm:demo-input-put-cursor di)))))

(provide %charterm:demo-input-backspace)
(define (%charterm:demo-input-backspace di)
  (let ((cursor (%charterm:demo-input-cursor di)))
    (if (zero? cursor)
        (begin (charterm-bell)
               (%charterm:demo-input-put-cursor di))
        (let ((bstr (%charterm:demo-input-bytes di))
              (used (%charterm:demo-input-used di)))
          ;; TODO: test beginning/end of buffer, of used, of width
          (bytes-copy! bstr (- cursor 1) bstr cursor used)
          (bytes-set! bstr (- used 1) 32)
          (set-%charterm:demo-input-used! di (- used 1))
          (set-%charterm:demo-input-cursor! di (- cursor 1))
          (%charterm:demo-input-redraw di)
          (%charterm:demo-input-put-cursor di)))))

(define (%charterm:demo-input-delete di)
  (let ((cursor (%charterm:demo-input-cursor di))
        (used   (%charterm:demo-input-used   di)))
    (if (= cursor used)
        (begin (charterm-bell)
               (%charterm:demo-input-put-cursor di))
        (let ((bstr (%charterm:demo-input-bytes di)))
          (or (= cursor used)
              (bytes-copy! bstr cursor bstr (+ 1 cursor) used))
          (bytes-set! bstr (- used 1) 32)
          (set-%charterm:demo-input-used! di (- used 1))
          (%charterm:demo-input-redraw     di)
          (%charterm:demo-input-put-cursor di)))))

(provide %charterm:demo-input-insert-byte)
(define (%charterm:demo-input-insert-byte di new-byte)
  (let ((used  (%charterm:demo-input-used  di))
        (width (%charterm:demo-input-width di)))
    (if (= used width)
        (begin (charterm-bell)
               (%charterm:demo-input-put-cursor di))
        (let ((bstr   (%charterm:demo-input-bytes  di))
              (cursor (%charterm:demo-input-cursor di)))
          (or (= cursor used)
              (bytes-copy! bstr (+ cursor 1) bstr cursor used))
          (bytes-set! bstr cursor new-byte)
          (set-%charterm:demo-input-used! di (+ 1 used))
          (set-%charterm:demo-input-cursor! di (+ cursor 1))
          (%charterm:demo-input-redraw di)
          (%charterm:demo-input-put-cursor di)))))
