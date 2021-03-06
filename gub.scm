#!guile -s
!#

(add-to-load-path ".")

(use-modules (ncurses curses)
             (srfi srfi-1)  ; list functions
             (srfi srfi-9)  ;
             (srfi srfi-11) ; let-values
             (ice-9 pretty-print)
             (ice-9 receive) (srfi srfi-39)
             (web uri)
             (web client)
             (web request)
             (web response)
             (xmlrpc)
             (gnutls)
             (sxml simple)
             (rnrs bytevectors))


(define *priority* "NORMAL")
(define *dh-bits* 1024)

;(define uri (string->uri "https://www.google.com/"))
(define uri (string->uri "https://bugzilla.blinkboxmusic.com/xmlrpc.cgi"))

(define *password* (make-parameter #f))

(load ".gubrc")

(define (make-https-session uri)
  (let* ((socket (open-socket-for-uri uri))
         (TCP_NODELAY 1)
         (TCP_CORK 3)
         (session (make-session connection-end/client)))
    ;; (set-log-level! 9)
    ;; (set-log-procedure!
    ;;  (lambda (level msg) (format #t "|<~d>| ~a" level msg)))

    ; This may need to be done in open-socket-for-uri
    (setsockopt socket IPPROTO_TCP TCP_NODELAY 1) 

    ;; Use the file descriptor that underlies SOCKET.
    (set-session-transport-fd! session (fileno socket))

    ;; Use the default settings.
    (set-session-priorities! session "NORMAL")
    ;(set-session-default-priority! session) 
    ;

    ;; Create anonymous credentials.
    (set-session-credentials! session
                              (make-anonymous-client-credentials))
    (set-session-credentials! session
                              (make-certificate-credentials))
    ;(set-certificate-credentials-x509-trust-file! cred "/etc/ssl/certs/ca-certificates.crt" x509-certificate-format/pem)

    ;; Perform the TLS handshake with the server.
    (handshake session)

    (values
      ; sender
      (lambda* (method body headers) 
               (let* ((port (session-record-port session))
                      (body  
                        (string->utf8
                          body))  
                      (body-len (bytevector-length body))
                      (req (build-request uri
                                          #:method method
                                          #:port port
                                          #:headers (append 
                                                      headers
                                                      `((content-length . ,body-len))))))
                 (write-request-body
                   (write-request req port)
                   body)
                 ;; Flush port
                 (force-output port) 

                 ;; Read and return server response
                 (let* ((resp (read-response port))
                        (bod  (read-response-body resp)))
                   (values resp bod)))) 

      ; close
      (lambda* ()
               (bye session close-request/rdwr)
               (close (session-record-port session))))))

(define* (make-xmlrpc-caller uri)
         (receive 
           (sender closer)
           (make-https-session uri) 
           (values 
             ; caller
             (lambda (req) 
               (let* ((reqbody  (call-with-output-string (lambda (p) (sxml->xml req p))))
                      (headers '((content-type . (text/xml)))))
                 (receive 
                   (response body)
                   (sender 'POST reqbody headers) 
                   (values 
                     response
                     
                     (xmlrpc-string->scm 
                       (utf8->string body))
                     ;(utf8->string body)
                     
                     ))))
             ; closer
             closer)))

(let-values (((caller closer) (make-xmlrpc-caller uri))) 
            
;            (receive 
;              (response body) 
;
;              (caller (sxmlrpc
;                        (request 'User.login 
;                                 (struct
;                                   ('login    "TristanC@blinkbox.com")
;                                   ('password ,(*password*))
;                                   ('remember #t))))) 
;
;              ;   (caller (sxmlrpc
;              ;             (request 'get_selectable_products
;              ;                      (struct
;              ;                        ('Bugzilla_login    "TristanC@blinkbox.com")
;              ;                        ('Bugzilla_password ,(*password*)))))) 
;
;              (pretty-print response)(newline)
;              (pretty-print body)(newline) ) 

            (receive 
              (response body) 

              ;(caller (sxmlrpc
              ;          (request 'User.login 
              ;                   (struct
              ;                     ('login    "TristanC@blinkbox.com")
              ;                     ('password ,(*password*))
              ;                     ('remember #t))))) 

              (caller (sxmlrpc
                        (request 'Product.get_selectable_products
                                 (struct
                                   ('Bugzilla_login    "TristanC@blinkbox.com")
                                   ('Bugzilla_password ,(*password*)))))) 

              (pretty-print response)(newline)
              (pretty-print body)(newline)) 

            (closer))
