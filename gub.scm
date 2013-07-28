#!guile -s
!#

(add-to-load-path ".")

(use-modules (ncurses curses)
             (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 pretty-print)
             (ice-9 receive)
             (srfi srfi-39)
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
(define uri (string->uri "https://bugzilla.mediagraft.com:1443/xmlrpc.cgi"))

(let ((client (make-session connection-end/client))
      (sock   (open-socket-for-uri uri)) 
      (cred   (make-certificate-credentials)))

 (set-log-level! 0)
 (set-session-default-priority! client) 
 (set-certificate-credentials-x509-trust-file! cred "/etc/ssl/certs/ca-certificates.crt" x509-certificate-format/pem)
 (set-session-credentials! client cred) 

 (set-session-transport-fd! client (fileno sock)) 
 (handshake client) 

 (let* ((body  
          (string->utf8
            (call-with-output-string
              (lambda (p) (sxml->xml 
                            (sxmlrpc
                              (request 'User.login 
                                       (struct
                                         ('login    "TristanC@blinkbox.com")
                                         ('password "password")
                                         ('remember #t))))
                            p)))))
        (body-len (bytevector-length body))
        (port (session-record-port client))
        (req (build-request uri
                            #:method 'POST
                            #:port  port
                            #:headers `((content-type . (text/xml))
                                        (content-length . ,body-len))))) 

   (write-request-body
     (write-request req port)
     body) 
   ;; Flush port
   (force-output port) 
   ;; Read and return server response
   (let* ((resp (read-response port))
          (hds  (response-headers resp))
          (bod  (xmlrpc-response-params 
                  (xmlrpc-string->scm 
                    (utf8->string 
                      (read-response-body 
                        resp))))))

     (pretty-print hds)(newline)
     (pretty-print bod)(newline))
   
    

   (bye client close-request/rdwr)
   (close sock))) 


 



