#!guile -s
!#

(add-to-load-path ".")

(use-modules (ncurses curses)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-39)
             (web uri)
             (web client)
             (xmlrpc)
             (gnutls))

(define uri (string->uri "https://google.com:443/"))

(let ((client (make-session connection-end/client)))

 ;; Use the default settings.
 (set-session-default-priority! client) 

 ;; Don't use certificate-based authentication.
 (set-session-certificate-type-priority! client '()) 

 ;; Request the "anonymous Diffie-Hellman" key exchange method.
 (set-session-kx-priority! client (list kx/anon-dh)) 
 
 ;; Specify the underlying socket.
 (set-session-transport-fd! client (fileno (open-socket-for-uri uri))) 
 
 ;; Create anonymous credentials.
 (set-session-credentials! client
                           (make-anonymous-client-credentials)) 
 ;; Perform the TLS handshake with the server.
 (handshake client) 

 ;; Send data over the TLS record layer.
 (display  (http-get 
   uri
   #:port (session-record-port client)
   #:keep-alive? #t )) 

 ;; Terminate the TLS session.
 (bye client close-request/rdwr)) 


 



