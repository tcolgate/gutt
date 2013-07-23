#!guile -s
!#

(use-modules (ncurses curses)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-39))

;; Program Begins
(define stdscr (initscr))		; Start curses

(define *logport*  (make-parameter (open-file ".gutt.log" "w0")))
(define *sidebarw* (make-parameter 40))


(define (quitGutt)
  (endwin)
  (exit 0))

(define-record-type cgonad:window 
                   (make-cgonad:window name draw) 
                   cgonad:window?
                   (name cgonad:window:name set-cgonad:window:name!)
                   (ncurseswin cgonad:window:ncurseswin set-cgonad:window:ncurseswin!)
                   (draw cgonad:window:draw set-cgonad:window:draw!))

(define *testlines* 
  '("First line to draw"
    "Second line to draw"
    "Third line to draw"))

(define *windowlist*
  (list 
    (let* ((name "Sidebar"))
      (make-cgonad:window 
        name
        (lambda (win) 
          (box win (acs-vline) (acs-hline))
          (addstr win name #:x 1 #:y 0)
          (noutrefresh win)))) 

    (let* ((name "Main"))
      (make-cgonad:window 
        name
        (lambda (win) 
          (box win (acs-vline) (acs-hline))
          (addstr win name #:x 1 #:y 0)
          (let loop ((xs *testlines*)
                     (y 1)) 
            (addstr win (car xs) #:x 1 #:y y)
            (if (not (eq? '() (cdr xs)))
              (loop (cdr xs) (+ y 1))))
          (noutrefresh win))))))


(define (full-layout)
  (set-cgonad:window:ncurseswin! 
    (car *windowlist*)  
    (newwin 0 0 0 0 ))
  (map 
    (lambda (w)
      (set-cgonad:window:ncurseswin! 
        w  
        #f))
    (cdr *windowlist*)))

(define *pricnt*       (make-parameter 0))
(define *changedelta*  (make-parameter 0.1))
(define *prisecratio*  (make-parameter 0.5))

(define (wide-layout)
  (let* ((cnt (min (length *windowlist*) (*pricnt*)))
         (mns (take *windowlist* cnt))
         (scs (drop *windowlist* cnt))
         (rat (cond 
                ((eq? mns '()) 0)
                ((eq? scs '()) 1)
                (else (*prisecratio*))))
         (mnh (inexact->exact (round (* (lines) rat))))
         (sch (inexact->exact (round (- (lines) mnh))))
         (prc (length mns))
         (scc (length scs)))
   (fold
     (lambda (w x)
       (set-cgonad:window:ncurseswin! 
         w
         (newwin mnh (inexact->exact (round (/ (cols) prc))) 0 x))
       (+ x (inexact->exact (round (/ (cols) prc)))))
     0
     mns) 
  (fold
     (lambda (w x)
       (set-cgonad:window:ncurseswin! 
         w
         (newwin sch (inexact->exact (round (/ (cols) scc))) mnh x))
       (+ x (inexact->exact (round (/ (cols) scc)))))
     0
     scs)))

(define (tall-layout)
  (let* ((cnt (min (length *windowlist*) (*pricnt*)))
         (mns (take *windowlist* cnt))
         (scs (drop *windowlist* cnt))
         (rat (cond 
                ((eq? mns '()) 0)
                ((eq? scs '()) 1)
                (else (*prisecratio*))))
         (mnw (inexact->exact (round (* (cols) rat))))
         (scw (inexact->exact (round (- (cols) mnw))))
         (prc (length mns))
         (scc (length scs)))
   (fold
     (lambda (w y)
       (set-cgonad:window:ncurseswin! 
         w
         (newwin (inexact->exact (round (/ (lines) prc))) mnw y 0))
       (+ y (inexact->exact (round (/ (lines) prc)))))
     0
     mns) 
  (fold
     (lambda (w y)
       (set-cgonad:window:ncurseswin! 
         w
         (newwin (inexact->exact (round (/ (lines) scc))) scw y mnw))
       (+ y (inexact->exact (round (/ (lines) scc)))))
     0
     scs)))


;(define (relay)
;  (let* ((sb (car *windowlist*))
;         (mn (cadr *windowlist*)))
;    (set-cgonad:window:ncurseswin! 
;      sb
;      (newwin (lines) (*sidebarw*) 0 0)) 
;    (set-cgonad:window:ncurseswin! 
;      mn
;      (newwin (lines) 0 0 (*sidebarw*)))))

(define *currlayout* (make-parameter tall-layout))

(define (relay)
  ((*currlayout*)))

(define (redraw)
  (endwin)
  (noutrefresh stdscr)
  (map 
    (lambda (win)
      (let ((cwin (cgonad:window:ncurseswin win)))
       (if (not (eq? cwin #f))
         (begin
           (clear cwin)
           ((cgonad:window:draw win) cwin)))))
    *windowlist*)
  (doupdate))

(define (main) 
  (cbreak!)				; Line buffering disabled
  (noecho!)
  (curs-set 0)
  (keypad! stdscr #t)			; Check for function keys
  (mousemask ALL_MOUSE_EVENTS)
  (sigaction SIGINT 
           (lambda (arg)
             (addstr stdscr "quitting")
             (refresh stdscr)
             (sleep 10)
             (quitGutt)))
  (relay)
  (redraw)

  (let loop ((ch (getch stdscr)))
   (cond
     ((eqv? ch KEY_RESIZE)
      (relay)
      (redraw)
      (loop (getch stdscr))) 
     ((eqv? ch #\q)
      quitGutt)
     ((eqv? ch #\-)
      (*prisecratio* (max 0.0 (- (*prisecratio*) (*changedelta*))))
      (relay)
      (redraw)
      (loop (getch stdscr)))
     ((eqv? ch #\+)
      (*prisecratio* (min 1.0 (+ (*prisecratio*) (*changedelta*))))
      (relay)
      (redraw)
      (loop (getch stdscr)) )
     (else
      (loop (getch stdscr)))))

  (quitGutt))

(define *windowlist*
  (list 
    (let* ((name "Sidebar"))
      (make-cgonad:window 
        name
        (lambda (win) 
          (box win (acs-vline) (acs-hline))
          (addstr win name #:x 1 #:y 0)
          (noutrefresh win)))) 

    (let* ((name "Main"))
      (make-cgonad:window 
        name
        (lambda (win) 
          (box win (acs-vline) (acs-hline))
          (addstr win name #:x 1 #:y 0)
          (let loop ((xs *testlines*)
                     (y 1)) 
            (addstr win (car xs) #:x 1 #:y y)
            (if (not (eq? '() (cdr xs)))
              (loop (cdr xs) (+ y 1))))
          (noutrefresh win))))))

(parameterize ((current-warning-port (*logport*)))
  (main))
