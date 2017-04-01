;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :irc-client
  (:use :cl :sys.net)
  (:export #:spawn))

(in-package :irc-client)

(defvar *irc-history* (make-instance 'mezzano.line-editor:history-table))

(defparameter *numeric-replies*
  '((401 :err-no-such-nick)
    (402 :err-no-such-server)
    (403 :err-no-such-channel)
    (404 :err-cannot-send-to-channel)
    (405 :err-too-many-channels)
    (406 :err-was-no-such-nick)
    (407 :err-too-many-targets)
    (409 :err-no-origin)
    (411 :err-no-recipient)
    (412 :err-no-text-to-send)
    (413 :err-no-toplevel)
    (414 :err-wild-toplevel)
    (421 :err-unknown-command)
    (422 :err-no-motd)
    (423 :err-no-admin-info)
    (424 :err-file-error)
    (431 :err-no-nickname-given)
    (432 :err-erroneus-nickname)
    (433 :err-nickname-in-use)
    (436 :err-nick-collision)
    (441 :err-user-not-in-channel)
    (442 :err-not-on-channel)
    (443 :err-user-on-channel)
    (444 :err-no-login)
    (445 :err-summon-disabled)
    (446 :err-users-disabled)
    (451 :err-not-registered)
    (461 :err-need-more-params)
    (462 :err-already-registred)
    (463 :err-no-perm-for-host)
    (464 :err-password-mismatch)
    (465 :err-youre-banned-creep)
    (467 :err-key-set)
    (471 :err-channel-is-full)
    (472 :err-unknown-mode)
    (473 :err-invite-only-chan)
    (474 :err-banned-from-chan)
    (475 :err-bad-channel-key)
    (481 :err-no-privileges)
    (482 :err-chanop-privs-needed)
    (483 :err-cant-kill-server)
    (491 :err-no-oper-host)
    (501 :err-umode-unknown-flag)
    (502 :err-users-dont-match)
    (300 :rpl-none)
    (302 :rpl-userhost)
    (303 :rpl-ison)
    (301 :rpl-away)
    (305 :rpl-unaway)
    (306 :rpl-nowaway)
    (311 :rpl-whoisuser)
    (312 :rpl-whoisserver)
    (313 :rpl-whoisoperator)
    (317 :rpl-whoisidle)
    (318 :rpl-endofwhois)
    (319 :rpl-whoischannels)
    (314 :rpl-whowasuser)
    (369 :rpl-endofwhowas)
    (321 :rpl-liststart)
    (322 :rpl-list)
    (323 :rpl-listend)
    (324 :rpl-channelmodeis)
    (331 :rpl-notopic)
    (332 :rpl-topic)
    (333 :rpl-topic-time)
    (341 :rpl-inviting)
    (342 :rpl-summoning)
    (351 :rpl-version)
    (352 :rpl-whoreply)
    (315 :rpl-endofwho)
    (353 :rpl-namreply)
    (366 :rpl-endofnames)
    (364 :rpl-links)
    (365 :rpl-endoflinks)
    (367 :rpl-banlist)
    (368 :rpl-endofbanlist)
    (371 :rpl-info)
    (374 :rpl-endofinfo)
    (375 :rpl-motdstart)
    (372 :rpl-motd)
    (376 :rpl-endofmotd)
    (381 :rpl-youreoper)
    (382 :rpl-rehashing)
    (391 :rpl-time)
    (392 :rpl-usersstart)
    (393 :rpl-users)
    (394 :rpl-endofusers)
    (395 :rpl-nousers)
    (200 :rpl-tracelink)
    (201 :rpl-traceconnecting)
    (202 :rpl-tracehandshake)
    (203 :rpl-traceunknown)
    (204 :rpl-traceoperator)
    (205 :rpl-traceuser)
    (206 :rpl-traceserver)
    (208 :rpl-tracenewtype)
    (261 :rpl-tracelog)
    (211 :rpl-statslinkinfo)
    (212 :rpl-statscommands)
    (213 :rpl-statscline)
    (214 :rpl-statsnline)
    (215 :rpl-statsiline)
    (216 :rpl-statskline)
    (218 :rpl-statsyline)
    (219 :rpl-endofstats)
    (241 :rpl-statslline)
    (242 :rpl-statsuptime)
    (243 :rpl-statsoline)
    (244 :rpl-statshline)
    (221 :rpl-umodeis)
    (251 :rpl-luserclient)
    (252 :rpl-luserop)
    (253 :rpl-luserunknown)
    (254 :rpl-luserchannels)
    (255 :rpl-luserme)
    (256 :rpl-adminme)
    (257 :rpl-adminloc1)
    (258 :rpl-adminloc2)
    (259 :rpl-adminemail)))

(defun decode-command (line)
  "Explode a line into (prefix command parameters...)."
  ;; <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
  ;; <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
  ;; <command>  ::= <letter> { <letter> } | <number> <number> <number>
  ;; <SPACE>    ::= ' ' { ' ' }
  ;; <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
  (let ((prefix nil)
        (offset 0)
        (command nil)
        (parameters '()))
    (when (and (not (zerop (length line)))
               (eql (char line 0) #\:))
      ;; Prefix present, read up to a space.
      (do () ((or (>= offset (length line))
                  (eql (char line offset) #\Space)))
        (incf offset))
      (setf prefix (subseq line 1 offset)))
    ;; Eat leading spaces.
    (do () ((or (>= offset (length line))
                (not (eql (char line offset) #\Space))))
      (incf offset))
    ;; Parse a command, reading until space or the end.
    (let ((start offset))
      (do () ((or (>= offset (length line))
                  (eql (char line offset) #\Space)))
        (incf offset))
      (setf command (subseq line start offset)))
    (when (and (= (length command) 3)
               (every (lambda (x) (find x "1234567890")) command))
      (setf command (parse-integer command))
      (setf command (or (second (assoc command *numeric-replies*))
                        command)))
    ;; Read parameters.
    (loop
       ;; Eat leading spaces.
       (do () ((or (>= offset (length line))
                   (not (eql (char line offset) #\Space))))
         (incf offset))
       (cond ((>= offset (length line)) (return))
             ((eql (char line offset) #\:)
              (push (subseq line (1+ offset)) parameters)
              (return))
             (t (let ((start offset))
                  (do () ((or (>= offset (length line))
                              (eql (char line offset) #\Space)))
                    (incf offset))
                  (push (subseq line start offset) parameters)))))
    (values prefix command (nreverse parameters))))

(defun parse-command (line)
  (cond ((and (>= (length line) 1)
              (eql (char line 0) #\/)
              (not (and (>= (length line) 2)
                        (eql (char line 1) #\/))))
         (let ((command-end nil)
               (rest-start nil)
               (rest-end nil))
           (dotimes (i (length line))
             (when (eql (char line i) #\Space)
               (setf command-end i
                     rest-start i)
               (return)))
           (when rest-start
             ;; Eat leading spaces.
             (do () ((or (>= rest-start (length line))
                         (not (eql (char line rest-start) #\Space))))
               (incf rest-start)))
           (values (subseq line 1 command-end)
                   (subseq line (or rest-start (length line)) rest-end))))
        (t (values "say" line))))

(defvar *command-table* (make-hash-table :test 'equal))

(defmacro define-server-command (name (state . lambda-list) &body body)
  (let ((args (gensym)))
    `(setf (gethash ,(if (and (symbolp name) (not (keywordp name)))
                         (symbol-name name)
                         name)
                    *command-table*)
           (lambda (,state ,(first lambda-list) ,args)
             (declare (sys.int::lambda-name (irc-command ,name)))
             (destructuring-bind ,(rest lambda-list) ,args
               ,@body)))))

(define-server-command privmsg (irc from channel message)
  ;; ^AACTION [msg]^A is a /me command.
  (cond ((and (>= (length message) 9)
              (eql (char message 0) (code-char #x01))
              (eql (char message (1- (length message))) (code-char #x01))
              (string= "ACTION " message :start2 1 :end2 8))
         (format (display-pane irc) "~&[~A]* ~A ~A" channel from
                 (subseq message 8 (1- (length message)))))
        (t (format (display-pane irc) "~&[~A]<~A> ~A" channel from message)
           #+(or)
           (when (and (>= (length message) 6)
                      (string-equal "!eval " message :end2 6))
             (let ((msg (handler-case
                            (with-output-to-string (*standard-output*)
                              (eval (read-from-string message t nil :start 6)))
                          (error (condition)
                            (format nil "~A" condition)))))
               (setf msg (substitute #\Space #\Newline msg))
               (when (> (length msg) 400)
                 (setf msg (subseq msg 0 400))
                 (setf msg (concatenate 'string msg "...")))
               (buffered-format (irc-connection irc) "PRIVMSG ~A :~A~%"
                                (or channel from) msg))))))


(define-server-command ping (irc from message)
  (buffered-format (irc-connection irc) "PONG :~A~%" message))

(defun parse-origin (origin)
  "Return the nick, ident and host parts of a nick!ident@host name.
If ORIGIN is a server name, then only the host is valid. Nick and ident will be false."
  (let ((bang (position #\! origin))
        (at (position #\@ origin)))
    (cond ((and bang at)
           (values (subseq origin 0 bang)
                   (subseq origin (1+ bang) at)
                   (subseq origin (1+ at))))
          ((and (not bang) (not at))
           (values nil
                   nil
                   origin))
          (t (error "Unknown origin form ~S" origin)))))

(define-server-command nick (irc from message)
  (multiple-value-bind (nick ident host)
      (parse-origin from)
    (cond ((and nick ident
                (string= nick (nickname irc)))
           (format (display-pane irc) "~&-!- Nickname changed to ~A." message)
           (setf (nickname irc) message))
          ((and nick ident)
           (format (display-pane irc) "~&-!- ~A is now known as ~A." nick message))
          (t (format (display-pane irc) "~&:~A NICK ~A" from message)))))

(defvar *known-servers*
  '((:freenode "chat.freenode.net" 6667))
  "A list of known/named IRC servers.")

(defun resolve-server-name (name)
  (let ((known (assoc name *known-servers* :key 'symbol-name :test 'string-equal)))
    (cond (known
           (values (second known) (third known)))
          (t
           (let* ((colon (position #\: name))
                  (server (if colon
                              (subseq name 0 colon)
                              name))
                  (port (if (and colon (not (eql (1+ colon) (length name))))
                            (parse-integer name :start (1+ colon))
                            6667)))
             (values server port))))))

(defclass server-disconnect-event ()
  ())

(defclass server-line-event ()
  ((%line :initarg :line :reader line)))

(defun irc-receive (irc)
  (let ((connection (irc-connection irc))
        (fifo (fifo irc)))
    (loop
       (let ((line (read-line connection nil)))
         (when (not line)
           (mezzano.supervisor:fifo-push (make-instance 'server-disconnect-event) fifo)
           (return))
         (mezzano.supervisor:fifo-push (make-instance 'server-line-event :line line) fifo)))))

(defvar *top-level-commands* (make-hash-table :test 'equal))
(defvar *top-level-command-doc* (make-hash-table :test 'equal))

(defmacro define-command (name (irc text) docstring &body body)
  `(progn
     (setf (gethash ',(string-upcase (string name))
                    *top-level-command-doc*)
           ',docstring)
     (setf (gethash ',(string-upcase (string name))
                    *top-level-commands*)
           (lambda (,irc ,text)
             (declare (sys.int::lambda-name (irc-command ,name)))
             ,@body))))

(define-command quit (irc text)
  "QUIT [message]
  Disconnect and close IRC."
  (when (irc-connection irc)
    (buffered-format (irc-connection irc) "QUIT :~A~%" text))
  (throw 'quit nil))

(define-command raw (irc text)
  "RAW <text>
  Send TEXT directly to the server without parsing."
  (when (irc-connection irc)
    (write-string text (irc-connection irc))
    (terpri (irc-connection irc))))

(define-command eval (irc text)
  "EVAL <code>
  Read and evaluate CODE, printing the result in the display pane."
  (let ((*standard-output* (display-pane irc)))
    (format t "~&[eval] ~A~%" text)
    (eval (read-from-string text))
    (fresh-line)))

(define-command say (irc text)
  "SAY <text>
  Send a message to the current channel."
  (cond ((and (irc-connection irc) (current-channel irc))
         (format (display-pane irc) "~&[~A]<~A> ~A" (current-channel irc) (nickname irc) text)
         (buffered-format (irc-connection irc) "PRIVMSG ~A :~A~%"
                          (current-channel irc) text))
        (t (error "Not connected or not joined to a channel."))))

(define-command me (irc text)
  "ACTION <text>
  Send a CTCP ACTION to the current channel."
  (cond ((and (irc-connection irc) (current-channel irc))
         (format (display-pane irc) "~&[~A]* ~A ~A" (current-channel irc) (nickname irc) text)
         (buffered-format (irc-connection irc) "PRIVMSG ~A :~AACTION ~A~A~%"
                          (current-channel irc) (code-char 1) text (code-char 1)))
        (t (error "Not connected or not joined to a channel."))))

(define-command nick (irc text)
  "NICK [nickname]
  Set, change or view your nickname."
  (cond ((zerop (length text))
         (format (display-pane irc) "~&Your nickname is ~S." (nickname irc)))
        ((irc-connection irc)
         ;; Connected, let the server drive the nickname change.
         (buffered-format (irc-connection irc) "NICK ~A~%" text))
        (t (format (display-pane irc) "~&Nickname changed to ~A." text)
           (setf (nickname irc) text))))

(define-command connect (irc text)
  "CONNECT <server or address>
  Connect to a server. Addresses use the address:port format, with port defaulting to 6667."
  (cond ((zerop (length text))
         (format (display-pane irc) "~&Known servers:")
         (loop for (name address port) in *known-servers*
            do (format (display-pane irc) "~&  ~:(~A~)  ~A:~D" name address port)))
        ((not (nickname irc))
         (error "No nickname set. Use /nick to set a nickname before connecting."))
        ((irc-connection irc)
         (error "Already connected to ~S." (irc-connection irc)))
        (t (multiple-value-bind (address port)
               (resolve-server-name text)
             (format (display-pane irc) "~&Connecting to ~A (~A:~A)." text address port)
             (setf (mezzano.gui.widgets:frame-title (frame irc)) (format nil "IRC - ~A" text))
             (mezzano.gui.widgets:draw-frame (frame irc))
             (mezzano.gui.compositor:damage-window (window irc)
                                                   0 0
                                                   (mezzano.gui.compositor:width (window irc))
                                                   (mezzano.gui.compositor:height (window irc)))
             (setf (irc-connection irc) (mezzano.network.tcp:tcp-stream-connect address port)
                   (receive-thread irc) (mezzano.supervisor:make-thread (lambda () (irc-receive irc))
                                                                        :name "IRC receive"))
             (buffered-format (irc-connection irc) "USER ~A hostname servername :~A~%" (nickname irc) (nickname irc))
             (buffered-format (irc-connection irc) "NICK ~A~%" (nickname irc))))))

(define-command disconnect (irc text)
  "DISCONNECT [text]
  Close the current connect."
  (cond ((irc-connection irc)
         (buffered-format (irc-connection irc) "QUIT :~A~%" text)
         (close (irc-connection irc)))
        (t (error "Not connected."))))

(define-command join (irc text)
  "JOIN <channel>
  Join a channel."
  (cond ((find text (joined-channels irc) :test 'string-equal)
         (error "Already joined to channel ~A." text))
        ((irc-connection irc)
         (buffered-format (irc-connection irc) "JOIN ~A~%" text)
         (push text (joined-channels irc))
         (unless (current-channel irc)
           (setf (current-channel irc) text)))
        (t (error "Not connected."))))

(define-command chan (irc text)
  "CHAN <channel>
  Switch to a channel you are joined to."
  (when (irc-connection irc)
    (if (find text (joined-channels irc) :test 'string-equal)
        (setf (current-channel irc) text)
        (error "Not joined to channel ~A." text))))

(define-command part (irc text)
  "PART [message]
  Leave the current channel."
  (when (and (irc-connection irc) (current-channel irc))
    (buffered-format (irc-connection irc) "PART ~A :~A~%" (current-channel irc) text)
    (setf (joined-channels irc) (remove (current-channel irc) (joined-channels irc)))
    (setf (current-channel irc) (first (joined-channels irc)))))

(define-command help (irc text)
  "HELP
  Show help on commands."
  (format (display-pane irc) "~&Available commands:")
  (maphash (lambda (name help)
             (declare (ignore name))
             (format (display-pane irc) "~&/~A" help))
           *top-level-command-doc*)
  (format (display-pane irc) "~&To connect, first set your nickname with the NICK command, then connect to a server with CONNECT.")
  (format (display-pane irc) "~&Known servers, use with the CONNECT command instead of an address:")
  (loop for (name address port) in *known-servers*
     do (format (display-pane irc) "~&  ~:(~A~)  ~A:~D" name address port)))

(defclass irc-client ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%frame :initarg :frame :reader frame)
   (%font :initarg :font :reader font)
   (%display-pane :initarg :display-pane :reader display-pane)
   (%input-pane :initarg :input-pane :reader input-pane)
   (%current-channel :initarg :current-channel :accessor current-channel)
   (%joined-channels :initarg :joined-channels :accessor joined-channels)
   (%nickname :initarg :nickname :accessor nickname)
   (%connection :initarg :connection :accessor irc-connection)
   (%receive-thread :initarg :receive-thread :accessor receive-thread))
  (:default-initargs :current-channel nil :joined-channels '() :nickname nil :connection nil))

(defclass irc-input-pane (mezzano.line-editor:line-edit-mixin
                          sys.gray:fundamental-character-input-stream
                          mezzano.gui.widgets:text-widget)
  ((%irc :initarg :irc :reader irc)))

(defmethod sys.gray:stream-read-char ((stream irc-input-pane))
  (let* ((irc (irc stream))
         (fifo (fifo irc)))
    (unwind-protect
         (catch 'next-character
           (setf (mezzano.gui.widgets:cursor-visible stream) t)
           (loop
              (dispatch-event irc (mezzano.supervisor:fifo-pop fifo))))
      (setf (mezzano.gui.widgets:cursor-visible stream) nil))))

(defun reset-input (irc)
  (mezzano.gui.widgets:reset (input-pane irc))
  (format (input-pane irc) "~A] " (or (current-channel irc) "")))

(defgeneric dispatch-event (irc event)
  (:method (irc event)))

(defmethod dispatch-event (irc (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame irc)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame irc)))

(defmethod dispatch-event (irc (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame irc) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (irc (event mezzano.gui.compositor:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (irc (event mezzano.gui.compositor:quit-event))
  (throw 'quit nil))

(defmethod dispatch-event (irc (event mezzano.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzano.gui.compositor:key-releasep event))
    (throw 'next-character
      (if (mezzano.gui.compositor:key-modifier-state event)
          ;; Force character to uppercase when a modifier key is active, gets
          ;; around weirdness in how character names are processed.
          ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
          (sys.int::make-character (char-code (char-upcase (mezzano.gui.compositor:key-key event)))
                                   :control (find :control (mezzano.gui.compositor:key-modifier-state event))
                                   :meta (find :meta (mezzano.gui.compositor:key-modifier-state event))
                                   :super (find :super (mezzano.gui.compositor:key-modifier-state event))
                                   :hyper (find :hyper (mezzano.gui.compositor:key-modifier-state event)))
          (mezzano.gui.compositor:key-key event)))))

(defmethod dispatch-event (irc (event server-disconnect-event))
  (format (display-pane irc) "~&Disconnected from server.")
  (close (irc-connection irc))
  (setf (irc-connection irc) nil
        (receive-thread irc) nil)
  (setf (mezzano.gui.widgets:frame-title (frame irc)) (format nil "IRC"))
  (mezzano.gui.widgets:draw-frame (frame irc))
  (mezzano.gui.compositor:damage-window (window irc)
                                        0 0
                                        (mezzano.gui.compositor:width (window irc))
                                        (mezzano.gui.compositor:height (window irc))))

(defmethod dispatch-event (irc (event server-line-event))
  (let ((line (line event)))
    (multiple-value-bind (prefix command parameters)
        (decode-command line)
      (let ((fn (gethash command *command-table*)))
        (cond (fn (funcall fn irc prefix parameters))
              ((keywordp command)
               (format (display-pane irc) "~&[~A] -!- ~A" prefix (car (last parameters))))
              ((integerp command)
               (format (display-pane irc) "~&[~A] ~D ~A" prefix command parameters))
              (t (format (display-pane irc) "~&~A" line)))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-request-event))
  (let ((old-width (mezzano.gui.compositor:width (window app)))
        (old-height (mezzano.gui.compositor:height (window app)))
        (new-width (max 100 (mezzano.gui.compositor:width event)))
        (new-height (max 100 (mezzano.gui.compositor:height event))))
    (when (or (not (eql old-width new-width))
              (not (eql old-height new-height)))
      (let ((new-framebuffer (mezzano.gui:make-surface
                              new-width new-height))
            (frame (frame app))
            (font (font app)))
        (mezzano.gui.widgets:resize-frame (frame app) new-framebuffer)
        (mezzano.gui.widgets:resize-text-widget (display-pane app)
                                                new-framebuffer
                                                (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                                (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                (- new-width
                                                   (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                                   (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                                                (- new-height
                                                   (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                   (nth-value 3 (mezzano.gui.widgets:frame-size frame))
                                                   1
                                                   (mezzano.gui.font:line-height font)))
        (mezzano.gui.widgets:resize-text-widget (input-pane app)
                                                new-framebuffer
                                                (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                                (+ (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                   (- new-height
                                                      (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                      (nth-value 3 (mezzano.gui.widgets:frame-size frame))
                                                      (mezzano.gui.font:line-height font)))
                                                (- new-width
                                                   (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                                   (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                                                (mezzano.gui.font:line-height font))
        (draw-seperating-line app new-width new-height new-framebuffer)
        (mezzano.gui.compositor:resize-window
         (window app) new-framebuffer
         :origin (mezzano.gui.compositor:resize-origin event))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-event))
  (reset-input app))

(defun draw-seperating-line (irc width height framebuffer)
  (let* ((frame (frame irc))
         (font (font irc)))
    ;; Line seperating display and input panes.
    (mezzano.gui:bitset :set
                        (- width
                           (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                           (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                        1
                        (mezzano.gui:make-colour 0.5 0.5 0.5)
                        framebuffer
                        (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                        (+ (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                           (- height
                              (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                              (nth-value 3 (mezzano.gui.widgets:frame-size frame))
                              (mezzano.gui.font:line-height font)
                              1)))))

(defun irc-main ()
  (catch 'quit
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (fifo (mezzano.supervisor:make-fifo 50)))
      (mezzano.gui.compositor:with-window (window fifo 640 480)
        (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
               (frame (make-instance 'mezzano.gui.widgets:frame
                                     :framebuffer framebuffer
                                     :title "IRC"
                                     :close-button-p t
                                     :resizablep t
                                     :damage-function (mezzano.gui.widgets:default-damage-function window)
                                     :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
               (display-pane (make-instance 'mezzano.gui.widgets:text-widget
                                            :font font
                                            :framebuffer framebuffer
                                            :x-position (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                            :y-position (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                            :width (- (mezzano.gui.compositor:width window)
                                                      (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                                      (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                                            :height (- (mezzano.gui.compositor:height window)
                                                       (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                       (nth-value 3 (mezzano.gui.widgets:frame-size frame))
                                                       1
                                                       (mezzano.gui.font:line-height font))
                                            :damage-function (mezzano.gui.widgets:default-damage-function window)))
               (input-pane (make-instance 'irc-input-pane
                                          :history-table *irc-history*
                                          :font font
                                          :framebuffer framebuffer
                                          :x-position (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                          :y-position (+ (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                         (- (mezzano.gui.compositor:height window)
                                                            (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                                            (nth-value 3 (mezzano.gui.widgets:frame-size frame))
                                                            (mezzano.gui.font:line-height font)))
                                          :width (- (mezzano.gui.compositor:width window)
                                                    (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                                    (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                                          :height (mezzano.gui.font:line-height font)
                                          :damage-function (mezzano.gui.widgets:default-damage-function window)))
               (irc (make-instance 'irc-client
                                   :fifo fifo
                                   :window window
                                   :frame frame
                                   :font font
                                   :display-pane display-pane
                                   :input-pane input-pane)))
          (setf (slot-value input-pane '%irc) irc)
          (draw-seperating-line irc (mezzano.gui.compositor:width window) (mezzano.gui.compositor:height window) framebuffer)
          (mezzano.gui.widgets:draw-frame frame)
          (mezzano.gui.compositor:damage-window window
                                                0 0
                                                (mezzano.gui.compositor:width window)
                                                (mezzano.gui.compositor:height window))
          (unwind-protect
               (progn
                 (funcall (gethash "HELP" *top-level-commands*) irc "")
                 (loop
                    (handler-case
                        (progn
                          (reset-input irc)
                          (let ((line (read-line (input-pane irc))))
                            (multiple-value-bind (command rest)
                                (parse-command line)
                              (let ((fn (gethash (string-upcase command) *top-level-commands*)))
                                (if fn
                                    (funcall fn irc rest)
                                    (error "Unknown command ~S." command))))))
                      (error (c)
                        (format (display-pane irc) "~&Error: ~A" c)))))
            (when (irc-connection irc)
              (close (irc-connection irc)))))))))

(defun spawn ()
  (mezzano.supervisor:make-thread 'irc-main
                                  :name "IRC"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "IRC console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
