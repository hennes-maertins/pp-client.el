;;; pp-client.el --- A pp-client written in Emacs Lisp

;;; Commentary:

;; TODO

;;; Code:
(require 'websocket)

(defvar-local ppc-websocket nil
  "Buffer local websocket used by pp-client.")

(defvar-local ppc-ping-timer nil
  "Buffer local timer used to send ping to pp-server regularly.")

(defvar-local ppc-deck nil
  "Buffer local deck used by pp-server.")

(defun ppc-format-user (user)
  "Format a USER entry taken from websocket-frame sent by pp-server."
  (let ((username (plist-get user :username))
        (userType (plist-get user :userType))
        (cardValue (plist-get user :cardValue))
        (yourUser  (plist-get user :yourUser))
        (labelize (lambda (s) (propertize s 'face 'bold-italic))))
    (concat
     (funcall labelize (if yourUser "Me:   " "User: "))
     (truncate-string-to-width username 20 nil ?\s "...")
     (funcall labelize "\tCard: ") cardValue
     )))

(defun ppc-compare-users (user1 user2)
  "Determine order of USER1 and USER2 by username."
  (string< (plist-get user1 :username)
           (plist-get user2 :username)))

(defun ppc-filter-and-sort-users (users)
  "Filter and sort the list of USERS.  Only participants are kept.
The own user is the last one.  Other users are sorted by `ppc-compare-users."
  (let* ((ps (seq-filter (lambda (u) (string= "PARTICIPANT"
                                              (plist-get u :userType)))
                         users))
         (me (seq-filter (lambda (u) (plist-get u :yourUser)) ps))
         (nme (seq-filter (lambda (u) (not (plist-get u :yourUser))) ps)))
    (append (seq-sort 'ppc-compare-users nme) me)))

(defun ppc-format-message (msg)
  "Format MSG sent by pp-server.
MSG is the payload of a websocket-frame parsed to json.
The message contains the room id, the deck of cards and
the cards played by users among other things."
  (let ((roomId (plist-get msg :roomId))
        (deck (plist-get msg :deck))
        (gamePhase (plist-get msg :gamePhase))
        (users (plist-get msg :users))
        (average (plist-get msg :average))
        (log (plist-get msg :log))
        (labelize (lambda (s) (propertize s 'face 'bold-italic))))
    (concat
     (funcall labelize "Room: ") roomId
     (funcall labelize "\nDeck: ") (mapconcat 'identity deck " ")
     (funcall labelize "\nGame phase: ") gamePhase
     "\n\n" (mapconcat 'ppc-format-user
                       (ppc-filter-and-sort-users users)
                       "\n")
     (funcall labelize "\n\nAverage: ") average "\n")))

(defun ppc-on-message (buffer _websocket frame)
  "Parse and format FRAME received by pp-server and insert it into BUFFER.
The parsed FRAME is formatted by `ppc-format-message.
The old content of BUFFER is erased before insertion.
This is a handler method used by `ppc-open-ws."
  (let ((msg (json-parse-string (websocket-frame-text frame)
                                :object-type 'plist
                                :array-type 'list
                                :null-object nil
                                :false-object nil)))
    (with-current-buffer buffer
      (cl-assert (derived-mode-p 'ppc-mode))
      (setq ppc-deck (plist-get msg :deck)
            buffer-read-only nil)
      (erase-buffer)
      (insert (ppc-format-message msg))
      (goto-char (point-max))
      (setq buffer-read-only t))))

(defun ppc-on-close (_buffer _websocket)
  "Do some cleanup when connection is closed.
This is a handler method used by `ppc-open-ws."
  (cancel-timer ppc-ping-timer)
  (setq ppc-websocket nil)
  (message "ppc-websocket closed"))

(defun ppc-open-ws (buffer url room user)
  "Setup a connection to a pp-server.
The server is determined by URL.  If URL is empty, a default value is used.
ROOM will be entered as USER.  If ROOM and USER are empty, default values are
used.  The messages received by the pp-server are inserted into BUFFER."
  (if (null ppc-websocket)
      (let* ((url (if (string-empty-p url) "wss://pp.discordia.network" url))
             (room (if (string-empty-p room) "pp" room))
             (user (if (string-empty-p user) (getenv "USER") user))
             (ping-frame (make-websocket-frame
                          :opcode 'ping
                          :payload (encode-coding-string "Greetings from Emacs!"
                                                         'raw-text)
                          :completep t))
             (my-websocket (websocket-open
                            (format "%s/rooms/%s?user=%s&userType=PARTICIPANT"
                                    url room user)
                            :on-message `(lambda (ws frame)
                                           (ppc-on-message ,buffer ws frame))
                            :on-close `(lambda (ws) (ppc-on-close ,buffer ws))))
             (ping `(lambda () (websocket-send ,my-websocket ,ping-frame))))
        (setq ppc-websocket my-websocket
              ppc-ping-timer (run-at-time t 10 ping))) ;; TODO 60 seconds (customizable)
    (message "ppc-websocket already opened")))

(defun ppc-close ()
  "Close connection to pp-server."
  (interactive)
  (unless (null ppc-websocket)
    (websocket-close ppc-websocket)))

(defun ppc-play-card (card)
  "Play CARD.  This sends a websocket frame to the connected pp-server."
  (interactive (list (completing-read "Your card: " ppc-deck nil t nil nil nil t)))
  (let ((msg (format
              "{\"requestType\": \"PlayCard\", \"cardValue\":\"%s\"}"
              card)))
    (websocket-send-text ppc-websocket msg)
    (message "Sent card: %s" card)))

(defun ppc-start-new-round ()
  "Start new round.  This sends a websocket frame to the connected pp-server."
  (interactive)
  (let ((msg "{\"requestType\": \"StartNewRound\"}"))
    (websocket-send-text ppc-websocket msg)
    (message "Sent new round")))

(defun ppc-reveal-cards ()
  "Reveal cards.  This sends a websocket frame to the connected pp-server."
  (interactive)
  (let ((msg "{\"requestType\": \"RevealCards\"}"))
    (websocket-send-text ppc-websocket msg)
    (message "Sent reveal cards")))

(define-derived-mode ppc-mode special-mode "ppc"
  "The planning poker client mode.
The buffer is read-only.  The following keys are defined:
\"c\": `ppc-play-card
\"n\": `ppc-start-new-round
\"r\": `ppc-reveal-cards
\"q\": `ppc-close"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (keymap-set ppc-mode-map "c" 'ppc-play-card)
  (keymap-set ppc-mode-map "n" 'ppc-start-new-round)
  (keymap-set ppc-mode-map "r" 'ppc-reveal-cards)
  (keymap-set ppc-mode-map "q" 'ppc-close))

(defun ppc (arg url room user)
  "Open a websocket connection to the pp-server at URL with ROOM and USER.
The URL must have the form \"ws[s]://HOST[:PORT]\".
To have multiple sessions call this function with different numeric prefixes
for ARG."
  (interactive "P\n\
MURL (default: wss://pp.discordia.network): \n\
Mroom (default: pp): \n\
Muser (default: $USER): ")
  (let ((buf (if (numberp arg)
                 (get-buffer-create (format "%s<%d>" "*pp-client*" arg))
               (get-buffer-create "*pp-client*"))))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (save-selected-window
	;; We switch to the buffer's window in order to be able
	;; to modify the value of point
	(select-window (get-buffer-window buf 0))
	(or (derived-mode-p 'ppc-mode)
	    (ppc-mode))
        (unless (null ppc-websocket) (ppc-close))
        (ppc-open-ws buf url room user)
        (add-hook 'kill-buffer-hook 'ppc-close nil t)))))

;;; pp-client.el ends here
