;;; pp-client.el --- A pp-client written in Emacs Lisp

;; Copyright (C) 2024 Hennes Märtins
;;
;; Author: Hennes Märtins

;;; Commentary:

;; This is an alternative planning poker client for pp
;; (https://github.com/sne11ius/pp).

;;; Code:
(require 'websocket)

(defgroup pp-client nil "Customization of pp-client.")

;; TODO validate defcustom values

(defcustom ppc-default-url "wss://pp.discordia.network"
  "Default pp-server URL.
The URL must have the form \"ws[s]://HOST[:PORT]\"."
  :group 'pp-client
  :type 'string)

(defcustom ppc-default-room "pp"
  "Default room to enter."
  :group 'pp-client
  :type 'string)

(defcustom ppc-default-user (getenv "USER")
  "Default user name."
  :group 'pp-client
  :type 'string)

(defcustom ppc-ping-seconds 60
  "Interval in seconds to send a PING to pp-server."
  :group 'pp-client
  :type 'natnum)

(defvar-local ppc-websocket nil
  "Buffer local websocket used by pp-client.")

(defvar-local ppc-ping-timer nil
  "Buffer local timer used to send ping to pp-server regularly.")

(defvar-local ppc-deck nil
  "Buffer local deck used by pp-server.")

(defface ppc-default-face
  '((default :inherit default))
  "Default face used in `ppc-mode."
  :group 'pp-client)

(defface ppc-label-face
  '((default :inherit font-lock-keyword-face :weight bold))
  "Face used for labels in `ppc-mode."
  :group 'pp-client)

(defface ppc-chat-log-face
  '((default :inherit font-lock-comment-face))
  "Face used for chat log messages in `ppc-mode."
  :group 'pp-client)

(defface ppc-info-log-face
  '((default :inherit default))
  "Face used for info log messages in `ppc-mode."
  :group 'pp-client)

(defface ppc-error-log-face
  '((default :inherit error))
  "Face used for error log messages in `ppc-mode."
  :group 'pp-client)

(defun ppc-format-user (user)
  "Format a USER entry taken from websocket-frame sent by pp-server."
  (let ((username (plist-get user :username))
        (userType (plist-get user :userType))
        (cardValue (plist-get user :cardValue))
        (yourUser  (plist-get user :yourUser))
        (labelize (lambda (s) (propertize s 'face 'ppc-label-face))))
    (concat
     (funcall labelize (if yourUser "Me:   " "User: "))
     (truncate-string-to-width username 20 nil ?\s "...")
     (funcall labelize "\tCard: ") cardValue)))

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

(defun ppc-format-log (log)
  "Format a LOG entry taken from websocket-frame sent by pp-server."
  (let ((level (plist-get log :level))
        (msg (plist-get log :message))
        (chatize (lambda (s) (propertize s 'face 'ppc-chat-log-face)))
        (infoize (lambda (s) (propertize s 'face 'ppc-info-log-face)))
        (errorize (lambda (s) (propertize s 'face 'ppc-error-log-face))))
    (cond ((string-equal level "CHAT") (funcall chatize msg))
          ((string-equal level "INFO") (funcall infoize msg))
          ((string-equal level "ERROR") (funcall errorize msg))
          (t msg))))

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
        (labelize (lambda (s) (propertize s 'face 'ppc-label-face))))
    (concat
     (funcall labelize "Room: ") roomId
     (funcall labelize "\nDeck: ") (mapconcat 'identity deck " ")
     (funcall labelize "\nGame phase: ") gamePhase
     "\n\n" (mapconcat 'ppc-format-user
                       (ppc-filter-and-sort-users users)
                       "\n")
     (funcall labelize "\n\nAverage: ") average
     (funcall labelize "\n\nLog:\n") (mapconcat 'ppc-format-log log "\n")
     "\n")))

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
The server is determined by URL.  If URL is empty, `ppc-default-url is used.
ROOM will be entered as USER.  If ROOM is empty, `ppc-default-room is used.
If USER is empty, `ppc-default-user is used.
The messages received by the pp-server are inserted into BUFFER."
  (if (null ppc-websocket)
      (let* ((url (if (string-empty-p url) ppc-default-url url))
             (room (if (string-empty-p room) ppc-default-room room))
             (user (if (string-empty-p user) ppc-default-user user))
             (ping-frame (make-websocket-frame
                          :opcode 'ping
                          :payload (encode-coding-string "Greetings from Emacs!"
                                                         'raw-text)
                          :completep t))
             (my-websocket
              (websocket-open
               (url-encode-url (format
                                "%s/rooms/%s?user=%s&userType=PARTICIPANT"
                                url room user))
               :on-message `(lambda (ws frame)
                              (ppc-on-message ,buffer ws frame))
               :on-close `(lambda (ws) (ppc-on-close ,buffer ws))))
             (ping `(lambda () (websocket-send ,my-websocket ,ping-frame))))
        (setq ppc-websocket my-websocket
              ppc-ping-timer (run-at-time t ppc-ping-seconds ping)))
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

(defun ppc-chat-message (chat-message)
  "Send a CHAT-MESSAGE.
This sends a websocket frame to the connected pp-server."
  (interactive "MMessage: ")
  (let ((msg (format
              "{\"requestType\": \"ChatMessage\", \"message\": \"%s\"}"
              chat-message)))
    (websocket-send-text ppc-websocket msg)
    (message "Sent chat message: %s" chat-message)))

(define-derived-mode ppc-mode special-mode "ppc"
  "The planning poker client mode.
The buffer is read-only.  The following keys are defined:
\"v\": `ppc-play-card
\"r\": `ppc-reveal-cards
\"n\": `ppc-start-new-round
\"q\": `ppc-close
\"c\": `ppc-chat-message`"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (keymap-set ppc-mode-map "v" 'ppc-play-card)
  (keymap-set ppc-mode-map "r" 'ppc-reveal-cards)
  (keymap-set ppc-mode-map "n" 'ppc-start-new-round)
  (keymap-set ppc-mode-map "q" 'ppc-close)
  (keymap-set ppc-mode-map "c" 'ppc-chat-message))

(defun ppc (arg)
  "Open a websocket connection to the pp-server.
To have multiple sessions call this function with different numeric prefixes
for ARG."
  (interactive "P")
  (let ((url (read-from-minibuffer
              (format "URL (default: %s): " ppc-default-url)))
        (room (read-from-minibuffer
               (format "room (default: %s): " ppc-default-room)))
        (user (read-from-minibuffer
               (format "user (default: %s): " ppc-default-user)))
        (buffer (if (numberp arg)
                    (get-buffer-create (format "%s<%d>" "*pp-client*" arg))
                  (get-buffer-create "*pp-client*"))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (save-selected-window
	(select-window (get-buffer-window buffer 0))
	(or (derived-mode-p 'ppc-mode)
	    (ppc-mode))
        (unless (null ppc-websocket) (ppc-close))
        (ppc-open-ws buffer url room user)
        (add-hook 'kill-buffer-hook 'ppc-close nil t)))))

;;; pp-client.el ends here
