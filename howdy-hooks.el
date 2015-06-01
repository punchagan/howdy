;; howdy-hooks.el -- a collection of utility hooks.

;; NOTE: The module doesn't require any of the dependent modules, and you
;; should ensure that the modules required for the hooks you are using are
;; available.

(defun howdy-email-message-send-hook ()
  (let* ((to (mail-fetch-field "to" nil t))
         (cc (mail-fetch-field "cc" nil t))
         (emails
          (mail-strip-quoted-names
           (concat to (when cc (concat ", " cc))))))
    (loop for email in (split-string emails ",[[:space:]]+" t)
          do (let ((info `((:email . ,email))))
               (howdy--contacted info nil)))))

(defun howdy-jabber-message-received-hook (from buffer text title)
  (let ((user (jabber-jid-user from))
        (name (jabber-jid-displayname user))
        (info `((:email . ,user) (:name . ,name))))
    (howdy--contacted info)))

(defun howdy-jabber-message-send-hook (body id)
  (let* ((name (jabber-jid-displayname (jabber-jid-user jabber-chatting-with)))
         (email (jabber-jid-user jabber-chatting-with))
         (info `((:name . ,name) (:email . ,email))))
    (howdy--contacted info nil))
  nil)

(defun howdy-mu4e-message-receive-hook ()
  (let* ((msg (mu4e-message-at-point 'noerror))
         (from (car (mu4e-message-field msg :from)))
         (name (car from))
         (email (cdr from))
         (time (mu4e-message-field msg :date))
         (flags (mu4e-message-field msg :flags))
         (unread (member 'unread flags))
         (to-me (not (member 'list flags)))
         (info `((:name . ,name) (:email . ,email))))
    (when (and to-me unread)
      (howdy--contacted info time))))

(provide 'howdy-hooks)
