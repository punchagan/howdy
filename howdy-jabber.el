;;; howdy-jabber.el --- Howdy code for integrating with Jabber -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Howdy code for integrating with Jabber
;;
;;; Code:

(require 'org-contacts)
(require 'howdy-email)

(defcustom howdy-jabber-domains nil
  "List of email domains accepted as jabber ids.

If NIL all email ids are accepted as jabber ids. If multiple
email ids are valid, the ids are prioritized in the order
specified here."

  :type 'list
  :group 'howdy)

(defcustom howdy-jabber-function
  (lambda (jid) (jabber-chat-with (jabber-read-account nil jid) jid))
  "Function to use for sending a jabber message."
  :type 'function
  :group 'howdy)

(defcustom howdy-jabber-property "JABBER"
  "Name of the property for jabber."
  :type 'string
  :group 'howdy)

(defun howdy-jabber--get-email-jid (contact)
  "Get email id for CONTACT to use as JID."
  (let* ((emails (cdr (assoc-string org-contacts-email-property (caddr contact))))
         (ids (org-contacts-split-property (or emails ""))))
    (if howdy-jabber-domains
        (car
         (cl-loop for domain in howdy-jabber-domains
                  for email = (car (seq-filter (lambda (email) (string-match domain email)) ids))
                  collect email))
      (howdy-email-get-primary-email contact))))

(defun howdy-jabber-get-jabber-id (contact)
  "Get jabber id for the CONTACT.

If `howdy-jabber-property' is not set, use a valid email-id based
on `howdy-jabber-domains'."
  (let ((jid (cdr (assoc-string "JABBER" (caddr contact)))))
    (or jid (howdy-jabber--get-email-jid contact))))

(declare-function howdy-contacted "howdy")
(defun howdy-jabber-message-received-hook (from _buffer _text _title)
  (let* ((user (jabber-jid-user from))
         (name (jabber-jid-displayname user))
         (info `((:email . ,user) (:name . ,name))))
    (howdy-contacted info)))

(defun howdy-jabber-message-send-hook (_body _id)
  (let* ((name (jabber-jid-displayname (jabber-jid-user jabber-chatting-with)))
         (email (jabber-jid-user jabber-chatting-with))
         (info `((:name . ,name) (:email . ,email))))
    (howdy-contacted info nil))
  nil)

(provide 'howdy-jabber)
;;; howdy-jabber.el ends here
