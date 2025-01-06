;;; howdy-mu4e.el --- Howdy code for mu4e integration -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Howdy code for mu4e integration
;;
;;; Code:

(declare-function howdy-contacted "howdy")
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-message-field "mu4e-message")

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
      (howdy-contacted info time))))

(provide 'howdy-mu4e)
;;; howdy-mu4e.el ends here
