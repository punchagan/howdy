;; howdy-hooks.el -- a collection of utility hooks.

;; NOTE: The module doesn't require any of the dependent modules, and you
;; should ensure that the modules required for the hooks you are using are
;; available.

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

(provide 'howdy-hooks)
