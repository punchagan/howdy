;;; howdy-email.el --- Howdy code for Email integration -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Howdy code for Email integration
;;
;;; Code:

(require 'org-contacts)

(declare-function howdy-contacted "howdy")
(declare-function mail-strip-quoted-names "mail-utils")

(defun howdy-email-get-primary-email (contact)
  "Get primary email id for CONTACT."
  (let* ((emails (cdr (assoc-string org-contacts-email-property (caddr contact))))
         (ids (org-contacts-split-property (or emails ""))))
    (car ids)))

(defun howdy-email-message-send-hook ()
  (let* ((to (mail-fetch-field "to" nil t))
         (cc (mail-fetch-field "cc" nil t))
         (emails
          (mail-strip-quoted-names
           (concat to (when cc (concat ", " cc))))))
    (cl-loop for email in (split-string emails ",[[:space:]]+" t)
             do (let ((info `((:email . ,email))))
                  (howdy-contacted info nil)))))

(provide 'howdy-email)
;;; howdy-email.el ends here
