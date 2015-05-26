;;; keepintouch.el --- A library to help you keep in touch with people.

;; Copyright (C) 2015 Puneeth Chaganti
;; Author: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Created: 24 May 2015
;; Keywords: contacts, org-mode, org-contacts
;; Homepage: http://github.com/punchagan/keepintouch

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Keep in touch is a library to help you keep in touch with people.

;; The library currently supports using org-contacts to maintain your contacts
;; database.

;; You can configure the frequency at which you wish to contact a person by
;; adding a `KEEPINTOUCH_INTERVAL' property to your contacts.

;; You can add information about when you last contacted a person, by using the
;; `keepintouch' function.  This adds a LOGNOTE to your contacts db entry for
;; the person, with the current timestamp.

;; The `keepintouch-contacts' function can be used to add agenda entries for
;; contacts who are out of touch.
;; Adding an entry like:
;;
;; * Keep in touch
;; %%(keepintouch-contacts)
;;
;; to an org-contacts-file will add agenda entries with list of people who are
;; out of touch.

(require 'org-contacts)

(defgroup keepintouch nil
  "Options for keepintouch.")

(defcustom keepintouch-interval-property "KEEPINTOUCH_INTERVAL"
  "Name of the property for keepintouch interval."
  :type 'string
  :group 'keepintouch)

(defcustom keepintouch-last-contacted-property "KEEPINTOUCH_LAST_CONTACTED"
  "Name of the property for last contacted timestamp."
  :type 'string
  :group 'keepintouch)

(defcustom keepintouch-agenda-entry-format "Say Howdy: %l (%p) (%e)"
  "Format of the \"say howdy!\" agenda entry.
The following replacements are available:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Email"
  :type 'string
  :group 'keepintouch)

(defun keepintouch (name &optional time)
  "Update last contacted time for the contact.

If TIME is nil, `org-log-note-effective-time' is used."
  ;;FIXME: Currently the exact name is required.  Later, allow
  ;;email/phone/nick.  More smartness!
  (unless time
    (setq time org-log-note-effective-time))
  (let ((contact (org-contacts-filter (concat "^" name "$"))))
    (if contact
        (let ((marker (cadar contact))
              (timestamp (format-time-string
                          (org-time-stamp-format 'long t) time)))
          (switch-to-buffer-other-window (marker-buffer marker))
          (goto-char marker)
          (org-set-property keepintouch-last-contacted-property timestamp))
      (error (format "No contact %s found!" name)))))

(defun keepintouch-contacts (&optional format)
  "Returns agenda entries for out-of-touch contacts.

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Email"
  (let ()
    (unless format (setq format keepintouch-agenda-entry-format))
    (loop for contact in (keepintouch-backlog-contacts)
          collect (format-spec format
                               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                                 (?h . ,(car contact))
                                 (?p . ,(cdr (assoc-string org-contacts-tel-property (caddr contact))))
                                 (?e . ,(cdr (assoc-string org-contacts-email-property (caddr contact)))))))))

(defun keepintouch-backlog-contacts ()
  "Returns a list of contacts who need to be contacted."
  (loop for contact in (org-contacts-db)
	  if (keepintouch-backlog-contact-p contact)
	  collect contact))

(defun keepintouch-backlog-contact-p (contact)
  (let ((interval
         (ignore-errors
           (string-to-number
            (cdr (assoc-string keepintouch-interval-property (caddr contact))))))
        (last-contacted
         (ignore-errors
           (org-parse-time-string
            (cdr (assoc-string keepintouch-last-contacted-property (caddr contact)))))))
    (when interval
      (if last-contacted
          (< interval
             (time-to-number-of-days
              (time-since (apply 'encode-time last-contacted))))
        t))))

(provide 'keepintouch)

;;; keepintouch.el ends here
