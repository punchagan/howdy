;;; howdy.el --- A library to help you keep in touch with people.

;; Copyright (C) 2015 Puneeth Chaganti
;; Author: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Created: 24 May 2015
;; Keywords: contacts, org-mode, org-contacts
;; Homepage: http://github.com/punchagan/howdy

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Howdy! is a library to help you keep in touch with people.

;; The library currently only supports using org-contacts to maintain your
;; contacts database.

;; You can configure the frequency at which you wish to contact a person by
;; adding a `HOWDY_INTERVAL' property to your contacts.

;; You can update information about when you last contacted a person, by using
;; the `howdy-contacted' function.  For example, (howdy-contacted "John Doe").

;; To get agenda entries for out-of-touch contacts, use `howdy-howdy'. For
;; example, add an entry like the one below to one of your org-contacts-files.
;;
;; * Keep in touch
;; %%(howdy-contacts)

(require 'org-contacts)

(defgroup howdy nil
  "Options for howdy.")

(defcustom howdy-interval-property "HOWDY_INTERVAL"
  "Name of the property for howdy interval."
  :type 'string
  :group 'howdy)

(defcustom howdy-last-contacted-property "LAST_HOWDY"
  "Name of the property for last contacted timestamp."
  :type 'string
  :group 'howdy)

(defcustom howdy-agenda-entry-format "Say Howdy: %l (%p) (%e)"
  "Format of the \"say howdy!\" agenda entry.
The following replacements are available:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Email"
  :type 'string
  :group 'howdy)

(defun howdy-contacted (name &optional time)
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
          (org-set-property howdy-last-contacted-property timestamp))
      (error (format "No contact %s found!" name)))))

(defun howdy-howdy (&optional format)
  "Returns agenda entries for out-of-touch contacts.

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Email"
  (let ()
    (unless format (setq format howdy-agenda-entry-format))
    (loop for contact in (howdy--backlog-contacts)
          collect (format-spec format
                               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                                 (?h . ,(car contact))
                                 (?p . ,(cdr (assoc-string org-contacts-tel-property (caddr contact))))
                                 (?e . ,(cdr (assoc-string org-contacts-email-property (caddr contact)))))))))

(defun howdy--backlog-contacts ()
  "Returns a list of contacts who need to be contacted."
  (loop for contact in (org-contacts-db)
	  if (howdy--backlog-contact-p contact)
	  collect contact))

(defun howdy--backlog-contact-p (contact)
  (let ((interval
         (ignore-errors
           (string-to-number
            (cdr (assoc-string howdy-interval-property (caddr contact))))))
        (last-contacted
         (ignore-errors
           (org-parse-time-string
            (cdr (assoc-string howdy-last-contacted-property (caddr contact)))))))
    (when interval
      (if last-contacted
          (< interval
             (time-to-number-of-days
              (time-since (apply 'encode-time last-contacted))))
        t))))

(provide 'howdy)

;;; howdy.el ends here
