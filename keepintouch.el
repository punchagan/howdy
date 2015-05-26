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

;; The `keepintouch-backlog' function creates a buffer with people who you
;; "need" to contact -- contacts for whom -- today - (last_contacted +
;; interval) is +ve.

;; FIXME: We need a way of showing upto N people to contact from the backlog,
;; ala org-contacts anniversary stuff.

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
