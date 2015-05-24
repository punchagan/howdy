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

;; You can configure the frequency at which you wish to contact a
;; person by adding a `KEEPINTOUCH_INTERVAL' property to your contacts.

;; You can add information about when you last contacted a person, by using the
;; `keepintouch' function.  This adds a LOGNOTE to your contacts db entry for
;; the person, with the current timestamp.

;; The `keepintouch-backlog' function creates a buffer with people who you
;; "need" to contact -- contacts for whom -- today - (last_contacted +
;; interval) is +ve.

;; FIXME: We need a way of showing upto N people to contact from the backlog,
;; ala org-contacts anniversary stuff.
