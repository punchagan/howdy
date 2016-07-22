;;; howdy.el --- An Emacs utility to help you keep in touch with people.

;; Copyright (C) 2015 Puneeth Chaganti
;; Author: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Created: 24 May 2015
;; Keywords: contacts, org-mode, org-contacts
;; Homepage: http://github.com/punchagan/howdy

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; Howdy! is a library to help you keep in touch with people.

;; The library currently only supports using org-contacts to maintain your
;; contacts database.

;; You can configure the frequency at which you wish to contact a person by
;; adding a `HOWDY_INTERVAL' property to your contacts.

;; You can update information about when you last contacted a person, by using
;; the `howdy' function.

;; To get agenda entries for out-of-touch contacts, use `howdy-howdy'. For
;; example, add an entry like the one below to one of your org-contacts-files.
;;
;; : * Keep in touch
;; : %%(howdy-howdy)

(require 'cl)
(require 'org-contacts)

(defgroup howdy nil
  "Options for howdy.")

(defcustom howdy-interval-property "HOWDY_INTERVAL"
  "Name of the property for howdy interval."
  :type 'string
  :group 'howdy)

(defcustom howdy-interval-default 15
  "The default number of days to use as howdy interval."
  :type 'integer
  :group 'howdy)

(defcustom howdy-last-contacted-property "LAST_HOWDY"
  "Name of the property for last contacted timestamp."
  :type 'string
  :group 'howdy)

(defcustom howdy-agenda-entry-format "Say Howdy: %l (%p) %E"
  "Format of the \"say howdy!\" agenda entry.
The following replacements are available:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Link to email
  %E - All emails"
  :type 'string
  :group 'howdy)

(defcustom howdy-max-contacts 5
  "Limit for the number of contacts to show in the agenda."
  :type 'integer
  :group 'howdy)

(defcustom howdy-scheduler 'backlog
  "The scheduler to use when displaying out-of-touch contacts."
  :type '(choice
          (const :tag "Random" random)
          (const :tag "Backlog" backlog))
  :group 'howdy)

(defvar howdy-add-contact-function nil
  "Function to call when a contact is not present in the
  database.")

(defmacro howdy--with-contact (contact &rest body)
  "Eval the BODY with point at the given contact."
  `(let ((marker (second contact)))
     (with-current-buffer (marker-buffer marker)
       (save-excursion
         (save-restriction
           (goto-char marker)
           ,@body))
       (save-buffer))
     (run-with-idle-timer 1 nil 'org-contacts-db)))

(defun howdy--backlog-contact-p (contact)
  (let ((backlog (howdy--get-backlog contact)))
    (if (null backlog)
        nil
      (>= backlog 0))))

(defun howdy--backlog-contacts ()
  "Returns a list of contacts who need to be contacted."
  (loop for contact in (org-contacts-db)
        if (howdy--backlog-contact-p contact)
        collect contact))

(defun howdy--cleanup-phone (phone-number)
  "Strip off all spaces and dashes from a phone number"
  (replace-regexp-in-string
   "^0+" ""
   (replace-regexp-in-string "\\(\s\\|-\\)+" ""  phone-number)))

(defun howdy--completing-read-name-or-tag ()
  (org-completing-read
   "Name or Tag: "
   (append
    (howdy--contact-tags)
    (mapcar (lambda (x) (org-no-properties (car x))) (org-contacts-filter)))))

(defun howdy--contact-tags ()
  "Get all the tags from the contacts db."
  (delete-dups
   (loop for contact in  (org-contacts-db)
         for tags = (cdr (assoc-string "ALLTAGS" (caddr contact)))
         if (not (null tags))
         append (org-split-string tags ":"))))

(defun howdy--contacted (info &optional time)
  "Update last contacted time for the contact.

If TIME is nil, `current-time' is used."
  (let ((contacts (howdy--find-contacts info)))
    (unless time (setq time (current-time)))
    (if (> (length contacts) 0)
        (loop for contact in contacts
              do (howdy--contacted-contact contact time))
      (when howdy-add-contact-function
        (ignore-errors (funcall howdy-add-contact-function info))
        (loop for contact in (howdy--find-contacts info)
              do (howdy--contacted-contact contact time))))))

(defun howdy--contacted-contact (contact time)
  "Update last contacted time for the contact."
  (howdy--with-contact
   contact
   (let (old-time)
     (setq old-time
           (ignore-errors
             (org-parse-time-string
              (cdr (assoc-string howdy-last-contacted-property (org-entry-properties))))))
     (if (or (not old-time)
             (time-less-p (apply 'encode-time old-time) time))
         (org-set-property
          howdy-last-contacted-property
          (format-time-string
           (org-time-stamp-format nil t) time))
       (message (format "Not updating %s with older timestamp." (car contact)))))))

(defun howdy--endswith (s end)
  "Check if S ends with END."
  (org-string-match-p (format "^.*%s$" end) s))

(defun howdy--find-contacts (info)
  "Find the contact using the given info."
  (let ((name (cdr (assoc :name info)))
        (email (cdr (assoc :email info)))
	(phone (cdr (assoc :phone info)))
	(tag (cdr (assoc :tag info)))
        props)
    (or
      (when email
        (setq props `("EMAIL" . ,email))
        (org-contacts-filter (concat "^" name "$") nil props))
      (when phone
        (loop for contact in (org-contacts-db)
              if (org-find-if (lambda (prop)
                                (and (howdy--startswith (car prop) org-contacts-tel-property)
                                     (let* ((number (howdy--cleanup-phone (cdr prop)))
                                            (n (length number))
                                            (phone (howdy--cleanup-phone phone))
                                            (p (length phone)))
                                       (and (>= n 7)
                                            (>= p 7)
                                            (or (howdy--endswith number phone)
                                                (howdy--endswith phone number))))))
                              (caddr contact))
             collect contact))
      (when tag (howdy--get-contacts-for-tag tag))
      (org-contacts-filter (concat "^" name "$") nil props))))

(defun howdy--format-contact (contact &optional format)
  (format-spec (or format howdy-agenda-entry-format)
               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                 (?h . ,(car contact))
                 (?p . ,(cdr (assoc-string org-contacts-tel-property (caddr contact))))
                 (?e . ,(howdy--get-email-link contact))
                 (?E . ,(howdy--get-email-links contact)))))

(defun howdy--get-backlog (contact)
  (let ((interval
         (ignore-errors
           (string-to-number
            (cdr (assoc-string howdy-interval-property (caddr contact))))))
        (last-contacted
         (ignore-errors
           (org-parse-time-string
            (cdr (assoc-string howdy-last-contacted-property (caddr contact)))))))
    (cond
     ((null interval) nil)
     ((null last-contacted) 0)
     (t
      (-
       (time-to-number-of-days (time-since (apply 'encode-time last-contacted)))
       interval)))))

(defun howdy--get-contacts-for-tag (tag)
    "Return all contacts with the given tag."
    (loop for contact in (org-contacts-db)
          for tags = (cdr (assoc-string "ALLTAGS" (caddr contact)))
          if (save-match-data (string-match (format ":%s:" tag) (or tags "")))
          collect contact))

(defun howdy--get-email-link (contact)
  (let ((emails (cdr (assoc-string org-contacts-email-property (caddr contact)))))
    (if (and emails (not (equal emails "")))
        (let ((email (car (split-string emails " "))))
          (format "[[mailto:%s][%s]]" email email))
      "")))

(defun howdy--get-email-links (contact)
  (let* ((emails (cdr (assoc-string org-contacts-email-property (caddr contact)))))
    (if (and emails (not (equal emails "")))
        (mapconcat
         (lambda (email) (format "[[mailto:%s][%s]]" email email))
         (split-string emails " ")
         " ")
      "")))

(defun howdy--startswith (s begin)
  "Check if S begins with BEGIN."
  (org-string-match-p (format "^%s.*$" begin) s))

(defun howdy-agenda-contacted (arg)
  "Mark a contact as contacted from an org-agenda."
  (interactive "P")
  (let* ((txt (org-no-properties (org-get-at-bol 'txt)))
         name info contact)
    (string-match "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" txt)
    (setq name (org-no-properties (match-string 1 txt)))
    (setq info `((:name . ,name)))
    (setq contact (car (howdy--find-contacts info)))
    (if arg
        (howdy--contacted info (org-read-date nil t nil nil (current-time)))
      (howdy--contacted info))))

(defun howdy-clear-backlog (confirm)
  "Clear all backlog contacts by resetting last howdy to now"
  (interactive (list (yes-or-no-p "Clear all howdy backlog?")))
  (let ((contacts (howdy--backlog-contacts))
        (time (org-read-date nil t nil nil (current-time))))
    (when confirm
      (loop for contact in contacts
            do (howdy--contacted-contact contact time)))))

(defun howdy-howdy (&optional format)
  "Returns agenda entries for out-of-touch contacts.

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Email"
  (let* ((contacts (howdy--backlog-contacts))
         entries)
    (cond
     ((equal howdy-scheduler 'random)
      (shuffle-list contacts))
     ((equal howdy-scheduler 'backlog)
      (setq contacts
            (sort contacts
                  (lambda (x y) (> (howdy--get-backlog x) (howdy--get-backlog y)))))))
    (setq entries (loop for contact in contacts
                        collect (howdy--format-contact contact format)))
    (if (> (length entries) howdy-max-contacts)
        (subseq entries 0 howdy-max-contacts)
      entries)))

(defun howdy-set-interval (&optional name interval)
  "Set the HOWDY_INTERVAL for a contact.

INTERVAL is the number of days to set as HOWDY_INTERVAL."
  (interactive)
  (unless name
    (setq name (org-contacts-completing-read "Name: ")))
  (unless interval
    (setq interval
          (read-number (format "%s (days): " howdy-interval-property)
                       howdy-interval-default)))
  (let ((contact (car (howdy--find-contacts `((:name . ,name))))))
    (if (null contact)
        (error (format "No contact %s found!" name))
      (howdy--with-contact
       contact
       (org-set-property howdy-interval-property (number-to-string interval))))))

(defun howdy ()
  "Update last contacted time for the contact.

If TIME is nil, `current-time' is used.

This function can only be called interactively.  Use
`howdy--contacted' for doing stuff programmatically."
  (interactive)
  (let* ((name-or-tag (howdy--completing-read-name-or-tag))
         (time (org-read-date nil t nil nil (current-time)))
         (info `((:name . ,name-or-tag) (:tag . ,name-or-tag))))
    (howdy--contacted info time)))

(provide 'howdy)

;;; howdy.el ends here
