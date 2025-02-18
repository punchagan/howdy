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
;;
;; You can use it to keep track of when you last contacted someone, and to
;; remind you to contact them again after a certain interval. It can also help
;; you generate an agenda view of people you need to contact.
;;
;; To use Howdy!, you need to have a contacts database that is managed by
;; `org-contacts'.
;;
;; You can configure the frequency at which you wish to contact a person by
;; adding a `HOWDY_INTERVAL' property to your contacts.

;; You can use the `howdy' function to mark a contact as contacted,
;; interactively. This will update the `LAST_HOWDY' property with the current
;; date. You can use the `howdy-contacted' function to mark a contact as
;; contacted, programatically. For example, look at the hooks provided in the
;; various integrations like `howdy-jabber', `howdy-email', `howdy-wa', etc.
;;
;; Howdy can display a list of contacts that you need to contact, in an agenda
;; view. To enable this, add an entry like the one below to one of your
;; org-contacts-files.
;;
;; : * Keep in touch
;; : %%(howdy-howdy)
;;
;; You can also use the `howdy-agenda-contacted' function to mark a contact as
;; contacted from an org agenda buffer. You can bind this function to a
;; keybinding in the `org-agenda-mode-map' to make it easy to mark a contact as
;; contacted from an Org agenda.
;;
;;; Code:

(require 'org-contacts)
(require 'howdy-jabber)
(require 'howdy-mu4e)

(defgroup howdy nil
  "Options for howdy."
  :group 'howdy)

(defcustom howdy-email-function #'compose-mail
  "Function to use for composing email."
  :type 'function
  :group 'howdy)

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

(defcustom howdy-agenda-entry-format "Howdy!%b: %l (%p) %E"
  "Format of the \"say howdy!\" agenda entry.
The following replacements are available:

  %h - Heading name
  %b - Show backlog duration
  %B - Show backlog duration along with last contacted date
  %l - Link to the heading
  %p - Phone number
  %e - Link to email
  %E - All emails"
  :type 'string
  :group 'howdy)

(defcustom howdy-max-contacts 5
  "Limit for the number of contacts to show in the agenda.

It can be set to 0 or a negative number to turn off limiting the
number of contacts shown."
  :type 'integer
  :group 'howdy)

(defcustom howdy-scheduler 'backlog
  "The scheduler to use when displaying out-of-touch contacts."
  :type '(choice
          (const :tag "Random" random)
          (const :tag "Backlog" backlog))
  :group 'howdy)

(defvar howdy-add-contact-function nil
  "Function to call when a contact is not present in the database.")

(defmacro howdy--with-contact (contact &rest body)
  "Eval the BODY with point at the given CONTACT."
  `(let ((marker (second contact)))
     (with-current-buffer (marker-buffer marker)
       (save-excursion
         (save-restriction
           (goto-char marker)
           ,@body))
       (save-buffer))
     (run-with-idle-timer 1 nil 'org-contacts-db)))

(defun howdy--backlog-contact-p (contact &optional at-time)
  "Check whether CONTACT is a backlog contact at AT-TIME."
  (let ((backlog (howdy--get-backlog contact at-time)))
    (if (null backlog)
        nil
      (and (>= backlog 0) backlog))))

(defun howdy--backlog-contacts (&optional at-time)
  "Return a list of contacts who need to be contacted AT-TIME."
  (cl-loop for contact in (org-contacts-db)
           do (let ((backlog (howdy--backlog-contact-p contact at-time)))
                (push `("BACKLOG" . ,backlog) (caddr contact)))
           if (cdr (assoc "BACKLOG" (caddr contact)))
           collect contact))

(defun howdy--backlog-format-str (contact &optional last-contacted)
  "Return the format string for displaying a CONTACT.

If LAST-CONTACTED is non-nil, the last contacted date is
displayed along with the number of backlog days."
  (let ((backlog (round (or (cdr (assoc "BACKLOG" (caddr contact))) 0)))
        (last-contacted (and last-contacted
                             (cdr (assoc "LAST_HOWDY" (caddr contact))))))
    (if (> backlog 0)
        (if last-contacted
            (format " %s [%sx]" last-contacted backlog)
          (format " [%sx]" backlog))
      (if last-contacted
          (format " %s" last-contacted)
        ""))))

(defun howdy--cleanup-phone (phone-number)
  "Strip off all spaces and dashes from a PHONE-NUMBER."
  (replace-regexp-in-string
   "^0+" ""
   (replace-regexp-in-string "\\(\s\\|-\\)+" ""  phone-number)))

(defun howdy-completing-read-name-or-tag ()
  "Prompt the user for a name or tag to search for."
  (org-completing-read
   "Name or Tag: "
   (append
    (howdy-contact-tags)
    (mapcar (lambda (x) (org-no-properties (car x))) (org-contacts-filter)))))

(defun howdy-contact-tags ()
  "Get all the tags from the contacts db."
  (delete-dups
   (cl-loop for contact in  (org-contacts-db)
            for tags = (cdr (assoc-string "ALLTAGS" (caddr contact)))
            if (not (null tags))
            append (org-split-string tags ":"))))

(defun howdy-contacted (info &optional time)
  "Update last contacted TIME for a contact based on INFO.

If TIME is nil, `current-time' is used."
  (let ((contacts (howdy-find-contacts info)))
    (unless time (setq time (current-time)))
    (when (and (= (length contacts) 0) howdy-add-contact-function)
      (ignore-errors (funcall howdy-add-contact-function info)))
    (cl-loop for contact in (howdy-find-contacts info)
             do (howdy--contacted-contact contact time)))
  (when (bound-and-true-p org--diary-sexp-entry-cache)
    (clrhash org--diary-sexp-entry-cache)))

(defun howdy--contacted-contact (contact time)
  "Update last contacted TIME for CONTACT."
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

(defun howdy-find-contacts (info)
  "Find contact using the given INFO."
  (let* ((name (cdr (assoc :name info)))
         (name-re (concat "^" (regexp-quote (or name "")) "$"))
         (email (cdr (assoc :email info)))
         (phone (cdr (assoc :phone info)))
         (clean-phone (and phone (howdy--cleanup-phone phone)))
         (tag (cdr (assoc :tag info))))
    (or
     ;; Search by name + email
     (and email
          (or
           ;; Search by name + email (in Jabber property)
           (org-contacts-filter name-re
                                nil
                                `(,howdy-jabber-property . ,email))
           ;; Search by name + email (in email property)
           (org-contacts-filter name-re
                                nil
                                `(,org-contacts-email-property . ,email))))

     ;; Search by phone
     (and clean-phone (>= (length clean-phone) 7)
          (cl-loop for contact in (org-contacts-db)
                   if (cl-some (lambda (prop)
                                 (and (howdy--startswith (car prop) org-contacts-tel-property)
                                      (let* ((number (howdy--cleanup-phone (cdr prop)))
                                             (n (length number)))
                                        (and (>= n 7)
                                             (or (howdy--endswith number clean-phone)
                                                 (howdy--endswith clean-phone number))))))
                               (caddr contact))
                   collect contact))

     ;; Search by tag
     (and tag
          (howdy-get-contacts-for-tag tag))

     ;; Default search
     (org-contacts-filter name-re nil nil))))

(defun howdy--format-contact (contact &optional format)
  "Format a CONTACT based on FORMAT."
  (format-spec (or format howdy-agenda-entry-format)
               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                 (?h . ,(car contact))
                 (?b . ,(howdy--backlog-format-str contact nil))
                 (?B . ,(howdy--backlog-format-str contact t))
                 (?p . ,(cdr (assoc-string org-contacts-tel-property (caddr contact))))
                 (?e . ,(howdy--get-email-link contact))
                 (?E . ,(howdy--get-email-links contact))) 'delete))

(defun howdy--get-backlog (contact &optional at-time)
  "Get the backlog value for a CONTACT at AT-TIME."
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
       (time-to-number-of-days (time-subtract at-time (apply 'encode-time last-contacted)))
       interval)))))

(defun howdy-get-contacts-for-tag (tag)
  "Return all contacts with the given TAG."
  (cl-loop for contact in (org-contacts-db)
           for tags = (cdr (assoc-string "ALLTAGS" (caddr contact)))
           if (save-match-data (string-match (format ":%s:" tag) (or tags "")))
           collect contact))

(defun howdy--get-email-link (contact)
  "Get email link for CONTACT."
  (let ((emails (cdr (assoc-string org-contacts-email-property (caddr contact)))))
    (if (and emails (not (equal emails "")))
        (let ((email (car (split-string emails " "))))
          (format "[[mailto:%s][%s]]" email email))
      "")))

(defun howdy--get-email-links (contact)
  "Get email links for CONTACT."
  (let* ((emails (cdr (assoc-string org-contacts-email-property (caddr contact)))))
    (if (and emails (not (equal emails "")))
        (mapconcat
         (lambda (email) (format "[[mailto:%s][%s]]" email email))
         (split-string emails " ")
         " ")
      "")))

(defun howdy--sorted-backlog-contacts (contacts)
  "Sort CONTACTS by backlog duration (desc) followed by howdy interval (asc)."
  (sort contacts
        (lambda (x y)
          (let ((backlog-x (cdr (assoc "BACKLOG" (caddr x))))
                (backlog-y (cdr (assoc "BACKLOG" (caddr y))))
                (interval-x (ignore-errors
                              (string-to-number
                               (cdr (assoc howdy-interval-property (caddr x))))))
                (interval-y (ignore-errors
                              (string-to-number
                               (cdr (assoc howdy-interval-property (caddr y)))))))
            (if (/= backlog-x backlog-y)
                ;; Primary: Sort by backlog (descending)
                (> backlog-x backlog-y)
              ;; Secondary: Sort by howdy interval (ascending)
              (< interval-x interval-y))))))

(defun howdy--startswith (s begin)
  "Check if S begins with BEGIN."
  (org-string-match-p (format "^%s.*$" begin) s))

(defun howdy--agenda-get-info ()
  "Get the `info' for contact at point in an `org-agenda' buffer."
  (let* ((txt (org-no-properties (org-get-at-bol 'txt)))
         (name (when (string-match org-link-bracket-re txt)
                 (match-string 2 txt))))
    (if (not name)
        (error "Could not extract contact name from agenda entry")
      `((:name . ,name)))))

(defun howdy-agenda-contacted (arg)
  "Mark a contact as contacted from an `org-agenda' buffer.
If ARG is non-nil, use the current time as the last contacted date.
Otherwise, prompt the user for a date."
  (interactive "P")
  (let* ((info (howdy--agenda-get-info)))
    (if arg
        (howdy-contacted info)
      (howdy-contacted info (org-read-date nil t nil nil (current-time))))))

(defun howdy-agenda-untrack-contact (no-confirm)
  "Stop tracking the contact at point in `org-agenda' buffer.

Unset HOWDY_INTERVAL and LAST_HOWDY for the contact at point, and
stop tracking the contact via howdy. If NO-CONFIRM is non-nil,
the user is not prompted for confirmation."
  (interactive "P")
  (let* ((info (howdy--agenda-get-info))
         (contact (car (howdy-find-contacts info))))
    (when (and (not no-confirm)
               (not (yes-or-no-p "Stop tracking this contact?")))
      (error "Aborted"))
    (howdy--with-contact
     contact
     (org-delete-property howdy-interval-property)
     (org-delete-property howdy-last-contacted-property))
    (when (bound-and-true-p org--diary-sexp-entry-cache)
      (clrhash org--diary-sexp-entry-cache))))

(defun howdy-clear-backlog (confirm)
  "Clear all backlog contacts by resetting last howdy to now.

If CONFIRM is non-nil, the user is prompted before proceeding."
  (interactive (list (yes-or-no-p "Clear all howdy backlog?")))
  (let ((contacts (howdy--backlog-contacts))
        (time (org-read-date nil t nil nil (current-time))))
    (when confirm
      (cl-loop for contact in contacts
               do (howdy--contacted-contact contact time)))))

(defun howdy-create-tag-buffer (tag)
  "Create buffer to contact people with specified TAG."
  (interactive (list (org-completing-read "Tag: " (howdy-contact-tags))))
  (switch-to-buffer (format "*howdy-%s*" tag))
  (delete-region (point-min) (point-max))
  (org-mode)
  (cl-loop for contact in (howdy-get-contacts-for-tag tag)
           for name = (car contact)
           for jid = (howdy-jabber-get-jabber-id contact)
           for email = (howdy-email-get-primary-email contact)
           do (insert (format "- [ ] %s " name))
           do (org-insert-link nil (format "elisp:(funcall howdy-email-function \"%s\")" email) "Email")
           do (insert " ")
           do (org-insert-link nil (format "elisp:(funcall howdy-jabber-function \"%s\")" jid) "Chat")
           do (insert "\n")))

(defun howdy--shuffle-contacts (contacts)
  "Shuffle a list of CONTACTS randomly."
  (sort contacts
        (lambda (x y) (> (random t) (random t)))))

;;;###autoload
(defun howdy-howdy (&optional format)
  "Return agenda entries for out-of-touch contacts.

FORMAT is a string matching the format specification of
`howdy-agenda-entry-format'."
  (let* ((date (or (bound-and-true-p org-agenda-current-date)
                   (calendar-current-date)))
         (at-time (org-time-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (contacts (howdy--backlog-contacts at-time))
         entries)
    (cond
     ((equal howdy-scheduler 'random)
      (howdy--shuffle-contacts contacts))
     ((equal howdy-scheduler 'backlog)
      (setq contacts (howdy--sorted-backlog-contacts contacts))))
    (setq entries (cl-loop for contact in contacts
                           collect (howdy--format-contact contact format)))
    (if (and (> howdy-max-contacts 0)
             (> (length entries) howdy-max-contacts))
        (cl-subseq entries 0 howdy-max-contacts)
      entries)))

(defun howdy-set-interval (&optional name interval)
  "Set the howdy INTERVAL for a contact.

INTERVAL is the number of days to set as HOWDY_INTERVAL.

If NAME is not provided, the user is prompted interactively."
  (interactive)
  (unless name
    (setq name (org-contacts-completing-read "Name: ")))
  (unless interval
    (setq interval
          (read-number (format "%s (days): " howdy-interval-property)
                       howdy-interval-default)))
  (let ((contact (car (howdy-find-contacts `((:name . ,name))))))
    (if (null contact)
        (error (format "No contact %s found!" name))
      (howdy--with-contact
       contact
       (org-set-property howdy-interval-property (number-to-string interval))))))

;;;###autoload
(defun howdy ()
  "Update last contacted time for the contact.

If TIME is nil, `current-time' is used.

This function can only be called interactively.  Use
`howdy-contacted' for doing stuff programmatically."
  (interactive)
  (let* ((name-or-tag (howdy-completing-read-name-or-tag))
         (time (org-read-date nil t nil nil (current-time)))
         (info `((:name . ,name-or-tag) (:tag . ,name-or-tag))))
    (howdy-contacted info time)))

(provide 'howdy)

;;; howdy.el ends here
