;;; howdy.el --- A library to help you keep in touch with people.

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
;; the `howdy-contacted' function.

;; To get agenda entries for out-of-touch contacts, use `howdy-howdy'. For
;; example, add an entry like the one below to one of your org-contacts-files.
;;
;; : * Keep in touch
;; : %%(howdy-contacts)

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

(defcustom howdy-agenda-entry-format "Say Howdy: %l (%p) (%e)"
  "Format of the \"say howdy!\" agenda entry.
The following replacements are available:

  %h - Heading name
  %l - Link to the heading
  %p - Phone number
  %e - Email"
  :type 'string
  :group 'howdy)

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

(defun howdy--backlog-contacts ()
  "Returns a list of contacts who need to be contacted."
  (loop for contact in (org-contacts-db)
        if (howdy--backlog-contact-p contact)
        collect contact))

(defun howdy--contacted (info &optional time)
  "Update last contacted time for the contact.

If TIME is nil, `current-time' is used."
  (let ((contact (howdy--find-contact info)))
    (unless time (setq time (current-time)))
    (when contact (howdy--contacted-contact contact time))))

(defun howdy--contacted-contact (contact time)
  "Update last contacted time for the contact."
  (howdy--with-contact
   contact
   (let (old-time)
     (setq old-time
           (ignore-errors
             (org-parse-time-string
              (cdr (assoc-string howdy-last-contacted-property (org-entry-properties))))))
     (if (or (not old-time) (time-less-p (apply 'encode-time old-time) time))
         (org-set-property
          howdy-last-contacted-property
          (format-time-string
           (org-time-stamp-format 'long t) time))
       (message "Not updating with older timestamp.")))))

(defun howdy--find-contact (info)
  "Find the contact using the given info."
  ;; FIXME: Allow phone/nick/IRC/twitter/etc.
  (let ((name (cdr (assoc :name info)))
        (email (cdr (assoc :email info)))
        props)
    (when email
      (setq props `("EMAIL" . ,email)))
    (org-contacts-filter (concat "^" name "$") nil props)))

(defmacro howdy--with-contact (contact &rest body)
  "Eval the BODY with point at the given contact."
  `(let ((marker (cadar contact)))
     (with-current-buffer (marker-buffer marker)
       (save-excursion
         (save-restriction
           (goto-char marker)
           ,@body))
       (save-buffer))
     (run-with-idle-timer 1 nil 'org-contacts-db)))

(defun howdy-contacted ()
  "Update last contacted time for the contact.

If TIME is nil, `current-time' is used.

This function can only be called interactively.  Use
`howdy--contacted' for doing stuff programmatically."
  (interactive)
  (let* ((name (org-contacts-completing-read "Name: "))
         (time (org-read-date t t nil "Time: " (current-time)))
         (contact (howdy--find-contact `((:name . ,name)))))
    (if contact (howdy--contacted-contact contact time)
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
          collect
          (format-spec format
                       `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                         (?h . ,(car contact))
                         (?p . ,(cdr (assoc-string org-contacts-tel-property (caddr contact))))
                         (?e . ,(cdr (assoc-string org-contacts-email-property (caddr contact)))))))))


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
  (let ((contact (org-contacts-filter (concat "^" name "$"))))
    (if (null contact)
        (error (format "No contact %s found!" name))
      (howdy--with-contact
       contact
       (org-set-property howdy-interval-property (number-to-string interval))))))

(provide 'howdy)

;;; howdy.el ends here
