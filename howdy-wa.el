;;; howdy-wa.el --- WhatsApp integration for Howdy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Puneeth Chaganti
;;
;; Author: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Maintainer: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Created: January 19, 2025
;; Modified: January 19, 2025
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; WhatsApp integration for Howdy
;;
;; This package integrates WhatsApp messaging with Howdy. It uses a Python
;; script to send messages via WhatsApp Web.
;;
;;; Code:

(require 'org-contacts)

(defcustom howdy-wa-script "howdy-wa"
  "Path to the Python script for WhatsApp messaging."
  :type 'string
  :group 'howdy)

(defcustom howdy-wa-headless t
  "Whether to run the WhatsApp script in headless mode."
  :type 'boolean
  :group 'howdy)

(defun howdy-wa--run-command-async (args)
  "Run howdy-wa command with ARGS asynchronously."
  (let* ((output-buffer "*Howdy WhatsApp Output*")
         (executable (if howdy-wa-headless
                         (format "%s --headless"
                                 (shell-quote-argument howdy-wa-script))
                       howdy-wa-script))
         (command (format "%s %s" executable args)))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (insert (format "Running: %s\n\n" command)))
    (make-process
     :name "howdy-wa-process"
     :buffer output-buffer
     :command (list shell-file-name shell-command-switch command)
     :sentinel (lambda (process event)
                 (when (memq (process-status process) '(exit signal))
                   (with-current-buffer (process-buffer process)
                     (goto-char (point-max))
                     (insert (format "\nProcess finished with %s.\n" event))
                     (message "Howdy WhatsApp command finished: %s" event))
                   (display-buffer (process-buffer process)))))))

;;;###autoload
(defun howdy-wa-send-message (contact message)
  "Send a WhatsApp MESSAGE to CONTACT using the WhatsApp script."
  (interactive
   (let* ((contact-name (howdy--completing-read-name-or-tag))
          (message (read-string (format "Message for %s: " contact-name))))
     (list contact-name message)))
  (let* ((contact-data (car (howdy--find-contacts `((:name . ,contact)))))
         (phone (cdr (assoc-string org-contacts-tel-property (caddr contact-data)))))
    (if (not phone)
        (message "No phone number found for %s." contact)
      (let ((howdy-args (format "send --contact %s %s"
                                (shell-quote-argument phone)
                                (shell-quote-argument message))))
        (howdy-wa--run-command-async howdy-args)))))

;;;###autoload
(defun howdy-wa-send-message-to-tag (tag text)
  "Send a WhatsApp TEXT message to all contacts associated with TAG."
  (interactive
   (let* ((tag (org-completing-read "Tag: " (howdy--contact-tags)))
          (text (read-string (format "Message for contacts tagged '%s': " tag))))
     (list tag text)))
  (let ((contacts (howdy--get-contacts-for-tag tag)))
    (if (not contacts)
        (message "No contacts found with tag '%s'." tag)
      (let* ((phones
              (delq nil
                    (mapcar (lambda (contact)
                              (cdr (assoc-string org-contacts-tel-property (caddr contact))))
                            contacts)))
             (contacts-args
              (mapconcat (lambda (phone)
                           (format "--contact %s" (shell-quote-argument phone)))
                         phones
                         " "))
             (howdy-args (format "send %s %s"
                                 contacts-args
                                 (shell-quote-argument text))))
        (if (not phones)
            (message "No valid phone numbers found for contacts with tag '%s'." tag)
          (message "Sending message to contacts tagged '%s'..." tag)
          (howdy-wa--run-command-async howdy-args))))))

;;;###autoload
(defun howdy-wa-last-contacted (contact)
  "Check the last contacted time for CONTACT using the Python script."
  (interactive
   (list (howdy--completing-read-name-or-tag)))
  (let* ((contact-data (car (howdy--find-contacts `((:name . ,contact)))))
         (phone (cdr (assoc-string org-contacts-tel-property (caddr contact-data)))))
    (if (not phone)
        (message "No phone number found for %s." contact)
      (let ((howdy-args (format "last-contacted %s"
                                (shell-quote-argument phone))))
        (howdy-wa--run-command-async howdy-args)))))


(provide 'howdy-wa)
;;; howdy-wa.el ends here
