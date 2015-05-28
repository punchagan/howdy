(require 'howdy)

(defconst howdy-tests-contacts-data "
#+STARTUP: showeverything indent
* John Doe
:PROPERTIES:
:EMAIL: john.doe@example.net
:PHONE: 999-999-999
:END:
")

(defmacro with-howdy-test-setup (&rest body)
  `(let* ((org-contacts-file (make-temp-file "contacts" nil ".org"))
          (org-contacts-files (list org-contacts-file))
          (old-org-contacts-last-update org-contacts-last-update))
     (with-temp-buffer
       (insert howdy-tests-contacts-data)
       (write-file org-contacts-file))
     (unwind-protect ,@body
       (kill-buffer (find-buffer-visiting org-contacts-file)))))

(ert-deftest should-find-contact-by-name ()
  (with-howdy-test-setup
   (let ((name "John Doe"))
     (should (string=
              name
              (let ((contact (howdy--find-contact `((:name . ,name)))))
                (car contact)))))))

(ert-deftest should-not-find-contact ()
  (with-howdy-test-setup
   (let ((name "Paul"))
     (should (null
              (let ((contact (howdy--find-contact `((:name . ,name)))))
                contact))))))

(ert-deftest should-find-contact-by-email ()
  (with-howdy-test-setup
   (let ((email "john.doe@example.net"))
     (should (string=
              email
              (let ((contact (howdy--find-contact `((:email . ,email)))))
                (cdr (assoc-string "EMAIL" (caddr contact)))))))))


(ert-deftest should-set-interval ()
  (with-howdy-test-setup
   (let ((interval 30)
         (name "John Doe"))
     (howdy-set-interval name interval)
     (should (string=
              (number-to-string interval)
              (let* ((org-contacts-last-update nil)
                     (contact (howdy--find-contact `((:name . ,name)))))
                (cdr (assoc-string howdy-interval-property (caddr contact)))))))))

(ert-deftest should-set-contacted ()
  (with-howdy-test-setup
   (let* ((name "John Doe")
          (timestamp "[2015-05-28 Thu]")
          (time (apply 'encode-time (org-parse-time-string timestamp)))
          (info `((:name . ,name))))
     (howdy--contacted info time)
     (should (string=
              timestamp
              (let* ((org-contacts-last-update nil)
                     (contact (howdy--find-contact info)))
                (cdr (assoc-string howdy-last-contacted-property (caddr contact)))))))))

(ert-deftest should-not-set-older-timestamp ()
  (with-howdy-test-setup
   (let* ((name "John Doe")
          (timestamp "[2015-05-28 Thu]")
          (old-timestamp "[2015-01-01 Thu]")
          (time (apply 'encode-time (org-parse-time-string timestamp)))
          (old-time (apply 'encode-time (org-parse-time-string timestamp)))
          (info `((:name . ,name))))
     (howdy--contacted info time)
     (howdy--contacted info old-time)
     (should (string=
              timestamp
              (let* ((org-contacts-last-update nil)
                     (contact (howdy--find-contact info)))
                (cdr (assoc-string howdy-last-contacted-property (caddr contact)))))))))

(ert-deftest should-update-timestamp ()
  (with-howdy-test-setup
   (let* ((name "John Doe")
          (timestamp "[2015-05-28 Thu]")
          (old-timestamp "[2015-01-01 Thu]")
          (time (apply 'encode-time (org-parse-time-string timestamp)))
          (old-time (apply 'encode-time (org-parse-time-string timestamp)))
          (info `((:name . ,name))))
     (howdy--contacted info old-time)
     (howdy--contacted info time)
     (should (string=
              timestamp
              (let* ((org-contacts-last-update nil)
                     (contact (howdy--find-contact info)))
                (cdr (assoc-string howdy-last-contacted-property (caddr contact)))))))))

(ert-deftest should-not-update-with-close-timestamp ()
  (with-howdy-test-setup
   (let* ((name "John Doe")
          (timestamp "[2015-05-28 Thu]")
          (new-timestamp "[2015-05-28 Thu 11:10]")
          (time (apply 'encode-time (org-parse-time-string timestamp)))
          (new-time (apply 'encode-time (org-parse-time-string new-timestamp)))
          (info `((:name . ,name))))
     (howdy--contacted info time)
     (howdy--contacted info new-time)
     (should (string=
              timestamp
              (let* ((org-contacts-last-update nil)
                     (contact (howdy--find-contact info)))
                (cdr (assoc-string howdy-last-contacted-property (caddr contact)))))))))

(ert-deftest should-show-howdy-pending-contacts ()
  (with-howdy-test-setup
   (let* ((name "John Doe")
          (timestamp "[2015-01-01 Thu]")
          (time (apply 'encode-time (org-parse-time-string timestamp)))
          (info `((:name . ,name)))
          (john-doe (howdy--find-contact info))
          (msg (howdy--format-contact john-doe)))
     (howdy-set-interval name howdy-interval-default)
     (howdy--contacted info time)
     (should (let* ((org-contacts-last-update nil))
               (member msg (howdy-howdy)))))))

(ert-deftest should-not-show-update-contacts ()
  (with-howdy-test-setup
   (let* ((name "John Doe")
          (info `((:name . ,name)))
          (john-doe (howdy--find-contact info))
          (msg (howdy--format-contact john-doe)))
     (howdy-set-interval name howdy-interval-default)
     (howdy--contacted info)
     (should (let* ((org-contacts-last-update nil))
               (null (member msg (howdy-howdy))))))))
