(defpackage #:test-grid-admin
  (:use :cl)
  (:export #:import-test-result-emails
           #:delete-imported-emails))

(in-package #:test-grid-admin)

(defun pop-message-uid (pop-connection message-num) 
  (let ((list-of-lists (cl-pop:send-pop-uidl pop-connection message-num)))
    ;; list-of-lists is ((<message-num> <message-uid>)),
    ;; fro example
    ;(("1" "13ee455a2a5ecf39f7bb8311b0ee4955"))
    (second (first list-of-lists))))

(defun pop-message-nums-by-uids (pop-connection uids)
  "Retrieves message numbers for messages with the specified unique identifiers."
  (let ((num-uid-map (cl-pop:send-pop-uidl pop-connection)))
    ;; num-uid-map now list in the form
    ;; ((<message-num> <message-uid>)
    ;;  (<message-num> <message-uid>)
    ;;  ...)
    (mapcar (lambda (uid) 
              (first (or (find uid 
                               num-uid-map 
                               :key #'second 
                               :test #'string=)
                         (error "Message with UID ~A is not found in the mail box." uid))))
            uids)))

(defun del-messages-by-uids (host username password uids)
  (cl-pop:with-pop-connection (conn :host host
                                    :username username
                                    :password password)
    (dolist (msg-num (pop-message-nums-by-uids conn uids))
      (cl-pop:delete-pop-message conn msg-num)))
  (length uids))

(defun save-letter-uids (letter-uids work-dir)
  (test-grid-utils::write-to-file letter-uids (merge-pathnames "letter-uids.txt" work-dir))
  letter-uids)

(defun read-letter-uids (work-dir)
  (test-grid-utils::safe-read-file (merge-pathnames "letter-uids.txt" work-dir)))
  
(defun save-string (str pathname)
  (with-open-file (stream pathname 
                          :direction :output
                          :if-exists :supersede)
    (write-sequence str stream)))

(defun get-attachment (multipart-mime)
  ;; we expect the email to be a multipart mime
  ;; message, and the test results are in the first
  ;; (and the only) attachment.
  (let* ((mime-parts (cl-mime:content multipart-mime))
         (attachment-part (second mime-parts)))
    (cl-base64:base64-string-to-string (cl-mime:content attachment-part))))

(defun get-all-attachments (host username password work-dir)
  "Saves test-results delivered as email attachments in the specified
POP3 mailbox as files on local file system, and returns the list of
these files. UIDs of all the processed POP3 messages are persisted 
using SAVE-LETTER-UIDS function."
  (let ((attachment-files '())
        (message-uids '()))
    (flet ((attachment-str (pop-connection msg-num)
             "Returns the attachment of the pop message with number MSG-NUM."
             (let ((temp-file (merge-pathnames "letter.txt" work-dir)))
               (cl-pop:save-message pop-connection msg-num temp-file)
               (get-attachment (with-open-file (stream temp-file)
                                 (cl-mime:parse-mime stream))))))
      (cl-pop:with-pop-connection (conn :host host
                                        :username username
                                        :password password)
        (dotimes (iterator (cl-pop:message-count conn))
          (let* ((attachment-file-name (format nil "attachement-letter~A.txt" iterator))
                 (attachment-file (merge-pathnames attachment-file-name work-dir))
                 (message-uid (pop-message-uid conn (+ 1 iterator))))
            (save-string (attachment-str conn (1+ iterator)) attachment-file)
            (push attachment-file attachment-files)
            (push message-uid message-uids)))))
    (save-letter-uids message-uids work-dir)
    attachment-files))

;;; Helper to produce a commit message for the imported test results.
;;;
;;; Example commit message:
;;; Test results for quicklisp1 and lisp1, lisp2. Contributed by person1. 
;;; Test results for quicklisp2 and lisp1. Contributed by person1.
;;; Test results for quicklisp1 and lisp2. Contributed by person2.
;;; 
;;; This means we breakdown the test resuls by contributors, 
;;; then for every contributor by quicklisps, and then, 
;;; for evenry contributor-quicklisp, store list of common
;;; lisp implementations.

(defun make-submittions-info-table ()
  (make-hash-table :test 'equal))

(defun add-submittions-info (table contributor lib-world lisp)
  (let* (;; lib-worlds for that contributor
         (lib-world-table (or (gethash contributor table)
                              (make-hash-table :test 'equal)))
         ;; lisps for these lib-world and contributor
         (lisp-table (or (gethash lib-world lib-world-table)
                         (make-hash-table :test 'equal))))
    (setf (gethash contributor table) lib-world-table)
    (setf (gethash lib-world lib-world-table) lisp-table)
    (setf (gethash lisp lisp-table) t)))

(defun print-commit-message (table)
  (dolist (contributor (sort (test-grid-utils::hash-table-keys table) #'string<))
    (let ((lib-world-table (gethash contributor table)))
      (dolist (lib-world (sort (test-grid-utils::hash-table-keys lib-world-table) #'string<))
        (format t "Test results for ~A and ~{~A~^, ~}. "
                lib-world 
                (sort (test-grid-utils::hash-table-keys (gethash lib-world 
                                                           lib-world-table))
                      #'string<))))
    (format t "Contributed by ~A.~%" contributor)))

;;; End commit message helper.

(defun src-dir()
  (asdf:system-relative-pathname :test-grid-admin #P"admin/"))

(defparameter *host* "pop.yandex.ru")
(defparameter *user* "cl-test-grid")
(defparameter *work-dir* (merge-pathnames "../work-dir/admin/mail-import/" 
                                          (src-dir)))

(defun import-test-result-emails (mailbox-password)
  "Reads email notifications about new test results and
adds the results to the db.lisp. Prints suggested
commit message to the *STANDARD-OUTPUT*.

Email message UIDs are saved to a file so that 
the processed emails may be later deleted by 
DELETE-IMPORTED-EMAILS function, after the
db.lisp is reviewed and commited.

Note, that if you run IMPORT-TEST-RESULT-EMAILS
again, the file with message UIDs is overwritten,
so be sure to commit db.lisp and clean mailbox
by DELETE-IMPORTED-EMAILS before repeating 
the procedure."

  (ensure-directories-exist *work-dir*)
  
  (let* ((attachment-files (get-all-attachments *host* *user* mailbox-password *work-dir*))
         (submittions-info (make-submittions-info-table))
         (db (test-grid-data::read-db)) )  
    (dolist (attachment-file attachment-files)
      (let* ((test-run-info (test-grid-utils::safe-read-file attachment-file))
             (descr (getf test-run-info :descr)))      
        (test-grid-data::add-run test-run-info db)
        (add-submittions-info submittions-info 
                              (getf (getf descr :contact) :email)
                              (getf descr :lib-world)
                              (getf descr :lisp))))
    (test-grid-data::save-db db)
    (print-commit-message submittions-info)
    (length attachment-files)))

(defun delete-imported-emails (mailbox-password)
  "Deletes the test result emails processed by
the last invocation of IMPORT-TEST-RESULT-EMAILS
from the POP3 mailbox."
  (del-messages-by-uids *host* *user* mailbox-password 
                        (read-letter-uids *work-dir*)))

