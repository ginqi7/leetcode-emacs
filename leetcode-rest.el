;;; leetcode-rest.el --- Controller for leetcode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: LeetCode
;; Version: 1.0.0
;; URL: https://github.com/ginqi7/leetcode-emacs
;; Package-Requires: ((emacs "29.1") (restclient) (pcache))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `leetcode-rest-login'
;;    LeetCode login, input session and csrftoken.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `leetcode-rest-host'
;;    LeetCode host.
;;    default = "leetcode.cn"
;;  `leetcode-rest-protocol'
;;    LeetCode protocol.
;;    default = "https"
;;  `leetcode-rest-show-detail-msg'
;;    Leetcode show detail in MESSAGE buffer.
;;    default = nil

;;; Code:

(require 'pcache)
(require 'restclient)
(require 'leetcode-object)

(defcustom leetcode-rest-host "leetcode.com" "LeetCode host."
  :type 'string
  :group 'leetcode)

(defcustom leetcode-rest-protocol "https" "LeetCode protocol."
  :type 'string
  :group 'leetcode)

(defcustom leetcode-rest-user-name ""
  "LeetCode username (only required on leetcode.com)."
  :type 'string
  :group 'leetcode)

(defcustom leetcode-rest-show-detail-msg nil "Leetcode show detail in MESSAGE buffer."
  :type 'boolean
  :group 'leetcode)

(defvar leetcode-rest--repository
  (pcache-repository "leetcode")
  "LeetCode Cache repository.")

(defvar leetcode-rest--requests-directory
  (file-name-concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "requests")
  "Requests directory for LeetCode.")

(defvar leetcode-rest--response-template ":leetcode-rest--%s-response"
  "The template for LeetCode response variable.")

(defvar leetcode-rest-csrftoken
  (pcache-get leetcode-rest--repository 'csrftoken)
  "LeetCode csrftoken.")

(defvar leetcode-rest-session
  (pcache-get leetcode-rest--repository 'seesion)
  "LeetCode session.")

(defun leetcode-rest--build-variables (&optional variable-alist)
  "Build rest variables.
VARIABLE-ALIST: alist for variable."
  (append
   `((":leetcode-rest--protocol" . ,leetcode-rest-protocol)
     (":leetcode-rest--host" . ,leetcode-rest-host)
     (":leetcode-rest--session" . ,leetcode-rest-session)
     (":leetcode-rest--csrftoken" . ,leetcode-rest-csrftoken))
   variable-alist))

(defun leetcode-rest--extend-restclient-variable ()
  "Leetcode Rest Extend Restclient Variable."
  (dolist (key-value restclient-var-overrides)
    (goto-char (point-min))
    (when (search-forward (format "${%s}" (car key-value)) nil t)
      (replace-match (cdr key-value)))))

(defun leetcode-rest--wait-for (func &optional delay)
  "Wait for apply the `func' return t.
FUNC: a function loop apply.
DELAY: the delay time to apply the FUNC."
  (unless delay (setq delay 0.1))
  (catch 'break
    (dotimes (number 100)
      (when (funcall func) (throw 'break number))
      (sit-for delay nil))))

(defun leetcode-rest--get-response (variable-name)
  "Get the response variable synchronously.
VARIABLE-NAME: a variable name in `restclient-var-overrides.'"
  (leetcode-rest--wait-for
   (lambda ()
     (alist-get variable-name restclient-var-overrides nil nil #'string=)))
  (alist-get variable-name restclient-var-overrides nil nil #'string=))

(defun leetcode-rest--list-request-files ()
  "List all requests files."
  (directory-files leetcode-rest--requests-directory t "\\.rest"))

(defun leetcode-rest--request-file ()
  "Get request file by `leetcode-rest-host'."
  (directory-files leetcode-rest--requests-directory t (concat leetcode-rest-host "\\.rest")))

(defun leetcode-rest--search-request (&optional rest-name)
  "Search request by REST-NAME."
  (search-forward-regexp
   (format "^# ?\\(%s\\)$" (or rest-name ".*"))
   nil t))

(defun leetcode-rest-login ()
  "LeetCode login, input session and csrftoken."
  (interactive)
  (let ((session (read-string "Please input leetcode session: "))
	(csrftoken (read-string "Please input leetcode csrftoken: ")))
    (pcache-put leetcode-rest--repository 'seesion session)
    (pcache-put leetcode-rest--repository 'csrftoken csrftoken)
    (setq leetcode-rest-session (pcache-get leetcode-rest--repository 'seesion))
    (setq leetcode-rest-csrftoken
	  (pcache-get leetcode-rest--repository 'csrftoken))))

(defun leetcode-rest-problemset-question-list (&optional skip limit filters)
"Leetcode Rest Problemset Question List.
SKIP: number.
LIMIT: number.
FILTERS: filter json."
  (leetcode-rest-send-request "problemset-question-list"
			      `((":leetcode-rest--limit" . ,(number-to-string limit))
				(":leetcode-rest--skip" . ,(number-to-string skip))
				(":leetcode-rest--filters" . ,filters))))

(defun leetcode-rest-question-content (title-slug)
"Leetcode Rest Question Content.
TITLE-SLUG: title slug."
  (leetcode-rest-send-request "question-content"
			      `((":leetcode-rest--title-slug" . ,title-slug))))

(defun leetcode-rest-question-editor-data (title-slug language)
"Leetcode Rest Question Editor Data.
TITLE-SLUG: title slug.
LANGUAGE: language sovling problem."
  (leetcode-rest-send-request "question-editor-data"
			      `((":leetcode--title-slug" . ,title-slug)
				(":leetcode--language" . ,language))))

(defun leetcode-rest-question-tag-list ()
  "Leetcode Rest Question Tag List."
  (leetcode-rest-send-request "question-tag-list"))

(defun leetcode-rest-send-request (request-name &optional params-alist)
"Leetcode Rest Send Request.
REQUEST-NAME: request name.
PARAMS-ALIST: params alist."
  (let ((restclient-var-overrides (leetcode-rest--build-variables params-alist))
	(inhibit-message (not leetcode-rest-show-detail-msg))
	(response))
    (catch 'break
      (dolist (file (leetcode-rest--request-file))
	(with-temp-buffer
	  (insert-file-contents file)
	  (leetcode-rest--extend-restclient-variable)
	  (goto-char (point-min))
	  (when (leetcode-rest--search-request request-name)
	    (restclient-http-send-current nil t t)
	    (setq response
		  (leetcode-rest--get-response (format leetcode-rest--response-template request-name)))
	    (throw 'break t)))))
    response))

(defun leetcode-rest-session-progress ()
  "Send reset to get session-progress."
  (leetcode-rest-send-request "session-progress"
			      `((":leetcode-rest--user-name" . ,leetcode-rest-user-name))))

(defun leetcode--rest-check-submission-state (submission-id)
"Leetcode Rest Check Submission State.
SUBMISSION-ID: number string."
  (let ((submission-status-json
	 (leetcode-rest-send-request "submission-status" `((":leetcode--submission-id" . ,submission-id)))))
    (string-match "SUCCESS" submission-status-json)))

(defun leetcode-rest-submit (title-slug code language question-id)
  "Leetcode Rest Submit.
TITLE-SLUG: title slug.
CODE: code.
LANGUAGE: language sovling problem.
QUESTION-ID: question id."
  (let ((submission-id
	 (leetcode-rest-send-request "submit"
				     `((":leetcode-rest--title-slug" . ,title-slug)
				       (":leetcode-rest--code" .
					,(string-replace "\t" "\\t"
							 (string-replace "\n" "\\n" code)))
				       (":leetcode-rest--language" . ,language)
				       (":leetcode-rest--question-id" .
					,(number-to-string question-id))))))

    (leetcode-rest--wait-for
     (lambda ()
       (leetcode--rest-check-submission-state submission-id))
     0.5)
    (leetcode-rest-send-request "submission-status" `((":leetcode--submission-id" . ,submission-id)))
    ))

(defun leetcode-rest-graphql-query-type (type)
"Leetcode Rest Graphql Query TYPE Information."
  (leetcode-rest-send-request "graphql-query-type" `((":leetcode-rest--graphql-type" . ,type))))

(provide 'leetcode-rest)
;;; leetcode-rest.el ends here

