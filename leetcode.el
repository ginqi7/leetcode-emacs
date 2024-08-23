;;; leetcode.el --- Leetcode plugin for Emacs.       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

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
;;  `leetcode'
;;    Leetcode main interactive command.
;;  `leetcode-get-current-description'
;;    Leetcode get description of current problem.
;;  `leetcode-show'
;;    Show leetcode programs message and download file.
;;  `leetcode-show-next'
;;    Show the next leetcode programs after the current buffer.
;;  `leetcode-submit'
;;    Submit the current buffer solution.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `leetcode-language'
;;    Coding language for leetcode.
;;    default = "go"
;;  `leetcode-path'
;;    The direcotry to save leetcode code files.
;;    default = "~/.leetcode/code"

;;; Code:

(require 'leetcode-object)
(require 'leetcode-render)
(require 'leetcode-rest)

(defcustom leetcode-language "go"
  "Coding language for leetcode."
  :type 'string
  :group 'leetcode)

(defcustom leetcode-path "~/.leetcode/code"
  "The direcotry to save leetcode code files."
  :type 'string
  :group 'leetcode)

(defconst leetcode--lang-extension-alist
  '(("c" . "c") ("cpp" . "cpp") ("csharp" . "cs")
    ("dart" . "dart") ("elixir" . "ex") ("erlang" . "erl")
    ("golang" . "go") ("java" .  "java") ("javascript" . "js")
    ("kotlin" . "kt") ("php" . "php") ("python" . "py") ("python3" . "py")
    ("racket" . "rkt") ("ruby" . "rb") ("rust" . "rs")
    ("scala" . "scala") ("swift" . "swift") ("typescript" . "ts"))
  "LeetCode programming language extension alist.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift")

(defvar leetcode--difficulties '("EASY" "MEDIUM" "HARD" "")
  "The LeetCode problem difficulty list.")

(defvar leetcode--statuses '("NOT_STARTED" "TRIED" "AC" "")
  "The LeetCode problem status list.")

(defun leetcode ()
  "Leetcode main interactive command."
  (interactive)
  (leetcode--query 0 (window-body-height) (make-instance 'leetcode-query-info)))

(defun leetcode--get-current-buff-num ()
  "Convert current buffer name to number."
  (car (split-string (buffer-name) "\\.")))

(defun leetcode--get-current-buff-problem-name ()
  "Convert current buffer name to problem name."
  (nth 1 (split-string (buffer-name) "\\.")))

(defun leetcode--get-file-name-num (name)
  "Convert file name to get first number.
NAME is leetcode problem's file name."
  (string-to-number (cl-first (split-string name "\\."))))

(defun leetcode--get-language-extension (language)
  "Leetcode Get Language Extension.
LANGUAGE: language for solving problem."
  (let ((extention (alist-get language leetcode--lang-extension-alist nil nil #'equal)))
    (if extention extention (error "Not Support: %s" language))))

(defun leetcode--query (skip limit query-info-ins)
  "Query LeetCode problems.
SKIP: number for skip.
LIMIT number for limit.:
QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (let* ((query-info-json
	  (leetcode-object-to-json-string query-info-ins))
	 (problemset-question-list-json
	  (leetcode-rest-problemset-question-list skip limit query-info-json))
	 (session-progress-json (leetcode-rest-session-progress))
	 (session-progress-ins (leetcode-object-json-convert 'leetcode-session-progress session-progress-json)))
    (leetcode-render--page
     (leetcode-object-json-convert 'leetcode-problem-list problemset-question-list-json)
     query-info-ins
     session-progress-ins)))

(defun leetcode--query-by-difficulty (leetcode-query-info-ins)
  "Add difficulty to leetcode-query-info-ins and Query problems.
LEETCODE-QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (let ((difficulty (completing-read "Select a difficulty: " leetcode--difficulties nil t)))
    (if (string= difficulty "")
	(slot-makeunbound leetcode-query-info-ins 'difficulty)
      (eieio-oset leetcode-query-info-ins 'difficulty difficulty))
    (leetcode--query 0 (window-body-height) leetcode-query-info-ins)))

(defun leetcode--query-by-keywords (leetcode-query-info-ins)
  "Add keywords to leetcode-query-info-ins and Query problems.
LEETCODE-QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (let ((keywords (read-string "Input some keywords: ")))
    (if (string-blank-p keywords)
	(slot-makeunbound leetcode-query-info-ins 'keywords)
      (eieio-oset leetcode-query-info-ins 'keywords keywords))
    (leetcode--query 0 (window-body-height) leetcode-query-info-ins)))

(defun leetcode--query-by-status (leetcode-query-info-ins)
  "Add status to leetcode-query-info-ins and Query problems.
LEETCODE-QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (let ((status (completing-read "Select a status: " leetcode--statuses nil t)))
    (if (string= status "")
	(slot-makeunbound leetcode-query-info-ins 'status)
      (eieio-oset leetcode-query-info-ins 'status status))
    (leetcode--query 0 (window-body-height) leetcode-query-info-ins)))

(defun leetcode--query-by-tags (leetcode-query-info-ins)
  "Add tags to leetcode-query-info-ins and Query problems.
LEETCODE-QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (unless leetcode--tags
    (setq leetcode--tags
	  (leetcode-object-json-convert 'leetcode-tag (leetcode-rest-question-tag-list))))
  (let ((selected-tags
	 (leetcode-object-completing-read-multiple "Select some tags: " leetcode--tags 'name)))
    (if selected-tags
	(eieio-oset leetcode-query-info-ins 'tags
		    (mapcar
		     (lambda (tag) (eieio-oref tag 'slug))
		     selected-tags))
      (slot-makeunbound leetcode-query-info-ins 'tags))
    (leetcode--query 0 (window-body-height) leetcode-query-info-ins)))

(defun leetcode--query-by-next (leetcode-page-info-ins leetcode-query-info-ins)
  "Leetcode Query Next Page.
LEETCODE-PAGE-INFO-INS: an object make instance by class \='leetcode-page-info.
LEETCODE-QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (let ((skip
	 (eieio-oref leetcode-page-info-ins 'skip))
	(limit (eieio-oref leetcode-page-info-ins 'limit))
	(has-more (eieio-oref leetcode-page-info-ins 'has-more)))
    (if has-more
	(leetcode--query
	 (+ skip limit)
	 limit leetcode-query-info-ins)
      (message "There is no more next page."))))

(defun leetcode--query-by-previous (leetcode-page-info-ins leetcode-query-info-ins)
  "Leetcode Query Previous Page.
LEETCODE-PAGE-INFO-INS: an object make instance by class \='leetcode-page-info.
LEETCODE-QUERY-INFO-INS: an object make instance by class \='leetcode-query-info."
  (let ((skip
	 (eieio-oref leetcode-page-info-ins 'skip))
	(limit (eieio-oref leetcode-page-info-ins 'limit)))
    (if (>= (- skip limit) 0)
	(leetcode--query
	 (- skip limit)
	 limit leetcode-query-info-ins)
      (message "There is no a previous page."))))

(defun leetcode-get-current-description ()
  "Leetcode get description of current problem."
  (interactive)
  (leetcode-show (string-to-number (leetcode--get-current-buff-num))))

(defun leetcode-open-problem-code (problem)
  "Leetcode Open Problem Code.
PROBLEM: Object instance for class \='leetcode-problem."
  (let* ((base-file-name
	  (concat
	   (eieio-oref problem 'id)
	   "."
	   (eieio-oref problem 'title-slug)))
	 (file-name
	  (concat
	   (file-name-concat leetcode-path base-file-name)
	   "."
	   (leetcode--get-language-extension leetcode-language))))

    (unless (file-exists-p  file-name)
      (find-file file-name)
      (insert
       (leetcode-rest-question-editor-data
	(eieio-oref problem 'title-slug)
	leetcode-language)))
    (find-file file-name)))

(defun leetcode--show (problem)
  "Show problem by \='leetcode-problem ojbect.
PROBLEM: an object instance for \='leetcode-problem."
  (leetcode-open-problem-code problem)
  (leetcode-render-question-content
   (leetcode-rest-question-content (eieio-oref problem 'title-slug))))

(defun leetcode-show (n)
  "Show leetcode programs message and download file.
N is a leetcode program number."
  (interactive "nProgram Number: ")
  (let ((problem
	 (car
	  (eieio-oref
	   (leetcode-object-json-convert
	    'leetcode-problem-list
	    (leetcode-rest-problemset-question-list
	     (1- n)
	     1
	     (leetcode-object-to-json-string
	      (make-instance 'leetcode-query-info))))
	   'problems))))
    (leetcode--show problem)))

(defun leetcode-show-next ()
  "Show the next leetcode programs after the current buffer."
  (interactive)
  (leetcode-show
   (+ 1 (string-to-number (leetcode--get-current-buff-num)))))

(defun leetcode-submit ()
  "Submit the current buffer solution."
  (interactive)
  (let ((id (string-to-number (leetcode--get-current-buff-num)))
	(title-slug (nth 1 (string-split (buffer-name) "\\."))))
    (leetcode-render--submission-status
     (leetcode-object-json-convert 'leetcode-submission-status
				   (leetcode-rest-submit title-slug
							 (buffer-string)
							 leetcode-language id)))))
(provide 'leetcode)
;;; leetcode.el ends here

