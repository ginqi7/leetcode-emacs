;;; leetcode-object.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)

(defvar leetcode--tags nil
  "The LeetCode problem tag list.")

(defclass leetcode-page-info ()
  ((total)
   (num)
   (hasMore)
   (skip)
   (limit))
  :documentation "LeetCode query page information class.")

(defclass leetcode-problem ()
  ((acRate)
   (difficulty)
   (paidOnly)
   (status)
   (frontendQuestionId)
   (title)
   (titleCn)
   (titleSlug)
   (tags))
  :documentation "LeetCode Problem class.")

(defclass leetcode-problem-list () ((page-info) (problems))
  :documentation "LeetCode Problem list class.")

(defclass leetcode-query-info ()
  ((difficulty)
   (status)
   (tags)
   (searchKeywords))
  :documentation "LeetCode query information class."
  )

(defclass leetcode-session-progress ()
  ((accepted)
   (failed)
   (untouched)
   (allSubNum)
   (acSubNum)
   (acRate))
  :documentation "LeetCode session progress class."
  )

(defclass leetcode-submission-status ()
  ((status_msg)
   (compile_error)
   (total_correct)
   (total_testcases)
   (runtime_percentile)
   (status_memory)
   (memory_percentile)
   (pretty_lang)
   (last_testcase)
   (expected_output)
   (code_output)
   (std_output))
  :documentation "LeetCode submission status class."
  )

(defclass leetcode-tag ()
  ((name)
   (id)
   (slug)
   (type))
  :documentation "LeetCode tag class."
  )

(defun leetcode-object-class-properties (class)
  "List all Properties of an Class.
CLASS: an object class."
  (mapcar
   (lambda (slot)
     (cl-struct-slot-value 'cl-slot-descriptor 'name slot))
   (eieio-class-slots class)))

(defun leetcode-object-completing-read-multiple (prompt obj-list id-key)
"Completing Read Multiple for object list.
PROMPT: an prompt for user inputs.
OBJ-LIST: object list as reading data.
ID-KEY: the property to identify the object."
  (let* ((obj-alist
	  (mapcar
	   (lambda (ins) (cons (eieio-oref ins id-key) ins))
	   obj-list))
	 (selected-keys (completing-read-multiple prompt obj-alist))
	 (selected-classes
	  (mapcar
	   (lambda (key) (alist-get key obj-alist nil nil #'equal))
	   selected-keys)))
    selected-classes))

(defun leetcode-object-copy (obj)
"Copy an object Instance.
OBJ: an object."
  (let* ((class (eieio-object-class obj))
	 (new-copy (make-instance class))
	 (properties (leetcode-object-class-properties class)))
    (dolist (property properties)
      (when (slot-boundp obj property)
	(eieio-oset new-copy property (eieio-oref obj property))))
    new-copy))

(defun leetcode-object-hashtable-convert (class hashtable)
  "Covert hashtable to class instance.
CLASS: a class symbol.
HASHTABLE: a hashtable needed to convert."
  (let ((class-instance (make-instance class))
	(class-slot-name-symbols (leetcode-object-class-properties class)))
    (mapc
     (lambda (slot-name-symbol)
       (when (gethash (symbol-name slot-name-symbol) hashtable)
	 (eieio-oset class-instance
		     slot-name-symbol
		     (gethash
		      (symbol-name slot-name-symbol)
		      hashtable))))
     class-slot-name-symbols)
    class-instance))

(defun leetcode-object-json-convert (class json-str)
  "Covert json string to class instance.
CLASS: a class symbol.
JSON-STR: a json string."
  (leetcode-object-hashtable-convert class (json-parse-string json-str)))

(defun leetcode-object-json-to-problem-list-ins (json-str)
"Convert Json To Problem List Instance.
JSON-STR: a json string."
  (let* ((problem-list-instance
	  (leetcode-object-json-convert 'leetcode-problem-list json-str))
	 (leetcode-problems
	  (leetcode--oref-with-default problem-list-instance 'problems))
	 (leetcode-page-info-hashtable
	  (leetcode--oref-with-default problem-list-instance 'page-info)))
    (eieio-oset problem-list-instance 'problems
		(mapcar
		 (lambda (problem)
		   (leetcode-object-hashtable-convert 'leetcode-problem problem))
		 leetcode-problems))
    (eieio-oset problem-list-instance 'page-info
		(leetcode-object-hashtable-convert 'leetcode-page-info leetcode-page-info-hashtable))

    problem-list-instance))

(defun leetcode-object-to-hashtable (obj)
"Convert an Object To a Hashtable.
OBJ: an object."
  (let ((hashtable (make-hash-table :test 'equal))
	(class-slot-name-symbols
	 (leetcode-object-class-properties (eieio-object-class obj))))
    (mapc
     (lambda (slot-name-symbol)
       (when (leetcode--oref-with-default obj slot-name-symbol)
	 (puthash
	  (symbol-name slot-name-symbol)
	  (leetcode--oref-with-default obj slot-name-symbol)
	  hashtable)))
     class-slot-name-symbols)
    hashtable))

(defun leetcode--oref-with-default (object slot &optional default-value)
  "Get the value of SLOT from OBJECT, or return DEFAULT-VALUE if SLOT is not set."
  (if (slot-boundp object slot)
      (eieio-oref object slot)
    default-value))

(provide 'leetcode-object)
;;; leetcode-object.el ends here

