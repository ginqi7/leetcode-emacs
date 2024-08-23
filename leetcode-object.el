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

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)

(defvar leetcode--tags nil "The LeetCode problem tag list.")

(defclass leetcode-page-info ()
  ((total-num :initarg :total-num :type integer)
   (page-num :initarg :page-num :type integer)
   (skip :initarg :skip :type integer)
   (limit :initarg :limit :type integer)
   (has-more :initarg :has-more :type boolean))
  :documentation "LeetCode query page information class.")

(defclass leetcode-problem ()
  ((acceptance-rate :initarg :acceptance-rate :type float)
   (difficulty :initarg :difficulty :type string)
   (paid :initarg :paid :type boolean)
   (status :initarg :status :type string :initform "")
   (id :initarg :id :type string)
   (title :initarg :title :type string)
   (title-cn :initarg :title-cn :type string)
   (title-slug :initarg :title-slug :type string)
   (tags :initarg :tags :type string))
  :documentation "LeetCode Problem class.")

(defclass leetcode-problem-list ()
  ((page-info :initarg :page-info :type leetcode-page-info)
   (problems
    :initarg :problems
    :type cons
    :custom ((:subtype . leetcode-problem))))
  :documentation "LeetCode Problem list class.")

(defclass leetcode-query-info ()
  ((difficulty :initarg :difficulty :type string :custom ((:slot-makeunbound-null . t)))
   (status :initarg :status :type string :custom ((:slot-makeunbound-null . t)))
   (tags :initarg :tags :type cons :custom ((:slot-makeunbound-null . t)))
   (keywords :initarg :keywords :type string :custom ((:slot-makeunbound-null . t)
						       (:json-output . "searchKeywords"))))
  documentation "LeetCode query information class.")

(defclass leetcode-question-count ()
  ((easy :initarg :easy :type integer)
   (medium :initarg :medium :type integer)
   (hard :initarg :hard :type integer))
  :documentation "LeetCode question count")

(defclass leetcode-session-progress ()
  ((accepted :initarg :accepted :type leetcode-question-count)
   (failed :initarg :failed :type leetcode-question-count)
   (untouched :initarg :untouched :type leetcode-question-count)
   (total-submissions :initarg :total-submissions :type integer)
   (accepted-submissions
    :initarg :accepted-submissions
    :type integer)
   (acceptance-rate :initarg :acceptance-rate :type float))
  :documentation "LeetCode session progress class.")

(defclass leetcode-submission-status ()
  ((state :initarg :status-msg :type string)
   (status-msg :initarg :status-msg :type string)
   (compile-error :initarg :compile-error :type string)
   (total-correct :initarg :total-correct :type integer)
   (total-testcases :initarg :total-testcases :type integer)
   (runtime-percentile :initarg :runtime-percentile :type float)
   (status-memory :initarg :status-memory :type string)
   (memory-percentile :initarg :memory-percentile :type float)
   (pretty-lang :initarg :pretty-lang :type string)
   (last-testcase :initarg :last-testcase :type string)
   (expected-output :initarg :expected-output :type string)
   (code-output :initarg :code-output :type string)
   (std-output :initarg :std-output :type string))
  :documentation "LeetCode submission status class.")

(defclass leetcode-tag ()
  ((name :initarg :name :type string)
   (id :initarg :id :type string)
   (slug :initarg :slug :type string)
   (type :initarg :type :type string))
  :documentation "LeetCode tag class.")

(defun leetcode--oref-with-default (object slot &optional default-value)
  "Get the value of SLOT from OBJECT, or return DEFAULT-VALUE if SLOT is not set."
  (if (and (slot-boundp object slot)
	   (not (and (stringp (eieio-oref object slot))
		     (string-blank-p (eieio-oref object slot)))))
      (eieio-oref object slot)
    default-value))

(defun leetcode-object--class-slot-name (class-slot)
  "Get Class Slot Name.
CLASS-SLOT: a class slot."
  (eieio-slot-descriptor-name class-slot))

(defun leetcode-object--class-slot-option (class-slot option)
  "Get Class Slot option.
CLASS-SLOT: a class slot.
OPTION: an option in custom."
  (alist-get
   option
   (alist-get
    :custom
    (cl-struct-slot-value 'cl-slot-descriptor 'props class-slot))))

(defun leetcode-object--class-slot-subtype (class-slot)
  "Get Class Slot subtype.
CLASS-SLOT: a class slot."
  (leetcode-object--class-slot-option class-slot :subtype))

(defun leetcode-object--class-slot-type (class-slot)
  "Get Class Slot type.
CLASS-SLOT: a class slot."
  (cl-struct-slot-value 'cl-slot-descriptor 'type class-slot))

(defun leetcode-object--json-get (key json-obj)
  "Get value from JSON-OBJ by KEY."
  (gethash key json-obj))

(defun leetcode-object--json-check-key (key json-obj)
  "Check if the JSON-OBJ contain the KEY."
  (let ((sentinel (make-symbol "sentinel")))
    (not (eq sentinel (gethash key json-obj sentinel)))))

(defun leetcode-object--json-obj-convert (class json-obj)
  "Covert json-obj to class instance.
CLASS: a class symbol.
JSON-OBJ: a json-obj needed to convert."
  (let ((obj (make-instance class))
       (class-slots (eieio-class-slots class)))
   (dolist (class-slot class-slots)
     (let* ((class-slot-name-symbol (leetcode-object--class-slot-name class-slot))
	   (class-slot-name-str (symbol-name class-slot-name-symbol))
	   (class-slot-type (leetcode-object--class-slot-type class-slot))
	   (class-slot-subtype (leetcode-object--class-slot-subtype class-slot))
	   (slot-makeunbound-null-p (leetcode-object--class-slot-option class-slot :slot-makeunbound-null))
	   (property-value (leetcode-object--json-get class-slot-name-str json-obj)))
       (when (leetcode-object--json-check-key class-slot-name-str json-obj)
	 (when (and (null property-value) slot-makeunbound-null-p)
	   (slot-makeunbound json-obj class-slot))
	 (when (not (null property-value))
	   (eieio-oset obj class-slot-name-symbol
		      (cond
		    ((class-p class-slot-type) (leetcode-object--json-obj-convert class-slot-type property-value))
		    ((and (eq 'cons class-slot-type) (class-p class-slot-subtype))
		     (mapcar (lambda (item)
				 (leetcode-object--json-obj-convert class-slot-subtype item))
			    property-value))
		    (t property-value)))
	   ))))
   obj))

(defun leetcode-object--json-parse-string (json-str)
  "Parse Json String.
JSON-STR: a json string."
  ;; (json-parse-string json-str
  ;; 			 :array-type 'list
  ;; 			 :null-object 'nil
  ;; 			 :false-object 'nil)
  (condition-case e
      (json-parse-string json-str
			 :array-type 'list
			 :null-object 'nil
			 :false-object 'nil)
    (json-parse-error (error "%s. A JSON error has occurred.  Please check the *HTTP Response* buffer for details. This may be due to expired cookies" e)))
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

(defun leetcode-object-json-convert (class json-str)
  "Covert json string to class instance.
CLASS: a class symbol.
JSON-STR: a json string."
  (let ((json-obj (leetcode-object--json-parse-string json-str)))
    (if (consp json-obj)
	(mapcar (lambda (obj)
		  (leetcode-object--json-obj-convert class obj))
		json-obj)
      (leetcode-object--json-obj-convert class json-obj))))

(defun leetcode-object-to-hashtable (obj)
  "Convert an Object To a Hashtable.
OBJ: an object."
  (let ((hashtable (make-hash-table :test 'equal))
	(class-slots (eieio-class-slots (eieio-object-class obj)))
	(slot-name-symbol))
    (mapc
     (lambda (class-slot)
       (setq slot-name-symbol (leetcode-object--class-slot-name class-slot))
       (when (slot-boundp obj slot-name-symbol)
	 (puthash
	  (if (leetcode-object--class-slot-option class-slot :json-output)
	      (leetcode-object--class-slot-option class-slot :json-output)
	    (symbol-name slot-name-symbol))
	  (if (eieio-object-p (eieio-oref obj slot-name-symbol))
	      (leetcode-object-to-hashtable
	       (eieio-oref obj slot-name-symbol))
	      (eieio-oref obj slot-name-symbol))

	  hashtable)))
     class-slots)
    hashtable))

(defun leetcode-object-to-json-string (obj)
  "Convert Object To Json String.
OBJ: An Object."
  (json-encode (leetcode-object-to-hashtable obj)))

(provide 'leetcode-object)
;;; leetcode-object.el ends here

