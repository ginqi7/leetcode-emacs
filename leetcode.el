;;; leetcode.el --- Leetcode plugin for Emacs.       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp, tools

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
(require 'tabulated-list)
(require 'rx)
(require 'transient)

(defvar leetcode-path "~/.leetcode/code")

(defvar leetcode-language "go")

(defvar leetcode-hide-no-auth-problems t)

(defvar leetcode-base-url "https://leetcode.com/problems/")

(defvar leetcode-top100 '("1. Two Sum" "2. Add Two Numbers" "3. Longest Substring Without Repeating Characters" "4. Median of Two Sorted Arrays" "5. Longest Palindromic Substring" "11. Container With Most Water" "13. Roman to Integer" "14. Longest Common Prefix" "15. 3Sum" "17. Letter Combinations of a Phone Number" "19. Remove Nth Node From End of List" "20. Valid Parentheses" "21. Merge Two Sorted Lists" "22. Generate Parentheses" "23. Merge k Sorted Lists" "24. Swap Nodes in Pairs" "25. Reverse Nodes in k-Group" "31. Next Permutation" "32. Longest Valid Parentheses" "33. Search in Rotated Sorted Array" "34. Find First and Last Position of Element in Sorted Array" "35. Search Insert Position" "39. Combination Sum" "41. First Missing Positive" "42. Trapping Rain Water" "45. Jump Game II" "46. Permutations" "48. Rotate Image" "49. Group Anagrams" "51. N-Queens" "53. Maximum Subarray" "54. Spiral Matrix" "55. Jump Game" "56. Merge Intervals" "62. Unique Paths" "64. Minimum Path Sum" "70. Climbing Stairs" "72. Edit Distance" "73. Set Matrix Zeroes" "74. Search a 2D Matrix" "75. Sort Colors" "76. Minimum Window Substring" "78. Subsets" "79. Word Search" "84. Largest Rectangle in Histogram" "94. Binary Tree Inorder Traversal" "98. Validate Binary Search Tree" "101. Symmetric Tree" "102. Binary Tree Level Order Traversal" "104. Maximum Depth of Binary Tree" "105. Construct Binary Tree from Preorder and Inorder Traversal" "114. Flatten Binary Tree to Linked List" "118. Pascal's Triangle" "121. Best Time to Buy and Sell Stock" "124. Binary Tree Maximum Path Sum" "128. Longest Consecutive Sequence" "131. Palindrome Partitioning" "136. Single Number" "138. Copy List with Random Pointer" "139. Word Break" "141. Linked List Cycle" "142. Linked List Cycle II" "146. LRU Cache" "148. Sort List" "152. Maximum Product Subarray" "153. Find Minimum in Rotated Sorted Array" "155. Min Stack" "160. Intersection of Two Linked Lists" "169. Majority Element" "189. Rotate Array" "198. House Robber" "199. Binary Tree Right Side View" "200. Number of Islands" "206. Reverse Linked List" "207. Course Schedule" "208. Implement Trie (Prefix Tree)" "215. Kth Largest Element in an Array" "226. Invert Binary Tree" "230. Kth Smallest Element in a BST" "234. Palindrome Linked List" "236. Lowest Common Ancestor of a Binary Tree" "238. Product of Array Except Self" "239. Sliding Window Maximum" "240. Search a 2D Matrix II" "283. Move Zeroes" "287. Find the Duplicate Number" "295. Find Median from Data Stream" "300. Longest Increasing Subsequence" "322. Coin Change" "347. Top K Frequent Elements" "394. Decode String" "416. Partition Equal Subset Sum" "438. Find All Anagrams in a String" "543. Diameter of Binary Tree" "560. Subarray Sum Equals K" "567. Permutation in String" "704. Binary Search" "739. Daily Temperatures" "994. Rotting Oranges" "1143. Longest Common Subsequence"))

(defvar leetcode-difficulties '("All" "Easy" "Medium" "Hard"))

(defvar leetcode-tags '("Array" "String" "Hash Table" "Dynamic Programming" "Math" "Sorting" "Depth-First Search" "Greedy" "Database" "Breadth-First Search" "Tree" "Binary Search" "Matrix" "Binary Tree" "Two Pointers" "Bit Manipulation" "Stack" "Design" "Heap (Priority Queue)" "Graph" "Simulation" "Backtracking" "Prefix Sum" "Counting" "Sliding Window" "Linked List" "Union Find" "Monotonic Stack" "Ordered Set" "Recursion" "Trie" "Binary Search Tree" "Enumeration" "Divide and Conquer" "Bitmask" "Queue" "Memoization" "Geometry" "Topological Sort" "Segment Tree" "Game Theory" "Hash Function" "Binary Indexed Tree" "Interactive"))

(defun leetcode--buffer-whole-string (buffer)
  "Get String without properties from other buffer.
BUFFER is the buffer name"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun leetcode--get-current-buff-num ()
  "Convert current buffer name to number."
  (car (split-string (buffer-name) "\\.")))

(defun leetcode--get-current-buff-problem-name ()
  "Convert current buffer name to problem name."
  (nth 1 (split-string (buffer-name) "\\.")))

(defun get-file-name-num (name)
  "Convert file name to get first number.
NAME is leetcode problem's file name."
  (string-to-number (cl-first (split-string name "\\."))))

(defun leetcode--parse-leetcode-entry (str)
  "Divide a leetcode entry title into 5 columns.
STR is a leetcode entry title."
  (let ((pattern (rx
                  (opt (group "✔"))
                  (opt (+ " "))
                  "[" (opt (+ " ")) (group (+ digit)) (opt (+ " ")) "] " ; id
                  (group (+? anything)) " "  ; title
                  (group (or "Easy" "Medium" "Hard")) (+ " ") ; difficulty
                  "(" (group (+? anything)) ")")) ; frequency
        (accepted)
        (number)
        (title)
        (difficulty)
        (frequency))
    (when (string-match pattern str)
      (setq accepted (match-string 1 str))
      (setq number (match-string 2 str))
      (setq title (match-string 3 str))
      (setq difficulty (match-string 4 str))
      (setq frequency (match-string 5 str)))
    (list accepted number title difficulty frequency)))

(defun leetcode--entry-filter (lst)
  "Filter leetcode list.
  if 'leetcode-hide-no-auth-problems' is t
  LST is leetcode problem list."
  (when (or (nth 0 lst)
            (nth 1 lst)
            (nth 2 lst)
            (nth 3 lst)
            (nth 4 lst))
    (if leetcode-hide-no-auth-problems (not (string= (car lst) "🔒")) t)))

(defun leetcode--parse-leetcode-list (str)
  "Divide a leetcode entry title into 5 columns.
  STR is a leetcode entry title."
  (seq-filter
   #'leetcode--entry-filter
   (mapcar #'leetcode--parse-leetcode-entry
           (split-string str "\n+" t))))

(defun leetcode-click ()
  "Add click function on leetcode."
  (interactive)
  (leetcode-show (string-to-number (tabulated-list-get-id))))

(defun leetcode--list-all-sync (process signal)
  "Create a new buffer to show all leetcode programs list.
  PROCESS is current running process.
  SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit signal))
    (let* ((mode-buffer (get-buffer-create "*leetcode-list*"))
           (rows
            (leetcode--parse-leetcode-list
             (process-get process 'output))))
      (kill-buffer "leetcode_list_value")
      (switch-to-buffer mode-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (leetcode-problems-mode)
      (leetcode-problems--refresh rows)
      (setq buffer-read-only t))
    (goto-char (point-min))))

(define-derived-mode leetcode-problems-mode tabulated-list-mode "leetcode-list"
  "Major mode for browsing LeetCode problems."
  (setq tabulated-list-format
        (vector
         '("Status" 8 t)
         '("Number" 8 t)
         '("Title" 50 t)
         '("Difficulty" 10 t)
         '("Acceptance" 10 t)))
  (setq tabulated-list-sort-key (cons "Number" nil))
  (tabulated-list-init-header)
  (keymap-set leetcode-problems-mode-map "RET" 'leetcode-click))


(defun leetcode-problems--refresh (data)
  "Refresh the LeetCode problems buffer with DATA."
  (setq tabulated-list-entries
        (mapcar (lambda (row)
                  (list (nth 1 row)
                        (vector (or (nth 0 row) "")
                                (format "%04d" (string-to-number (nth 1 row)))
                                (nth 2 row)
                                (nth 3 row)
                                (nth 4 row))))
                data))
  (tabulated-list-print))

(defun leetcode--pick (n)
  "Run =leetcode pick= to select a leetcode problem.
  N is leetcode number."
  (make-process :name "leetcode pick"
                :buffer "*leetcode-description*"
                :command `("leetcode" "pick" ,(int-to-string n))
                :filter #'leetcode--ansi-color-insertion-filter))

(defun leetcode--edit (n)
  "Run =leetcode edit= to edit a leetcode problem.
  N is leetcode number."
  (shell-command-to-string (format "leetcode edit %s" n))
  (let ((match
         (concat "^"
                 (number-to-string n)
                 "\." ".*" "\." leetcode-language "$")))
    (find-file (car (directory-files leetcode-path 'full match)))
    (leetcode-edit-mode)))

(defun leetcode--ansi-color-insertion-filter (proc string)
  "Parse leetcode command output put it in process' properties.
  PROC is current running process.
  STRING is current process' output."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((output (ansi-color-apply string)))
        (process-put proc 'output
                     (concat (process-get proc 'output) output))
        (insert output)))))

(defun leetcode--exec (action num)
  "Execute leetcode action.
  ACTION is leetcode shell optons.
  NUM is leetcode problem's number."
  (let ((mode-buffer (get-buffer-create "*leetcode-result*")))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (font-lock-mode 1)
    (other-window 1))

  (make-process :name "leetcode exec"
                :buffer "*leetcode-result*"
                :command `("leetcode" ,action ,num)
                :filter #'leetcode--ansi-color-insertion-filter))

(defun leetcode-list-all ()
  "Async Create a new buffer to show all leetcode programs list."
  (interactive)
  (make-process :name "leetcode list"
                :buffer "leetcode_list_value"
                :command '("leetcode" "list")
                :filter #'leetcode--ansi-color-insertion-filter
                :sentinel 'leetcode--list-all-sync))

(defun leetcode--random (process signal)
  "Random open a leetcode problem.
  PROCESS is current running process.
  SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit signal))
    (let* ((mode-buffer (get-buffer-create "*leetcode-list*"))
           (rows
            (leetcode--parse-leetcode-list
             (process-get process 'output)))
           (candidates
            (mapcar
             (lambda (row)
               (format "%s: %s (%s)"
                       (second row)
                       (third row)
                       (nth 3 row)))
             rows))
           (selected-item
            (nth (random (length candidates)) candidates))
           (seleted-num (car (split-string selected-item ":"))))
      (leetcode-show (string-to-number seleted-num))
      (kill-buffer "leetcode_list_value"))))

(defun leetcode--list-all-interactive (process signal)
  "List all leetcode in minibuffer.
  PROCESS is current running process.
  SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit signal))
    (let* ((mode-buffer (get-buffer-create "*leetcode-list*"))
           (rows
            (leetcode--parse-leetcode-list
             (process-get process 'output)))
           (candidates
            (mapcar
             (lambda (row)
               (format "%s: %s (%s)"
                       (second row)
                       (third row)
                       (nth 3 row)))
             rows))
           (selected-item
            (completing-read "select leetcode-problems: " candidates))
           (seleted-num (car (split-string selected-item ":"))))
      (leetcode-show (string-to-number seleted-num))
      (kill-buffer "leetcode_list_value"))))

(defun leetcode-query-all-interactive ()
  "Async interactively select all leetcode programs list and show it."
  (interactive)
  (make-process :name "leetcode list"
                :buffer "leetcode_list_value"
                :command '("leetcode" "list")
                :filter #'leetcode--ansi-color-insertion-filter
                :sentinel 'leetcode--list-all-interactive))

(defun leetcode-random ()
  "Random show a leetcode question."
  (interactive)
  (make-process :name "leetcode list"
                :buffer "leetcode_list_value"
                :command '("leetcode" "list")
                :filter #'leetcode--ansi-color-insertion-filter
                :sentinel 'leetcode--random))

(defun leetcode-filter-by-difficulty ()
  "Async Create a new buffer to query leetcode programs list by difficulty."
  (interactive)
  (let* ((difficulty
          (completing-read "Select Leetcode Difficulty: "
                           leetcode-difficulties))
         (option (downcase (substring difficulty 0 1))))
    (make-process :name "leetcode list"
                  :buffer "leetcode_list_value"
                  :command (list "leetcode" "list" "-q" option)
                  :filter #'leetcode--ansi-color-insertion-filter
                  :sentinel 'leetcode--list-all-sync)))

(defun leetcode-random-by-difficulty ()
  "Select a difficulty and random show a leetcode question."
  (interactive)
  (let* ((difficulty
          (completing-read "Select Leetcode Difficulty: "
                           leetcode-difficulties))
         (option (downcase (substring difficulty 0 1))))
    (make-process :name "leetcode list"
                  :buffer "leetcode_list_value"
                  :command (list "leetcode" "list" "-q" option)
                  :filter #'leetcode--ansi-color-insertion-filter
                  :sentinel 'leetcode--random)))

(defun leetcode-filter-by-difficulty-interactive ()
  "Async interactively select leetcode programs by difficulty and show it."
  (interactive)
  (let* ((difficulty
          (completing-read "Select Leetcode Difficulty: "
                           leetcode-difficulties))
         (option (downcase (substring difficulty 0 1))))
    (make-process :name "leetcode list"
                  :buffer "leetcode_list_value"
                  :command (list "leetcode" "list" "-q" option)
                  :filter #'leetcode--ansi-color-insertion-filter
                  :sentinel 'leetcode--list-all-interactive)))

(defun leetcode-filter-by-tag()
  "Async Create a new buffer to query leetcode programs list by tag."
  (interactive)
  (let* ((tag
          (completing-read "Select Leetcode tag: "
                           leetcode-tags))
         (option (downcase (string-replace " " "-" tag))))
    (make-process :name "leetcode list"
                  :buffer "leetcode_list_value"
                  :command (list "leetcode" "list" "-t" option)
                  :filter #'leetcode--ansi-color-insertion-filter
                  :sentinel 'leetcode--list-all-sync)))

(defun leetcode-random-by-tag()
  "Select a tag and show a question randomly."
  (interactive)
  (let* ((tag
          (completing-read "Select Leetcode tag: "
                           leetcode-tags))
         (option (downcase (string-replace " " "-" tag))))
    (make-process :name "leetcode list"
                  :buffer "leetcode_list_value"
                  :command (list "leetcode" "list" "-t" option)
                  :filter #'leetcode--ansi-color-insertion-filter
                  :sentinel 'leetcode--random)))

(defun leetcode-filter-by-tag-interactive()
  "Async interactively select leetcode programs by tag and show it."
  (interactive)
  (let* ((tag
          (completing-read "Select Leetcode tag: "
                           leetcode-tags))
         (option (downcase (string-replace " " "-" tag))))
    (make-process :name "leetcode list"
                  :buffer "leetcode_list_value"
                  :command (list "leetcode" "list" "-t" option)
                  :filter #'leetcode--ansi-color-insertion-filter
                  :sentinel 'leetcode--list-all-interactive)))

(defun leetcode-filter-by-keyword(keyword)
  "Async Create a new buffer to query leetcode programs list by KEYWORD."
  (interactive "sProblem keyword: ")
  (make-process :name "leetcode list"
                :buffer "leetcode_list_value"
                :command (list "leetcode" "list" keyword)
                :filter #'leetcode--ansi-color-insertion-filter
                :sentinel 'leetcode--list-all-sync))

(defun leetcode-show (n)
  "Show leetcode programs message and download file.
  N is a leetcode program number."
  (interactive "nProgram Number: ")
  (delete-other-windows)
  (let ((mode-buffer (get-buffer-create "*leetcode-description*")))
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (split-window-right)
    (leetcode--pick n)
    (leetcode--edit n)))

(defun leetcode-show-next ()
  "Show the next leetcode programs after the current buffer."
  (interactive)
  (leetcode-show
   (+ 1 (string-to-number (leetcode--get-current-buff-num)))))

(defun leetcode-test ()
  "Submit the current buffer solution."
  (interactive)
  (leetcode--exec "test" (leetcode--get-current-buff-num)))

(defun leetcode-submit ()
  "Submit the current buffer solution."
  (interactive)
  (let ((the-buffer-name (buffer-name)))
    (leetcode--exec "exec" (leetcode--get-current-buff-num))))

(defun show-local-max-problem ()
  "Show max item problem saved in local."
  (interactive)
  (leetcode-show
   (apply #'max
          (mapcar #'get-file-name-num
                  (directory-files leetcode-path nil
                                   (format "\\.%s$" leetcode-language))))))

(defun leetcode-interactive ()
  "Interactively select leetcode problem and show it."
  (interactive)
  (let ((selected-item
         (completing-read "Choose a way to find the problem :"
                          '("query all"
                            "query by difficulty"
                            "query by tag"))))
    (cond
     ((string= "query all" selected-item)
      (leetcode-query-all-interactive))
     ((string= "query by difficulty" selected-item)
      (leetcode-filter-by-difficulty-interactive))
     ((string= "query by tag" selected-item)
      (leetcode-filter-by-tag-interactive)))))

(defun leetcode-top100 ()
  "Leetcode top 100."
  (interactive)
  (let* ((leetcode-question
          (completing-read "Select a problem: " leetcode-top100))
         (leetcode-num
          (car (string-split leetcode-question "\\." t " "))))
    (print leetcode-question)
    (leetcode-show (string-to-number leetcode-num))))


(defun leetcode-top100-random ()
  "Leetcode top 100 random."
  (interactive)
  (let* ((leetcode-question (nth (random 100) leetcode-top100))
         (leetcode-num
          (car (string-split leetcode-question "\\." t " "))))
    (print leetcode-question)
    (leetcode-show (string-to-number leetcode-num)))
  leetcode-question)

(defun leetcode-get-current-description ()
  "Leetcode get description of current problem."
  (interactive)
  (leetcode-show (string-to-number (leetcode--get-current-buff-num))))

(defun leetcode-open-in-browser ()
  "Open current Leetcode problem in browser."
  (interactive)
  (browse-url (concat leetcode-base-url (leetcode--get-current-buff-problem-name))))

(transient-define-prefix leetcode-commands ()
  "LeetCode commands menu."
  [["Problem"
    ("l" "List problems" leetcode-list-all)
    ("d" "Show description" leetcode-get-current-description)
    ("b" "Show problem in browser" leetcode-open-in-browser)]
   ["Code"
    ("s" "Submit solution" leetcode-submit)
    ("t" "Test solution" leetcode-test)]
   ["Navigation"
    ("n" "Next problem" leetcode-show-next)
    ("S" "Show problem" leetcode-show)]])

(define-minor-mode leetcode-edit-mode
  "A minor mode for LeetCode."
  :lighter "LC"
  :global nil
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") 'leetcode-commands)
    map))

(provide 'leetcode)
;;; leetcode.el ends here
