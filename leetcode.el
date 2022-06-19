;;; leetcode.el --- a plugin for leetcodeme
;;; Commentary:
;;; code:
(require 'ctable)
(require 'cl-lib)

(defvar leetcode-path "~/.leetcode/code")

(defvar leetcode-language "go")

(defvar leetcode-hide-no-auth-problems t)

(defun leetcode--buffer-whole-string (buffer)
  "Get String without properties from other buffer"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun leetcode--get-current-buff-num ()
  "Convert current buffer name to number"
  (car (split-string (buffer-name) "\\.")))

(defun get-file-name-num (name)
  "Convert file name to get first number"
  (string-to-number (first (split-string name "\\."))))


(defun leetcode--parse-leetcode-entry (str)
  "Divide a leetcode entry title into 5 columns.
STR is a leetcode entry title."
  (let ((the-list (split-string str " +\\|\\[\\|\\]" t))
        (accepted)
        (number)
        (title)
        (difficulty)
        (frequency))
    (if (string-match "[0-9]+" (car the-list))
        (setq number (car the-list)
              title (mapconcat 'identity (cl-subseq the-list 1 (- (length the-list) 3)) " "))
      (setq acceped (car the-list)
          number (nth 1 the-list)
              title (mapconcat 'identity (cl-subseq the-list 2 (- (length the-list) 3)) " ")))
    (setq difficulty (nth (- (length the-list) 3) the-list)
          frequency (concat (nth (- (length the-list) 2) the-list) (nth (- (length the-list) 1) the-list)))
    
    (list acceped number title difficulty frequency)))

(defun leetcode--entry-filter (lst)
  (if leetcode-hide-no-auth-problems
      (not (string= (car lst) "🔒"))
    t))

(defun leetcode--parse-leetcode-list (str)
  "Divide a leetcode entry title into 5 columns.
STR is a leetcode entry title."
  (seq-filter 
   #'leetcode--entry-filter
   (mapcar #'leetcode--parse-leetcode-entry (split-string str "\n+" t))))

(defun leetcode--create-cmodel (title)
  "Create cmodel by title string"
  (make-ctbl:cmodel :title  title
                    :align 'left))

(defun leetcode-add-click-hook ()
  (goto-line 0)
  (lexical-let ((cp (ctbl:cp-get-component)))
    (ctbl:cp-add-click-hook 
     cp 
     (lambda ()
       (leetcode-show (string-to-number (nth 1 (ctbl:cp-get-selected-data-row cp))))))))

(defun leetcode--list-all-sync (process signal)
  "Create a new buffer to show all leetcode programs list."
  (when (memq (process-status process) '(exit signal))
    (let* ((mode-buffer (get-buffer-create "*leetcode-list*"))
           (rows (leetcode--parse-leetcode-list (leetcode--buffer-whole-string "leetcode_list_value")))
           (async-model ; wrapping a large data in async-data-model
            (ctbl:async-model-wrapper rows)))

      (kill-buffer "leetcode_list_value")
      (switch-to-buffer mode-buffer)
      (erase-buffer)
      (ctbl:create-table-component-region
       :model
       (make-ctbl:model
        :column-model
        (mapcar 'leetcode--create-cmodel (list "Accepted"
                                          "Number"
                                          "Title"
                                          "Difficulty"
                                          "Frequency"))
        :data async-model)))
    (leetcode-add-click-hook)
    (setq buffer-read-only nil)
    (goto-char (point-min))))

(defun leetcode--pick (n)
  (let ((raw-message (shell-command-to-string (format "leetcode pick %s" n))))
    (insert (replace-regexp-in-string "\015" "" raw-message))))

(defun leetcode--edit (n)
  (shell-command-to-string (format "leetcode edit %s" n))
  (let ((match (concat "^" (number-to-string n)  "\." ".*" "\." leetcode-language "$")))
    (find-file (car (directory-files leetcode-path 'full match)))))

(defun leetcode--ansi-color-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert (ansi-color-apply string)))))

(defun leetcode--exec (action num)
  "Execute leetcode action"
  (let ((mode-buffer (get-buffer-create "*leetcode-result*")))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer mode-buffer)
    (erase-buffer)
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
  (leetcode-show (+ 1 (string-to-number (leetcode--get-current-buff-num)))))

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
  "Show max item problem saved in local"
  (interactive)
  (show (apply #'max (mapcar #'get-file-name-num  (directory-files leetcode-path nil (format "\\.%s$" leetcode-language))))))


(provide 'leetcode)
;;; leetcode.el ends here
