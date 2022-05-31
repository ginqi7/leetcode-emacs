;;; leetcode.el --- a plugin for leetcodeme
;;; Commentary:
;;; code:
(require 'ctable)
(require 'names)

(define-namespace leetcode-

(defvar path "~/.leetcode/code")

(defvar language "go")

(defun buffer-whole-string (buffer)
  "Get String without properties from other buffer"
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun get-current-buff-num ()
  "Convert current buffer name to number"
  (first (split-string (buffer-name) "\\."))
  )

(defun get-file-name-num (name)
  "Convert file name to get first number"
  (string-to-number (first (split-string name "\\.")))
  )

(defun -split-string (str)
  "Divide a leetcode entry title into 5 columns.
STR is a leetcode entry title."
  
  (let* ((n1 (string-match "\\[" str))
         (n2 (string-match "\\]" str n1))
         (n3 (string-match "Easy\\|Medium\\|Hard" str n1))
         (n4 (string-match "\(" str n3))
         (substr1 (substring str 0 n1))
         (substr2 (substring str n1 (+ 1 n2)))
         (substr3 (substring str (+ 1 n2) n3))
         (substr4 (substring str n3 n4))
         (substr5 (substring str n4)))
    (list substr1 substr2  substr3 substr4 substr5)))


(defun list-all-sync (process signal)
  "Create a new buffer to show all leetcode programs list."
  (when (memq (process-status process) '(exit signal))
    (let* (
           (mode-buffer (get-buffer-create "*leetcode-list*"))
           (show-list (reverse (split-string (substring-no-properties (ansi-color-apply (buffer-whole-string "leetcode_list_value")))   "\n")))
           (rows  (mapcar 'leetcode--split-string (remove-if (lambda (arg) (string= "" arg)) show-list)))
           (async-model ; wrapping a large data in async-data-model
            (ctbl:async-model-wrapper rows))
           )
      (kill-buffer "leetcode_list_value")
      (switch-to-buffer mode-buffer)
      (erase-buffer)
      (pop show-list)
      (ctbl:create-table-component-region
       :model
       (make-ctbl:model
        :column-model
        (list (make-ctbl:cmodel :title "Accepted")
              (make-ctbl:cmodel :title "Number")
              (make-ctbl:cmodel :title "Title")
              (make-ctbl:cmodel :title "Difficulty")
              (make-ctbl:cmodel :title "Frequency"))
        :data async-model)))
    (setq buffer-read-only nil)
    (goto-char (point-min)))
  )

(defun list-all ()
  "Async Create a new buffer to show all leetcode programs list."
  (interactive)
  (make-process :name "leetcode list"
                :buffer "leetcode_list_value" 
                :command '("leetcode" "list")
                :sentinel 'leetcode-list-all-sync
                )
  )


(defun pick (n)
  (let ((raw-message (shell-command-to-string (format "leetcode pick %s" n))))
    (insert raw-message))
  )

(defun edit (n)
  (shell-command-to-string (format "leetcode edit %s" n))
  (let (
        (match (concat "^" (number-to-string n)  "\." ".*" "\." leetcode-language "$"))
        )
    (find-file (car (directory-files path 'full match)))
    )
  )

(defun show (n)
  "Show leetcode programs message and download file.
N is a leetcode program number."
  (interactive "nProgram Number: ")
  (delete-other-windows)
  (let (
        (mode-buffer (get-buffer-create "*leetcode-description*"))
        )
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (split-window-right)
    (pick n)
    (edit n)
    ))


(defun show-next ()
  "Show the next leetcode programs after the current buffer."
  (interactive)
  (leetcode-show (+ 1 (string-to-number (get-current-buff-num)))))

(defun submit ()
  "Submit the current buffer solution."
  (interactive)
  (let (
        (the-buffer-name (buffer-name))
        )
    (exec "exec" (get-current-buff-num))
    ))


(defun ansi-color-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert (ansi-color-apply string)) 
      )
    )
  )


(defun exec (action num)
  "Execute leetcode action"
  (let (
        (mode-buffer (get-buffer-create "*leetcode-result*"))
        )
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (other-window 1)
    ) 
  
  (make-process :name "leetcode list"
                :buffer "*leetcode-result*"
                :command `("leetcode" ,action ,num)
                :filter #'ansi-color-insertion-filter
                )
  )


(defun test ()
  "Submit the current buffer solution."
  (interactive)
  (exec "test" (get-current-buff-num))
  )

(defun show-local-max-problem ()
  "Show max item problem saved in local"
  (interactive)
  (show (apply #'max (mapcar #'get-file-name-num  (directory-files leetcode-path nil (format "\\.%s$" leetcode-language)))))
  )

)

(provide 'leetcode)
;;; leetcode.el ends here
