;;; leetcode.el --- a plugin for leetcodeme
;;; Commentary:
;;; code:
(require 'ctable)

(define-namespace leetcode-

(defvar test-case-history nil)

(defvar path nil)

(defvar language nil)

(defun -remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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

(defun list-all ()
  "Create a new buffer to show all leetcode programs list."
  (interactive)
  (let ((mode-buffer (get-buffer-create "*leetcode-list*"))
	(show-list (reverse (split-string (shell-command-to-string "leetcode list") "\n"))))
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
      :data (mapcar 'leetcode--split-string show-list)))
    (setq buffer-read-only nil)
    (goto-char (point-min))))

(defun file-exists-number-p (file number)
  "Determine if the file name contain a number.
FILE is a file name.
NUMBER is a number."
  (equal number (first (split-string file "\\."))))

(defun find-file-number (files number)
  "Determine if there is a file containing the number in the directory.
FILES are some file names.
NUMBER is a number."
  (if (not files)
      nil
    (if (file-exists-number-p (car files) number)
	(car files)
      (find-file-number (cdr files) number))))

(defun show-and-open (n file)
  "Show leetcode programs message and open exists file.
N is a leetcode program number.
FILE is a generated program file name by leetcode."
  (insert (shell-command-to-string (format "leetcode show %s" n)))
  (find-file file))

(defun show-and-create (n)
  "Show leetcode programs message and download file.
N is a leetcode program number."
  (insert (shell-command-to-string (format "leetcode show %s -g -l %s" n leetcode-language)))
  (find-file (find-file-number (directory-files default-directory) (number-to-string n))))

(defun show (n)
  "Show leetcode programs message and download file.
N is a leetcode program number."
  (interactive "nProgram Number: ")
  (setq leetcode-test-case-history nil)
  (delete-other-windows)
  (let ((mode-buffer (get-buffer-create "*leetcode-description*"))
	new-path)
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (split-window-right)
    (setq-local default-directory leetcode-path)
    (setq new-path (expand-file-name leetcode-language))
    (if (file-exists-p new-path)
	"directory exists"
      (make-directory new-path))
    (setq default-directory new-path)
    (let ((file (find-file-number (directory-files default-directory) (number-to-string n))))
      (if file
	  (show-and-open n file)
      (leetcode--remove-dos-eol)
	(show-and-create n)))))

(defun show-next ()
  "Show the next leetcode programs after the current buffer."
  (interactive)
  (leetcode-show (+ 1 (string-to-number (first (split-string (buffer-name) "\\."))))))

(defun submit ()
  "Submit the current buffer solution."
  (interactive)
  (let ((the-buffer-name (buffer-name))
	(mode-buffer (get-buffer-create "*leetcode-result*")))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (insert (shell-command-to-string (format "leetcode submit %s" the-buffer-name)))))

(defun test (s)
  "Submit the current buffer solution.
S is a string that represents the test case."
  (interactive
   (if leetcode-test-case-history
       (list (read-string (format "test-case (%s): " (car leetcode-test-case-history)) nil 'leetcode-test-case-history))
     (list (read-string "test-case: " nil 'leetcode-test-case-history)))
   )

  (let ((the-buffer-name (buffer-name))
	(mode-buffer (get-buffer-create "*leetcode-result*")))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer mode-buffer)
    (erase-buffer)
    (insert (shell-command-to-string (format "leetcode test %s -t %s" the-buffer-name (if leetcode-test-case-history (car leetcode-test-case-history) s))))))

(defun login (username passwd)
  "Login.
USERNAME is leetcode username.
PASSWD is leetcode password."
  (interactive (list
		(read-string "username: ")
		(read-passwd "password: ")))

  (let ((string (eshell-command-result (format "echo '%s\n%s\n' | leetcode user -l"
					       username passwd))))
    (if (string-match-p "Error" string)
	(message "Error: please check your username, password and network")
      (message (format "Successfully login as %s" username)))))

(defun logout ()
  "Logout."
  (interactive)
  (let ((string (shell-command-to-string "leetcode user -L")))
    (if (string-match-p "Error" string)
	(message "Error: please check your network")
      (message "Successfully logout")))))

(provide 'leetcode)
;;; leetcode.el ends here
