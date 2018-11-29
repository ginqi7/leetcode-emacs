;;; leetcode.el --- a plugin for leetcode
;;; Commentary:
;;; code:
(require 'ctable)

(define-namespace leetcode-

(defun -split-string (str)
  (setq n1 (string-match "\\[" str)
	n2 (string-match "\\]" str n1)
	n3 (string-match "Easy\\|Medium\\|Hard" str n1)
	n4 (string-match "\(" str n3))
  (setq substr1 (substring str 0 n1)
	substr2 (substring str n1 (+ 1 n2))
	substr3 (substring str (+ 1 n2) n3)
	substr4 (substring str n3 n4)
	substr5 (substring str n4))
  (list substr1 substr2  substr3 substr4 substr5)
  )

(defun list-all ()
  (interactive)
  (setq mode-buffer (get-buffer-create "*leetcode*"))
  (switch-to-buffer mode-buffer)
  (erase-buffer)
  (setq show-list (reverse (split-string (shell-command-to-string "leetcode list") "\n")))
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
    :data (mapcar 'leetcode--split-string show-list)
    )
   )
  (setq buffer-read-only nil)
  ))

(provide 'leetcode)
;;; leetcode.el ends here
