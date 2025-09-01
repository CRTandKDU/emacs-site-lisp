;; While Emacs Lips keyboard binding M-c capitalizes each next word on the line, it is not sufficient to capitalize the English title. There are different capitalization styles in English such as Chicago Manual of Style, Associated Press Style, Modern Language Association, so here we are using Chigago Manual of Style for capitalization.

(defvar rcd-do-not-capitalize 
  '("a" "an" "and" "as" "at" "but" "by" "for" "he" "if" "in" "it" "nor" "of" "on" "or" "she" "so" "the" "they" "to" "up" "via" "we" "with" "yet")
  "Words which should not be capitalized by Chicago Manual of Style (CMOS).")

(defun rcd-capitalize-english (string)
  "Capitalize correctly the English string."
  (let* ((string (replace-regexp-in-string "[[:blank:]]+" " " string))
     (words (split-string string " "))
     (capitalized nil))
    (setq capitalized (append (list (capitalize (downcase (pop words)))) capitalized))
    (while words
      (let* ((word (pop words))
         (word (cond ((member word rcd-do-not-capitalize)
              (downcase word))
             (t (capitalize (downcase word))))))
    (setq capitalized (append (list word) capitalized))))
    (mapconcat 'identity (reverse capitalized) " ")))

(defun rcd-capitalize-english-line ()
  "Capitalize current line by Chicago Manual of Style (CMOS)."
  (interactive)
  (beginning-of-line)
  (let ((line (thing-at-point 'line)))
    (when (stringp line)
      (kill-whole-line)
      (beginning-of-line)
      (insert (rcd-capitalize-english line)))))

(provide 'cmos)
