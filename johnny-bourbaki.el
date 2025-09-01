
;;; johnny-bourbaki.el --- Johnny.Bourbaki helpers for org-mode

;; Friday, October 27, 2023

;; Author: jmc

;;; Commentary:

;; This library provides dynamic numbering for Org headlines.  Use
;;
;;     <M-x org-num-mode>
;;
;; to toggle it.
(require 'org-num)

(defun JB-org-num-default-format (numbering)
  "Bourbaki numbering display function.
NUMBERING is a list of numbers."
  (let ((depth (length numbering)))
    (cond
     ((= 1 depth) (format "§ %d. " (car numbering)))
     ((= 2 depth) (format "%d.%d. " (car numbering) (cadr numbering)))
     (t (format "(%d.%d.%d.) " (car numbering) (cadr numbering) (caddr numbering))))))

;; Customize org-num
(setq org-num-format-function #'JB-org-num-default-format
      org-num-max-level 3)

(push "noexport" org-num-skip-tags)

(provide 'johnny-bourbaki)
;;; johnny-bourbaki.el ends here
