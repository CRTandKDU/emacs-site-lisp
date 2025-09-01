;;; johnny-decimal.el --- Johnny.Decimal helpers for org-mode

;; Friday, October 27, 2023

;; Author: jmc

;;; Commentary:

;; This library provides dynamic numbering for Org headlines.  Use
;;
;;     <M-x org-num-mode>
;;
;; to toggle it.
(require 'org-num)

(defun JD-org-num-default-format (numbering)
  "Johnny.Decimal numbering display function.
NUMBERING is a list of numbers."
  (concat (mapconcat '(lambda (n) (format "%02d" n)) numbering ".") " "))

;; Customize org-num
(setq org-num-format-function #'JD-org-num-default-format
      org-num-max-level 2)

(push "noexport" org-num-skip-tags)

(provide 'johnny-decimal)
;;; johnny-decimal.el ends here
