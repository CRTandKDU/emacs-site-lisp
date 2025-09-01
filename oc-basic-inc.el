;;; oc-basic-inc.el --- addendum to basic backend for citations

;; Author: J.-M. Chauvet, Friday, September 29, 2023

;;; Comment:
;; This file extends `oc-basic', the basic citation processor, with the style "enhanced".
;;
;; When the source org file uses `#+CITE_EXPORT: basic enhanced', citations are inlined
;; with the `author-year' format and enhanced with a tooltip containing the bibliographic
;; entry. The tooltip is similar to the `:activate' feature and requires the
;; Tippy.js library [[https://atomiks.github.io/tippyjs/]].

;; It is installed in two places in the source org file:
;; - In headers:
;; #+HTML_HEAD: <script src="https://unpkg.com/@popperjs/core@2"></script>
;; #+HTML_HEAD: <script src="https://unpkg.com/tippy.js@6"></script>
;; - At the end of the file for activation, after export:
;; 
;; #+BEGIN_EXPORT html
;; <script>
;; tippy('[data-tippy-content]', {
;;   allowHTML: true,
;; });
;; </script>
;; #+END_EXPORT
;; 

;; Bibliographic entries in the `References' section are exported with the `plain' format with
;; added URL if one was found either in the source or a specific `url' fields in the bibtex
;; entry.


;;; Code:
(require 'oc-basic)

;;; Ancillaries
(defun org-cite-basic--latex-string-urls (str style)
  (let ((update str)
	)
    (setq update
	  (replace-regexp-in-string
	   "\\\\url" ""
	   (replace-regexp-in-string
	    "{\\([^ \t\n\r]+\\)}"
	    ;; "{\\([a-Z0-9\\.\\:/_=]+\\)}"
	    (if (string= style "enhanced") "" "\\1") update))
	  )
    update)
  )

(defun org-cite-basic--latex-string-get-url (str)
  (let ((start (string-match-p "\\\\url" str)))
    (and start (string-match
		"{\\([^ \t\n\r]+\\)}"
		;; "{\\([a-Z0-9\\.\\:/_=]+\\)}"
		str)
	 (let ((url (match-string 1 str)))
	   url))
    )
  )

(defun org-cite-basic--latex-string-chars (str)
  (let ((substs '(("\\\\\"a" . "ä" )
		  ("\\\\\"u" . "ü" )
		  ("\\\\\"O" . "Ö" )
		  ("\\\\\"o" . "ö" )
		  ("\\\\\^u" . "û" )
		  ("\\\\textquotesingle" . "'")
		  ("\\\\ldots" . "...")
		  ("{" . "")
		  ("}" . "")
		  ("\\\\ss" . "ss")
		  ))
	(update str)
	)
    (dolist (sub substs update)
      (setq update (replace-regexp-in-string (car sub) (cdr sub) update)))
    )
  )  

(defun org-cite-basic--latex-string-to-html (str &optional author and-to-comma)
  (let ((update str)
	(re (regexp-opt '(" and " " AND ")))
	)
    (setq update (org-cite-basic--latex-string-chars update))
    ;; Shorten text for author field if required
    (if author
	(let ((authors (split-string update re t "[ \f\t\n\r\v]+" ))
	      )
	  ;; (debug (length authors) authors)
	  (cond
	   ((= 1 (length authors))
	    (setq update (car (split-string (car authors) "," t "[ \f\t\n\r\v]+" ))))
	   ((= 2 (length authors))
	    (setq update (format "%s and %s"
				 (car (split-string (car authors) "," t "[ \f\t\n\r\v]+" ))
				 (car (split-string (cadr authors) "," t "[ \f\t\n\r\v]+" )))))
	   (t (setq update
		    (format "%s et al." (car (split-string (car authors) "," t "[ \f\t\n\r\v]+" )))))
	   )
	  )
      )
    (if and-to-comma
	(setq update (replace-regexp-in-string re "; " update))
      )
    update
    )
  )

(defun org-cite-basic--tip (entry-or-key info)
  (let (
	(year (org-cite-basic--get-year entry-or-key info 'no-suffix))
	(author (or (org-cite-basic--get-field 'author entry-or-key info)
		    (org-cite-basic--get-field 'editor entry-or-key info)))
        (title (org-cite-basic--get-field 'title entry-or-key info))
	(url nil)
        (from
         (or (org-cite-basic--get-field 'publisher entry-or-key info)
             (org-cite-basic--get-field 'journal entry-or-key info)
             (org-cite-basic--get-field 'institution entry-or-key info)
             (org-cite-basic--get-field 'school entry-or-key info))))
    ;; (debug (org-cite-basic--latex-string-to-html author))
    (setq author (org-cite-basic--latex-string-to-html author nil t)
	  title  (org-cite-basic--latex-string-to-html title)
	  url    (or
		  (and (stringp from) (org-cite-basic--latex-string-get-url from))
		  (org-cite-basic--get-field 'url entry-or-key info))
	  from   (if (stringp from)
		     (org-cite-basic--latex-string-to-html
		      (org-cite-basic--latex-string-urls from "enhanced"))
		   from)
	  )
    (format "<b>%s</b> (%4s) <i>%s</i>." author year title)
    )
  )

;; Supersede homonymous functions in `oc-basic.el'
(defun org-cite-basic--get-author (entry-or-key &optional info raw)
  "Return author associated to ENTRY-OR-KEY.

ENTRY-OR-KEY, INFO and RAW arguments are the same arguments as
used in `org-cite-basic--get-field', which see.

Author is obtained from the \"author\" field, if available, or
from the \"editor\" field otherwise."
  (let ((update
	 (or (org-cite-basic--get-field 'author entry-or-key info raw)
	     (org-cite-basic--get-field 'editor entry-or-key info raw)))
	)
    (if update (org-cite-basic--latex-string-to-html update t) update)
    )
  )

(defun org-cite-basic--print-entry (entry style &optional info)
  "Format ENTRY according to STYLE string.
ENTRY is an alist, as returned by `org-cite-basic--get-entry'.
Optional argument INFO is the export state, as a property list."
  (let (;; (author (org-cite-basic--get-author entry info))
	(author (or (org-cite-basic--get-field 'author entry info)
		    (org-cite-basic--get-field 'editor entry info)))
        (title (org-cite-basic--get-field 'title entry info))
	(url nil)
        (from
         (or (org-cite-basic--get-field 'publisher entry info)
             (org-cite-basic--get-field 'journal entry info)
             (org-cite-basic--get-field 'institution entry info)
             (org-cite-basic--get-field 'school entry info))))
    ;; (debug (org-cite-basic--latex-string-to-html author))
    (setq author (org-cite-basic--latex-string-to-html author nil t)
	  title  (org-cite-basic--latex-string-to-html title)
	  url    (or
		  (and (stringp from) (org-cite-basic--latex-string-get-url from))
		  (org-cite-basic--get-field 'url entry info))
	  from   (if (stringp from)
		     (org-cite-basic--latex-string-to-html (org-cite-basic--latex-string-urls from style))
		   from)
	  )

    (pcase style
      ("enhanced"
       (let ((year (org-cite-basic--get-year entry info)))
         (org-cite-concat
          author " (" year "). "
          (org-cite-emphasize 'italic title)
          (and from (list ", " from))
	  (and url (list " "
			 (org-element-create
			  'link
			  (list :type "" :path url :contents url)
			  )))
	  ".")
	 )
       )
      ("plain"
       (let ((year (org-cite-basic--get-year entry info 'no-suffix)))
         (org-cite-concat
          (org-cite-basic--shorten-names author) ". "
          title (and from (list ", " from)) ", " year ".")))
      ("numeric"
       (let ((n (org-cite-basic--key-number (cdr (assq 'id entry)) info))
             (year (org-cite-basic--get-year entry info 'no-suffix)))
         (org-cite-concat
          (format "[%d] " n) author ", "
          (org-cite-emphasize 'italic title)
          (and from (list ", " from)) ", "
          year ".")))
      ;; Default to author-year.  Use year disambiguation there.
      (_
       (let ((year (org-cite-basic--get-year entry info)))
         (org-cite-concat
          author " (" year "). "
          (org-cite-emphasize 'italic title)
          (and from (list ", " from)) "."))))))

(defun org-cite-basic--format-author-year (citation format-cite format-ref info)
  "Format CITATION object according to author-year format.

FORMAT-CITE is a function of three arguments: the global prefix, the contents,
and the global suffix.  All arguments can be strings or secondary strings.

FORMAT-REF is a function of four arguments: the reference prefix, as a string or
secondary string, the author, the year, and the reference suffix, as a string or
secondary string.

INFO is the export state, as a property list."

  (let ((export-data
	 (funcall format-cite
            (org-element-property :prefix citation)
            (org-cite-mapconcat
             (lambda (ref)
               (let* ((k (org-element-property :key ref))
                      (prefix
		       ;; (org-element-property :prefix ref)
		       (org-element-create
			'export-snippet
			(list :back-end "html"
			      :value (format "<span data-tippy-content=\"%s\">"
					     (org-cite-basic--tip k info))
			      ))
		      )
                     (suffix
		      ;; (org-element-property :suffix ref))
		      (org-element-create
		       'export-snippet
		       (list :back-end "html" :value "</span>"))
		      )
		     )
                 (funcall format-ref
                          prefix
                          (or (org-cite-basic--get-author k info) "??")
                          (or (org-cite-basic--get-year k info) "????")
                          suffix)))
             (org-cite-get-references citation)
             org-cite-basic-author-year-separator)
            (org-element-property :suffix citation))
	 )
	)
    
    (org-export-data export-data info)
    )
  )

(provide 'oc-basic-inc)
;;; oc-basic-inc.el ends here
