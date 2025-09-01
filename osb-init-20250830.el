;; Source: [[https://github.com/bastibe/org-static-blog]]
;; Date: Tuesday, October 10, 2023

(require 'org-static-blog)

(setq org-static-blog-publish-title "The Knowcessable - Views from the High Street")
(setq org-static-blog-publish-url "")
;; (setq org-static-blog-publish-url "http://jeanmariechauvet.com/papers/")
(setq org-static-blog-publish-directory "~/projects/blog/")
(setq org-static-blog-posts-directory "~/projects/blog/posts/")
(setq org-static-blog-drafts-directory "~/projects/blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-static-blog-use-preview t)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
;; <link href= \"style.css\" rel=\"stylesheet\" type=\"text/css\" />
;; (setq org-static-blog-page-header
;;       "<meta name=\"author\" content=\"Crt_and_KDU\">
;; <meta name=\"referrer\" content=\"no-referrer\">
;; <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
;; <link href=\"https://fonts.cdnfonts.com/css/open-sans\" rel=\"stylesheet\">
;; <link rel=\"icon\" href=\"static/favicon.ico\">")
(setq org-static-blog-page-header
      " <meta charset=\"utf-8\">
        <meta content=\"width=device-width, initial-scale=1.0\" name=\"viewport\">
        <meta name=\"author\" content=\"CRT_and_KDU\">
        <!-- Favicon -->
        <link href=\"img/favicon.ico\" rel=\"icon\">

        <!-- Google Fonts -->
<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Open+Sans:300,400,600,700&amp;lang=en\" />
        <!-- Font Awesome -->
        <link href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.10.0/css/all.min.css\" rel=\"stylesheet\">
        <script src=\"https://unpkg.com/@popperjs/core@2\"></script>
        <script src=\"https://unpkg.com/tippy.js@6\"></script>
        <link href=\"https://fonts.googleapis.com/css?family=EB+Garamond\" rel=\"stylesheet\">
        <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css\">
        <style>table.ref td{ text-align: right; font-size: small; font-family:'Fira Code', monospace; }</style>
        <style>table.center { margin-left:auto; margin-right:auto; }</style>
        <style>img.center { margin-left:auto; margin-right:auto; }</style>
        <style>.source-code { text-align: left; font-size: small; font-family:'Fira Code', monospace; }</style>
        <style>pre { text-align: left; font-size: small; font-family:'Fira Code', monospace; }</style>
        <style>body { font-family:'EB Garamond', serif; font-size: 16px; }</style>
        <style>blockquote {background: #f9f9f9; border-left: 10px solid #ccc; margin: 1.5em 10px; padding: 0.5em 10px; quotes: \"\\201C\"\"\\201D\"\"\\2018\"\"\\2019\";} blockquote:before {color: #ccc; content: open-quote; font-size: 4em; line-height: 0.1em; margin-right: 0.25em; vertical-align: -0.4em;} blockquote p {display: inline;}</style>

        <!-- Customized Bootstrap Stylesheet -->
        <link href=\"css/style.css\" rel=\"stylesheet\">")


;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
;; (setq org-static-blog-page-preamble
;;       "<div class=\"header\">
;;   <a href=\"http://jeanmariechauvet.com/papers/\">Excursions</a>
;; </div>")
(setq org-static-blog-page-preamble
      "<div class=\"wrapper\">
        <div class=\"sidebar\">
            <div class=\"sidebar-text d-flex flex-column h-100 justify-content-center text-center\">
                <img class=\"mx-auto d-block w-75 bg-primary img-fluid rounded-circle mb-4 p-3\" src=\"img/profile.jpg\" alt=\"Image\">
                <h1 class=\"font-weight-bold\">CRT_and_KDU</h1>
                <p class=\"mb-4\">
Durissima est hodie condition scribendi libros Mathematicos, praecipue Astronomicos. Nisi enim servaveris genuinam subtilitatem propositionum, instructionum, demonstrationum, conclusionum, liber non erit Mathematicus sin autem servaveris. Adeoq; hodie perquam pauci sunt lectores idonei.
                </p>
                <div class=\"d-flex justify-content-center mb-5\">
                    <a class=\"btn btn-outline-primary mr-2\" href=\"#\"><i class=\"fab fa-twitter\"></i></a>
                    <a class=\"btn btn-outline-primary mr-2\" href=\"#\"><i class=\"fab fa-facebook-f\"></i></a>
                    <a class=\"btn btn-outline-primary mr-2\" href=\"#\"><i class=\"fab fa-linkedin-in\"></i></a>
                    <a class=\"btn btn-outline-primary mr-2\" href=\"#\"><i class=\"fab fa-instagram\"></i></a>
                </div>
                <!-- <a href=\"\" class=\"btn btn-lg btn-block btn-primary mt-auto\">Hire Me</a> -->
            </div>
            <div class=\"sidebar-icon d-flex flex-column h-100 justify-content-center text-right\">
                <i class=\"fas fa-2x fa-angle-double-right text-primary\"></i>
            </div>
        </div>
        <div class=\"content\">
            <!-- Navbar Start -->
            <div class=\"container p-0\">
                <nav class=\"navbar navbar-expand-lg bg-secondary navbar-dark\">
                    <a href=\"\" class=\"navbar-brand d-block d-lg-none\">Navigation</a>
                    <button type=\"button\" class=\"navbar-toggler\" data-toggle=\"collapse\" data-target=\"#navbarCollapse\">
                        <span class=\"navbar-toggler-icon\"></span>
                    </button>
                    <div class=\"collapse navbar-collapse justify-content-between\" id=\"navbarCollapse\">
                        <div class=\"navbar-nav m-auto\">
                            <a href=\"index.html\" class=\"nav-item nav-link\">Home</a>
                            <a href=\"about.html\" class=\"nav-item nav-link\">About</a>
<!--
                            <div class=\"nav-item dropdown\">
                                <a href=\"#\" class=\"nav-link dropdown-toggle active\" data-toggle=\"dropdown\">Pages</a>
                                <div class=\"dropdown-menu\">
                                    <a href=\"blog.html\" class=\"dropdown-item\">Blog Grid</a>
                                    <a href=\"single.html\" class=\"dropdown-item\">Blog Detail</a>
                                </div>
                            </div>
                            <a href=\"contact.html\" class=\"nav-item nav-link\">Contact</a>
-->    
                    </div>
                    </div>
                </nav>
            </div>
            <!-- Navbar End -->

            <div class=\"container py-5 px-2 bg-white\">
                <div class=\"row px-4\">
                    <div class=\"col-12\">
                        <img class=\"img-fluid mb-4\" src=\"img/444High.webp\" alt=\"Image\">

")

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble
      "<div id=\"archive\">
  <a href=\"archive.html\">Other posts</a>
</div>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/deed.en\"><img alt=\"Creative Commons License\" width=100 style=\"border-width:0\" src=\"https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by-sa.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">jeanmariechauvet.com</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://jeanmariechauvet.com\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">CRT_and_KDU</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/deed.en\">Creative Commons Attribution-Attribution-ShareAlike 4.0 International</a>.</center>
<center>All images were created under <a rel=\"license\" href=\"https://huggingface.co/spaces/CompVis/stable-diffusion-license\">Creative ML Open RAIL-M</a>  unless otherwise indicated.</center>
</div></div></div></div></div>

")

;; This HTML code is inserted into the index page between the preamble and
;;   the blog posts
(setq org-static-blog-index-front-matter
      "<h1>The Knowcessable</h1><h3>Views from the High Street</h3>\n")

;; Redefining some functions holding templates
(defun org-static-blog-assemble-multipost-page (pub-filename post-filenames &optional front-matter)
  "Assemble an index page that contains multiple posts one after another.
Posts are sorted in descending time.
Overvwrite adapted to CSS design."
  (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date y)
                                                                  (org-static-blog-get-date x)))))
  (org-static-blog-with-find-file
   pub-filename
   (org-static-blog-template
    org-static-blog-publish-title
   (concat
    (when front-matter front-matter)
    "<div class=\"container bg-white pt-5\">"
    (apply 'concat (mapcar
                    (if org-static-blog-use-preview
                        'org-static-blog-get-preview
                      'org-static-blog-get-post-content) post-filenames))
    "</div>"
    "<div id=\"archive\">\n"
    "<a href=\"" (org-static-blog-get-absolute-url org-static-blog-archive-file) "\">" (org-static-blog-gettext 'other-posts) "</a>\n"
    "</div>\n"))))

(defun org-static-blog-get-preview (post-filename)
  "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML.
If the HTML body contains multiple paragraphs, include only the first paragraph,
and display an ellipsis.
Preamble and Postamble are excluded, too.
Overwrite adapted to CSS design."
  (with-temp-buffer
    (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
    (let ((post-title (org-static-blog-get-title post-filename))
          (post-date (org-static-blog-get-date post-filename))
          (post-taglist (org-static-blog-post-taglist post-filename))
          (post-ellipsis "")
          (preview-region (org-static-blog--preview-region)))
      (when (and preview-region (search-forward "<p>" nil t))
        (setq post-ellipsis
              (concat (when org-static-blog-preview-link-p
                        (format "<a href=\"%s\">"
                                (org-static-blog-get-post-url post-filename)))
                      org-static-blog-preview-ellipsis
                      (when org-static-blog-preview-link-p "</a>\n"))))
      ;; Put the substrings together.
      (let ((title-link
             (format "<h3 class=\"mt-md-4 px-md-3 mb-2 py-2 bg-white font-weight-bold\"><a href=\"%s\">%s</a></h3>"
                     (org-static-blog-get-post-url post-filename) post-title))
            (date-link
             (format-time-string (concat "<small class=\"mr-2 text-muted\"><i class=\"fa fa-calendar-alt\"></i>&nbsp;"
                                         (org-static-blog-gettext 'date-format)
                                         "</small>")
                                 post-date))
	    (img-link
	     (format "<img class=\"img-fluid mb-4 mb-md-0\" src=\"img/%s.png\" alt=\"Image\">"
		     (file-name-base post-filename)))
	    )

        (concat
	 "<div class=\"row blog-item px-3 pb-5\">"
	 "<div class=\"col-md-5\">"
	 img-link
	 "</div>"
         "<div class=\"col-md-7\">"
	 title-link
         "<div class=\"d-flex flex-column mb-3\">"
         date-link
         "<small class=\"mr-2 text-muted\"><i class=\"fa fa-folder\"></i>&nbsp;"
	 (format "<span class=\"taglist\">%s</span>" post-taglist)
	 "</small>"
         "</div>"
         preview-region
         post-ellipsis
	 "</div>"
         )))))


(defun osb-init-get-content (result)
  "Get content only from the HTML export of an org-mode file.
Any #+HTML_HEADER: is lost however."
  ;; (debug result)
  (with-temp-buffer
    (insert result)
    (buffer-substring-no-properties
     (progn
       (goto-char (point-min))
       (search-forward "<div id=\"content\" class=\"content\">"))
     (progn
       (goto-char (point-max))
       (search-backward "<div id=\"postamble\" class=\"status\">")
       (search-backward "</div>"))
     )
    ) 
  )

(advice-remove 'org-static-blog-render-post-content
	       #'osb-init-get-content)

(advice-add 'org-static-blog-render-post-content
	    :filter-return
	    #'osb-init-get-content)

(provide 'osb-init)
