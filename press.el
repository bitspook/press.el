;; -*- lexical-binding: t -*-
;;; Code:
(require 'seq)
(require 'org-roam)
(require 'cl-lib)
(require 'ox-publish)
(require 'ox-html)

(defvar press--staging-dir (expand-file-name "./_staged/"))
(defvar press--publish-dir (expand-file-name "./_published/"))
(defvar press--legacy-content-dir (expand-file-name "./content/"))
(defvar press--static-dir (expand-file-name "./static/"))
(defvar press--author "Charanjit Singh")
(defvar press--templates-dir (expand-file-name "./templates"))
(defvar press--preamble-tmpl (expand-file-name "./preamble.el" press--templates-dir))
(defvar press--postamble-tmpl (expand-file-name "./postamble.el" press--templates-dir))
(defvar press--newsletter-tmpl (expand-file-name "./email-newsletter-form.el" press--templates-dir))

(defun press--roam-nodes-with-tags (tags)
  "Find all org-roam nodes which have all TAGS."
  (let ((nodes (seq-filter
                (lambda (node) (= (seq-length tags) (seq-length (seq-intersection (org-roam-node-tags node) tags))))
                (org-roam-node-list))))
    nodes))

(defvar press--hidden-tags '("blog-post" "draft" "published")
  "Tags which should be hidden from press.el.
So they don't end up being published on the blog.")

(defun press--clean ()
  "Cleanup intermediate and published content."
  (delete-directory press--staging-dir t)
  (delete-directory press--publish-dir t))

(defun press--stage ()
  "Prepare the staging area for publishing.
Since we borrow content from multiple sources (e.g already
published blog posts and org-roam notes), we collect the content
in the staging area before handing it over to `org-mode' for
publishing as a single project."
  (let* ((staging-dir press--staging-dir)
         (legacy-content-dir press--legacy-content-dir)
         (blog-posts-to-publish (press--roam-nodes-with-tags '("blog-post" "published"))))
    (press--clean)

    (copy-directory legacy-content-dir staging-dir nil t t)

    (cl-dolist (node blog-posts-to-publish)
      (copy-file (org-roam-node-file node)
                 (expand-file-name
                  (concat "./blog/"
                          (string-replace "_" "-" (org-roam-node-slug node))
                          ".org")
                  staging-dir)))))

(defvar press--index nil)

(defun press--render (filename scope)
  "Render FILENAME as html with SCOPE.
FILENAME should contain DOM as Lisp forms, as provided by
`libxml-parse-html-region' (or accepted by `shr-dom-to-xml').
SCOPE is an alist of (var . val) which the form in FILENAME will
have access to."
  (shr-dom-to-xml
   (with-temp-buffer
     (insert-file-contents filename)
     (eval (read (current-buffer)) scope))))

(defun press--publish-page (dest template scope)
  "Publish page to DEST with TEMPLATE and SCOPE.
TEMPLATE is relative to `press--templates-dir'."
  (let* ((index press--index)
         (dest (if (file-name-extension dest) dest (concat dest ".org")))
         (staged-file (expand-file-name dest press--staging-dir))
         (staged-file-dir (file-name-directory staged-file))
         (dest-dir (string-replace press--staging-dir press--publish-dir staged-file-dir))
         (template (expand-file-name template press--templates-dir))
         (html (press--render template scope)))
    (with-current-buffer (find-file-noselect staged-file)
      (erase-buffer)
      (insert (concat "#+title: " (alist-get 'title scope) "\n"))
      (insert (concat "#+begin_export html\n" html "\n#+end_export"))
      (mkdir staged-file-dir t)
      (write-file staged-file nil)
      (press--org-publish-to-clean-html nil staged-file dest-dir)
      (kill-buffer))))

(defun press--publish-rss-feed (dest posts)
  "Publish RSS feed to DEST for POSTS.
DEST is relative to `press--publish-dir'."
  (let ((feed (shr-dom-to-xml `(feed ((xmlns . "http://www.w3.org/2005/Atom"))
                                     (link ((href . "https://bitspook.in/")))
                                     (link ((href . "https://bitspook.in/feed.xml")
                                            (rel . "self")))
                                     (updated nil ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"))
                                     (author nil (name nil ,press--author))
                                     (id nil "https://bitspook.in/")
                                     (title nil "bitspook.in")

                                     ,@(mapcar
                                        (lambda (post)
                                          `(entry nil
                                                  (title nil ,(alist-get 'title post))
                                                  (link ((href . ,(concat "https://bitspook.in" (alist-get 'url post)))))
                                                  (id nil ,(concat "https://bitspook.in" (alist-get 'url post)))
                                                  (updated nil ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (alist-get 'date post)))
                                                  (summary nil)))
                                        posts)))))
    (with-temp-buffer
      (insert feed)
      (write-file (expand-file-name dest press--publish-dir) nil))))

(defun press--publish-home-page ()
  "Create the home page ."
  (let* ((org-html-preamble nil)
         (org-html-postamble nil)
         (org-html-content-class "")
         (org-export-with-title nil)
         (scope `((title . "Online home of Charanjit Singh")
                  (author . ,press--author)
                  (handle . "bitspook")
                  (latest-posts . ,(seq-take press--index 5))
                  (github . "https://github.com/bitspook")
                  (twitter . "https://twitter.com/bitspook")
                  (linkedin . "https://www.linkedin.com/in/bitspook/")
                  (resume . "https://docs.google.com/document/d/1HFOxl97RGtuhAX95AhGWwa808SO9qSCYLjP1Pm39la0")
                  (gpg-qr-url . "/assets/images/public-key-qr.svg")
                  (avatar . "/assets/images/avatar.png"))))
    (press--publish-page "index" "index.el" scope)
    (press--publish-rss-feed "feed.xml" (seq-take press--index 10))))

(defun press--publish-archive-page ()
  "Publish archive.html."
  (press--publish-page
   "archive" "listing.el"
   `((title . "Archive") (posts . ,press--index))))

(defun press--publish-tags-pages ()
  "Publish listing pages for tags."
  (let ((tags (seq-reduce
               (lambda (accum post)
                 (seq-map
                  (lambda (tag)
                    (push post (alist-get tag accum nil nil #'string=)))
                  (alist-get 'tags post))
                 accum)
               press--index nil)))
    (seq-map
     (lambda (tp)
       (let ((tag (car tp))
             (posts (reverse (cdr tp))))
         (press--publish-page
          (format "tags/%s" tag) "listing.el"
          `((title . ,(capitalize tag)) (posts . ,posts)))
         (press--publish-rss-feed (format "tags/%s.xml" tag) (seq-take posts 10))))
     tags)))

(defun press--publish-category-pages ()
  "Publish listing pages for categories."
  (let ((categories
         (seq-reduce
          (lambda (accum post)
            (let ((category (alist-get 'category post)))
              (when category
                (push post
                      (alist-get category accum nil nil #'string=))))
            accum)
          press--index nil)))
    (seq-map
     (lambda (cp)
       (let* ((category (car cp))
              (posts (reverse (cdr cp))))
         (press--publish-page
          category "listing.el"
          `((title . ,(capitalize category)) (posts . ,posts)))
         (press--publish-rss-feed (format "%s.xml" category) (seq-take posts 10))))
     categories)))

(defun press--get-org-file-props (filename)
  "Get file-level org props for FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (org-element-map (org-element-parse-buffer 'greater-element)
        '(keyword)
      (lambda (kwd)
        (let ((data (cadr kwd)))
          (list (plist-get data :key)
                (plist-get data :value)))))))

(defun press--get-post-meta (org-file published-file)
  "Get post metadata for org file with ORG-FILE published to PUBLISHED-FILE."
  (let* ((props (press--get-org-file-props org-file))
         (props (seq-map
                 (lambda (pcell)
                   (let ((key (downcase (car pcell)))
                         (val (cadr pcell)))
                     (pcase key
                       ("date" (cons 'date (encode-time (org-parse-time-string val))))
                       ("filetags" (seq-filter
                                    (lambda (tag)
                                      (not (seq-contains-p press--hidden-tags tag)))
                                    (cons 'tags (split-string val " " t "[ \t]"))))
                       (_ (cons (intern key) val)))))
                 props)))

    (when (not (assq 'date props))
      (push (cons 'date (parse-time-string (current-time-string))) props))

    (when (not (assq 'category props))
      (let* ((path-frags (split-string (string-replace press--staging-dir "" org-file) "/"))
             (category (when (> (length path-frags) 1) (car path-frags))))
        (when category (push `(category . ,category) props))))

    (push
     `(url . ,(string-replace
               press--publish-dir "/"
               (replace-regexp-in-string "\\(index\\)?.html$" "" published-file)))
     props)

    props))

(defun press--org-publish-to-clean-html (plist filename pub-dir)
  "Publish an org-file to a clean URL.
PLIST FILENAME PUB-DIR are same as `org-html-publish-to-html'"
  (let* ((published-file (org-html-publish-to-html plist filename pub-dir))
         (basename (car (split-string published-file ".html" t)))
         (clean-published-file (expand-file-name "index.html" basename))
         (clean-published-file (cond
                                ((string= (file-name-base published-file) "index") published-file)
                                (t (mkdir basename t)
                                   (rename-file published-file clean-published-file)
                                   clean-published-file))))
    (when plist
      (push (press--get-post-meta filename clean-published-file) press--index))
    clean-published-file))

(defun press--publish ()
  "Publish the project."
  (interactive)
  (defvar org-publish-project-alist)
  (defvar js-mode-hook)
  (let* ((user-full-name press--author)
         (inhibit-message nil)
         (org-html-preamble t)
         (org-export-with-section-numbers nil)
         (org-html-preamble-format
          `(("en" ,(press--render
                    press--preamble-tmpl
                    `((github . "https://github.com/bitspook")
                      (author . "Charanjit Singh")
                      (avatar . "/assets/images/avatar.png"))))))
         (org-html-postamble t)
         (org-html-postamble-format
          `(("en" ,(press--render
                    press--postamble-tmpl
                    `((author . "Charanjit Singh")
                      (handle . "bitspook")
                      (newsletter-form . ,(press--render press--newsletter-tmpl nil))
                      )))))
         (posts `("posts"
                  :base-directory ,press--staging-dir
                  :recursive t
                  :base-exteinsion "org"
                  :publishing-directory ,press--publish-dir
                  :publishing-function press--org-publish-to-clean-html
                  :auto-preamble nil
                  :with-toc nil
                  :with-creator nil
                  :with-drawers nil
                  :html-extension nil
                  :html-style nil))
         (static `("static"
                   :base-directory ,press--static-dir
                   :base-extension "[a-zA-Z0-9]*"
                   :publishing-directory ,press--publish-dir
                   :recursive t
                   :publishing-function org-publish-attachment))
         (project `("project" :components ("static" "posts")))
         (org-publish-project-alist (list posts project static))
         (org-html-head-include-default-style nil)
         (org-html-head-extra
          (string-join
           '("<link rel=\"stylesheet\" href=\"/dist/main.css\"></link>"
             "<script type=\"text/javascript\" src=\"/dist/main.js\"></script>")
           "\n"))
         (js-mode-hook nil))
    (press--clean)
    (press--stage)
    (setq press--index nil)
    (org-publish-project project t)
    ;; Sort press--index anti-chronologically.
    (setq press--index (sort
                        press--index
                        (lambda (a b)
                          (time-less-p
                           (alist-get 'date b)
                           (alist-get 'date a)))))
    (press--publish-home-page)
    (press--publish-archive-page)
    (press--publish-tags-pages)
    (press--publish-category-pages)))

;;; press.el ends here
