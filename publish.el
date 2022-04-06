;; -*- lexical-binding: t -*-
;;; Code:
(require 'seq)
(require 'org-roam)
(require 'cl-lib)
(require 'ox-publish)

(defvar spub--staging-dir (expand-file-name "./_staged/"))
(defvar spub--publish-dir (expand-file-name "./_published/"))
(defvar spub--legacy-content-dir (expand-file-name "./content/"))
(defvar spub--static-dir (expand-file-name "./static/"))
(defvar spub--preamble-file (expand-file-name "./preamble.html"))
(defvar spub--base-url "http://localhost:8000"
  "Need an HTTP(s) URL to convince org-mode that absolute links in
index.html aren't local file URLs.")
(defvar spub--author "Charanjit Singh")

(defun spub--roam-nodes-with-tags (tags)
  "Find all org-roam nodes which have all TAGS."
  (let ((nodes (seq-filter
                (lambda (node) (= (seq-length tags) (seq-length (seq-intersection (org-roam-node-tags node) tags))))
                (org-roam-node-list))))
    nodes))

(defun spub--clean ()
  "Cleanup intermediate and published content."
  (delete-directory spub--staging-dir t)
  (delete-directory spub--publish-dir t))

(defun spub--stage ()
  "Prepare the staging area for publishing.
Since we borrow content from multiple sources (e.g already
published blog posts and org-roam notes), we collect the content
in the staging area before handing it over to `org-mode' for
publishing as a single project."
  (let* ((staging-dir spub--staging-dir)
         (legacy-content-dir spub--legacy-content-dir)
         (blog-posts-to-publish (spub--roam-nodes-with-tags '("blog-post" "published"))))
    (spub--clean)

    (copy-directory legacy-content-dir staging-dir nil t t)

    (cl-dolist (node blog-posts-to-publish)
      (copy-file (org-roam-node-file node)
                 (expand-file-name "./blog/" staging-dir)))))

(defvar spub--index nil)

(defun spub--render (filename scope)
  "Render FILENAME as html with SCOPE.
FILENAME should contain DOM as Lisp forms, as provided by
`libxml-parse-html-region' (or accepted by `shr-dom-to-xml').
SCOPE is an alist of (var . val) which the form in FILENAME will
have access to."
  (shr-dom-to-xml
   (with-temp-buffer
     (insert-file-contents filename)
     (eval (read (current-buffer)) scope))))

(defun spub--make-index-file ()
  "Create the index.html ."
  (let* ((index (sort spub--index
                      (lambda (a b)
                        (time-less-p
                         (encode-time (cdr (assq 'date b)))
                         (encode-time (cdr (assq 'date a)))))))
         (org-html-preamble nil)
         (org-html-postamble nil)
         (org-html-content-class "")
         (org-export-with-title nil)
         (staged-index-file (expand-file-name "index.org" spub--staging-dir))
         (index-body (spub--render
                      "./index.el"
                      `((author . ,spub--author)
                        (handle . "bitspook")
                        (latest-posts . ,(seq-map (lambda (p) (cdr p)) (seq-take index 5)))
                        (github . "https://github.com/bitspook")
                        (stackoverflow . "https://stackoverflow.com/users/3175762")
                        (linkedin . "https://www.linkedin.com/in/bitspook/")
                        (resume . "https://docs.google.com/document/d/1HFOxl97RGtuhAX95AhGWwa808SO9qSCYLjP1Pm39la0")
                        (gpg-qr-url . "/assets/images/public-key-qr.svg")
                        (avatar . "/assets/images/avatar.png")))))
    (with-current-buffer (find-file-noselect staged-index-file)
      (erase-buffer)
      (insert "#+title: Online home of Charanjit Singh \n\n")
      (insert (concat "#+begin_export html\n" index-body "\n#+end_export"))
      (write-file staged-index-file nil)
      (org-export-to-file 'html (expand-file-name "index.html" spub--publish-dir))
      (kill-buffer))))

(defun spub--get-org-file-props (filename)
  "Get file-level org props for FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (org-element-map (org-element-parse-buffer 'greater-element)
        '(keyword)
      (lambda (kwd)
        (let ((data (cadr kwd)))
          (list (plist-get data :key)
                (plist-get data :value)))))))

(defun spub--get-post-meta (filename)
  "Get post metadata for org file with FILENAME."
  (let* ((props (spub--get-org-file-props filename))
         (props (seq-map
                 (lambda (pcell)
                   (let ((key (downcase (car pcell)))
                         (val (cadr pcell)))
                     (pcase key
                       ("date" (cons 'date (org-parse-time-string val)))
                       ("filetags" (cons 'tags (split-string val " " t "[ \t]")))
                       (t (cons (intern key) val))))) props)))
    (when (string= "index.org" (f-filename filename))
      (push (cons 'date (parse-time-string (current-time-string))) props))

    (when (not (assq 'category props))
      (let* ((path-frags (split-string (string-replace spub--staging-dir "" filename) "/"))
             (category (when (> (length path-frags) 1) (car path-frags))))
        (when category (push `(category . ,category) props))))

    (push (cons 'url (string-replace
                      spub--staging-dir "/"
                      (replace-regexp-in-string "\\(index\\)?.org$" "" filename))) props)

    (when (not (cdr (assq 'date props)))
      (error "Date is a required field for a post"))
    props))

(defun spub--org-publish-to-clean-html (plist filename pub-dir)
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
    (push (cons clean-published-file (spub--get-post-meta filename)) spub--index)
    clean-published-file))

(defun spub--publish (&optional force? async?)
  "Publish the project."
  (defvar org-publish-project-alist)
  (let* ((user-full-name spub--author)
         (org-html-preamble t)
         (org-html-preamble-format
          `(("en" ,(with-temp-buffer
                     (insert-file-contents spub--preamble-file)
                     (buffer-string)))))
         (posts `("posts"
                  :base-directory ,spub--staging-dir
                  :recursive t
                  :base-exteinsion "org"
                  :publishing-directory ,spub--publish-dir
                  :publishing-function spub--org-publish-to-clean-html
                  :auto-preamble nil
                  :with-toc nil
                  :with-creator nil
                  :with-drawers nil
                  :template "./templates/test.org"
                  :html-extension nil
                  :html-style nil))
         (static `("static"
                   :base-directory ,spub--static-dir
                   :base-extension "[a-zA-Z0-9]*"
                   :publishing-directory ,spub--publish-dir
                   :recursive t
                   :publishing-function org-publish-attachment))
         (project `("project" :components ("static" "posts")))
         (org-publish-project-alist (list posts project static))
         (org-html-head-include-default-style nil)
         (org-html-head
          (string-join
           '("<link rel=\"stylesheet\" href=\"/dist/main.css\"></link>"
             "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
           "\n"))
         (js-mode-hook nil))
    (when force? (spub--clean))
    (spub--stage)
    (setq spub--index nil)
    (org-publish-project project t)
    (spub--make-index-file)))

;;; publish.el ends here
