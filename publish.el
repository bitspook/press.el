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
(defvar spub--index-template (expand-file-name "index.org"))

(defun spub--make-index-file ()
  "Create the index.html ."
  (let* ((index (sort spub--index
                      (lambda (a b)
                        (time-less-p
                         (encode-time (cdr (assoc "date" b)))
                         (encode-time (cdr (assoc "date" a)))))))
         (latest-5 (seq-take index 5))
         (org-export-global-macros
          `((latest-5-posts . ,(seq-reduce
                                (lambda (accum post)
                                  (let ((url (string-replace
                                              "index.html" ""
                                              (string-replace spub--publish-dir "/" (car post))))
                                        (title (cdr (assoc "title" (cdr post)))))
                                    (string-join (list accum (format "- [[%s%s][%s]]" spub--base-url url title)) "\n")))
                                latest-5 "")))))
    (with-current-buffer (find-file-noselect (expand-file-name "index.org" spub--staging-dir))
      (insert-file spub--index-template)
      (write-file (expand-file-name "index.org" spub--staging-dir) nil)
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
                       ("date" (cons key (org-parse-time-string val)))
                       ("filetags" (cons "tags" (split-string val " " t "[ \t]")))
                       (t (cons key val))))) props)))
    (when (string= "index.org" (f-filename filename))
      (push (cons "date" (parse-time-string (current-time-string))) props))

    (when (not (cdr (assoc "date" props)))
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
  (let* ((user-full-name "Charanjit Singh")
         (org-html-preamble t)
         (org-html-preamble-format `(("en" ,(with-temp-buffer
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
