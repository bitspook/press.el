;; -*- lexical-binding: t -*-
;;; Code:
(require 'seq)
(require 'org-roam)
(require 'cl-lib)

(defvar spub--staging-dir (expand-file-name "./_staged/"))
(defvar spub--publish-dir (expand-file-name "./_published/"))
(defvar spub--legacy-content-dir (expand-file-name "./content/"))
(defvar spub--static-dir (expand-file-name "./static/"))
(defvar spub--index-file (expand-file-name "index.org"))
(defvar spub--preamble-file (expand-file-name "./preamble.html"))

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
         (index-file spub--index-file)
         (blog-posts-to-publish (spub--roam-nodes-with-tags '("blog-post" "published"))))
    (spub--clean)

    (copy-directory legacy-content-dir staging-dir nil t t)
    (copy-file index-file staging-dir t)

    (cl-dolist (node blog-posts-to-publish)
      (copy-file (org-roam-node-file node)
                 (expand-file-name "./blog/" staging-dir)))))

(defun spub--publish (&optional force? async? sitemap?)
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
                  :publishing-function org-html-publish-to-html
                  :auto-preamble nil
                  :with-toc nil
                  :with-creator nil
                  :with-drawers nil
                  :sitemap-filename "index.org"
                  :auto-sitemap ,sitemap?
                  :template "./templates/test.org"
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
    (org-publish-project (car project) force? async?)))

(spub--publish)

;;; publish.el ends here