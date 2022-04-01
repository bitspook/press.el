;; -*- lexical-binding: t -*-
;;; Code:
(require 'seq)
(require 'org-roam)
(require 'cl-lib)

(defvar spub--staging-dir (expand-file-name "./_staged"))
(defvar spub--publish-dir (expand-file-name "./_published"))
(defvar spub--legacy-content-dir (expand-file-name "./content"))
(defvar spub--index-file-name "index.org")

(defun spub--roam-nodes-with-tags (tags)
  "Find all org-roam nodes which have all TAGS."
  (let ((nodes (seq-filter
                (lambda (node) (= (seq-length tags) (seq-length (seq-intersection (org-roam-node-tags node) tags))))
                (org-roam-node-list))))
    nodes))

(defun spub--stage ()
  "Prepare the staging area for publishing.
Since we borrow content from multiple sources (e.g already
published blog posts and org-roam notes), we collect the content
in the staging area before handing it over to `org-mode' for
publishing as a single project."
  (let* ((staging-dir spub--staging-dir)
         (legacy-content-dir spub--legacy-content-dir)
         (index-file spub--index-file-name)
         (nodes-to-publish (spub--roam-nodes-with-tags '("blog-post" "published"))))
    (spub--clean-stage staging-dir)

    (copy-directory legacy-content-dir staging-dir nil t t)
    (copy-file index-file (expand-file-name index-file staging-dir) t)

    (cl-dolist (node nodes-to-publish)
      (copy-file (org-roam-node-file node)
                 (expand-file-name "./blog/" staging-dir)))))

(defun spub--publish ()
  "Publish the project."
  (defvar org-publish-project-alist)
  (let* ((project `("project"
                    :base-directory ,spub--staging-dir
                    :recursive t
                    :base-exteinsion "org"
                    :publishing-directory ,spub--publish-dir
                    :publishing-function org-html-publish-to-html
                    :auto-preamble nil
                    :with-toc nil
                    :with-drawers nil
                    :auto-sitemap t
                    :html-style nil))
         (org-publish-project-alist (list project))
         (org-html-head-include-default-style nil)
         (js-mode-hook nil))
    (spub--clean)
    (spub--stage)
    (org-publish-project (car project) t)))

(defun fix-imported-file (org-file)
  (let* ((md-file (concat (file-name-sans-extension org-file) ".md"))
         (metadata-str
          (with-temp-buffer
            (insert-file md-file)
            (goto-char (point-min))
            (delete-line)
            (search-forward "---")
            (delete-line)
            (set-mark (point))
            (set-mark (point-max))
            (delete-region (region-beginning) (region-end))
            (buffer-substring (point-min) (point-max))))
         (metadata nil)
         (metadata (cl-dolist (line (s-lines metadata-str) metadata)
                     (let ((pair (split-string line ":" t "[ \t]+")))
                       (when pair (push pair metadata))))))
    (with-current-buffer (create-file-buffer org-file)
      (goto-char (point-min))
      (insert
       (s-join "\n"
               (seq-filter
                #'org-not-nil
                (mapcar
                 (lambda (pair)
                   (pcase (car pair)
                     ("draft" nil)
                     ("date" (format "#+DATE: <%s>" (car (split-string (car (cdr pair)) "T"))))
                     ("title" (format "#+TITLE: %s" (s-replace "\"" "" (cadr pair))))
                     ("author" (format "#+AUTHOR: %s" (s-replace-regexp "[\[\"\]]*" "" (cadr pair))))
                     ("tags" (format "#+FILETAGS: %s"
                                     (s-join
                                      " "
                                      (split-string
                                       (s-replace-regexp "[\[\"\]]*" "" (cadr pair)) "," t "[ \t]"))))))
                 metadata))))
      (insert "\n\n\n")
      (insert-file org-file)
      (write-file org-file nil)
      (kill-buffer))))

(seq-map #'fix-imported-file (f-entries (expand-file-name "./content") (lambda (f) (s-ends-with? ".org" f)) t))

(spub--publish)
