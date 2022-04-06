`(ul ((class . "generic-listing"))
     ,@(seq-map
        (lambda (post)
          `(li ((class . "generic-li"))
               (span ((class . ,(concat "li-icon li-icon--" (cdr (assq 'category post))))))
               (div ((class . "generic-li-conent"))
                    (a ((href . ,(cdr (assq 'url post)))
                        (class . "generic-li-title"))
                       ,(cdr (assq 'title post)))
                    (span ((class . "generic-li-meta"))
                          (span ((class . "meta-item date"))
                                ,(format-time-string "%B %d, %Y" (encode-time (cdr (assq 'date post)))))
                          (span ((class . "meta-item tags"))
                                ,@(seq-map (lambda (t) (concat (capitalize t) ", ")) (cdr (assq 'tags post))))))))
        posts))
