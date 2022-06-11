`(footer ((class . "site-footer postamble"))
         ,newsletter-form
         (p ((class . "rss-sub"))
            (a
             ((href . "/feed.xml") (title . "Follow via RSS") (target . "blank"))
             (span ((class . "rss")))
             "OR Follow via RSS"))
         (p nil "Made with ❤️ and Emacs.")
         (p nil
            ,(concat "Author: " author)
            (a ((href . ,(concat "https://twitter.com/" handle)))
               ,(concat "@" handle)))
         (p nil "Published on %d"))
