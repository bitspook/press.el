`(footer ((class . "site-footer postamble"))
         ,newsletter-form

         (p nil "Made with ❤️ and Emacs.")
         (p nil
            ,(concat "Author: " author)
            (a ((href . ,(concat "https://twitter.com/" handle)))
               ,(concat "@" handle)))
         (p nil "Published on %d"))
