`(nav ((class . "top-nav"))
      (div ((class . "brand"))
           (a ((href . "/"))
              (img ((class . "brand-avatar")
                    (src . ,avatar)
                    (alt . ,author)))))
      (div nil
           (ul nil
               (li nil (a ((href . "/blog")) "Blog"))
               (li nil (a ((href . "/poems")) "Poems"))
               (li nil (a ((href . ,github)) "Projects")))))
