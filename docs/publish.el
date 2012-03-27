(defun handlebar-publish ()


  (require 'org-install)
  (require 'org-publish)



  (setq org-publish-project-alist
        '(
          ("handlebar"
           :publishing-directory "."
           :base-directory "."
           :recursive t
           :publishing-function org-publish-org-to-html
           :auto-preamble t
           :sub-superscript nil
           )
          )
        )

  (org-publish (assoc "handlebar" org-publish-project-alist) t)
  )
