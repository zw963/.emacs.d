(require 'org)

(defun my-tbl-export (file-name)
  "Search for table named `NAME` and export."
  (interactive "s")
  (show-all)
  (if (search-forward-regexp (concat "#\\+TBLNAME: +" "first-table") nil t)
      (progn
        (next-line)
        (org-table-export (format "%s.csv" file-name) "orgtbl-to-csv"))))
