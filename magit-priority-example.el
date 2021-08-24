;;; magit-priority-example.el --- Example code for magit/forge

;; Copyright (C) 2021  Emin Martinian

;; Author: Emin Martinian
;; Maintainer: Emin Martinian

;; magit-priority-example is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; magit-priority-example is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Example to illustrate prioritizing github issues

;;; Code:

(defun forge-list-labeled-issues (id label)
  "List issues of the repo with given ID that have LABEL.

Note that LABEL can be a SQL like pattern such as 'priority:%' so that
you can get a sorted list of a set of similar issues.  See examples
at https://github.com/emin63/magit-priority-example for details.

Issues are listed in a separate buffer."
  (interactive  ;; interactively get desired arguments from user
   (list
    (oref (forge-get-repository t) id) ;; this reads in current magit repo
    (completing-read  ;; use completing-read so we can set require-match nil
     "Label: "        ;; will ask user for desired label
     (mapcar
      #'cadr
      (slot-value (forge-get-repository t) 'labels) ;; gets known labels
      )
     nil ;; predicate for completing-read means keep all labels
     nil ;; allow nil so user can use a % pattern
     )
    )
   )
  ;; then pass them to forge-list-labeled-issues-main
  (forge-list-labeled-issues-main id label)
  )

(defun forge-list-labeled-issues-query (id label)
  "FIXME
blah."
  (forge-sql
   [:select [$i1 label:name]
	    :from [issue issue_label label]
	    :where (and (= issue_label:issue issue:id)
			(= issue_label:id    label:id)
			(= issue:repository  $s2)
			(like label:name        $s3)
			(isnull issue:closed)
			)
	    :order-by [(desc label:name updated)]]
   (forge--tablist-columns-vector 'issue)
   id label)
  )

(defun forge-list-labeled-issues-main (id label)
  "List issues of the repo with given ID that have LABEL.

This is intended to be called by forge-list-labeled-issues if
used interactively.

"
  (forge-topic-list-setup
      #'forge-issue-list-mode id nil nil
    (lambda ()
      (let* ((my-items (forge-list-labeled-issues-query id label)))
	(mapc (lambda (thing)
		(setcar (nthcdr 2 thing)
			(format "[%s] %s" (nth 3 thing) (nth 2 thing)))
		)
	      my-items)
	my-items)
      )
    )
  )

;;; _
(provide 'magit-priority-example)
;;; magit-priority-example.el ends here
