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

;; Example to illustrate prioritizing github issues.  Basically, you
;; apply labels like 'priority:1' and 'priority:2' then you can
;; call forge-list-labeled-issues with LABEL as 'priority:%' to get
;; prioritized issues.
;;
;; As a fancier example, see the forge-list-orgit-labeled-issues-table
;; to get a sorted list of labeled issues as an org-table.

;;; Code:

(require 'forge-list)

(defun forge-list-labeled-issues (id label)
  "List issues of the repo with given ID that have LABEL.

Note that LABEL can be a SQL like pattern such as 'priority:%' so
that you can get a sorted list of a set of similar issues.
Basically, you apply labels like 'priority:1' and 'priority:2' to
your issues then you can call forge-list-labeled-issues with
LABEL as 'priority:%' to get prioritized issues.

See examples at https://github.com/emin63/magit-priority-example for details.

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

(defun forge-get-repo-ids (pattern &optional no-msg)
  "Get list of pairs of repo name and forge repo ID.

Does a forge-sql query to find the name and ID of repositories
that forge knows about filtered by the given sql PATTERN.  If you
provide \"%\" for pattern then all repos are returned but if you
provide something like \"foo%\" then only repos starting with \"foo\"
are returned.

If the optional NO-MSG evaluates to true, then no message is printed
to the message buffer while if it is false then the result is shown
in the message buffer as well as being returned.

This is useful for finding the repo ID for functions such
as forge-list-orgit-labeled-issues-table."
  (interactive (list (read-string "pattern: " "%")))
  (let ((info (forge-sql
   [:select
    :distinct [repository:name repository:id]
    :from [ repository ]
    :where
    (like repository:name $s1)
    ]
   pattern ;; query paramters
   )))
    (if (not no-msg)
	(message (format "%s" info)))
    info)
  )


(defun forge-list-labeled-issues-query (id label)
  "Query magit db with given ID for the desired LABEL.

This is a helper function which constructs a SQL query for the forge
database stored locally to get issues matching LABEL.  Note that you
can make label a 'like pattern' such as 'priority:%' to get things
which match multiple labels.

The return value is a list of items where each item has the following data:

  - issue ID in the forge db (this is some long weird string used for ID)
  - issue number (as you see on github)
  - issue summary
  - label we pulled for the issue

IMPORTANT: you should make sure that LABEL is a pattern which
does not match multiple labels on a single issue (e.g., do not
use a pattern like '%').  Otherwise issues will be listed multiple
times (once for each matching label)."
  (forge-sql
   [:select [$i1 label:name] ;; $i1 means first thing in params below
	    :from [issue issue_label label] :where
	    (and
	     (= issue_label:issue issue:id)
	     (= issue_label:id    label:id)
	     (= issue:repository  $s2) ; $s2 means second thing in params below
	     (like label:name     $s3) ; $se means third thing in params below
	     (isnull issue:closed)
	     )
	    :order-by [(asc label:name updated)]]
   ;; query parameters qre below
   (forge--tablist-columns-vector 'issue) id label)
  )

(defun forge-list-labeled-issues-put-label-in-title (issue-info)
  "Helper to take issue label and put it into the title.

This function takes in an ISSUE-INFO (i.e., an element of the list
produced by the forge-list-labeled-issues-query function) and puts
the label into the issue title."
  (setcar
   (nthcdr 2 issue-info) ;; title is in element 2
   (format "[%s] %s"
	   (nth 3 issue-info) ;; label is in element 3
	   (nth 2 issue-info)
	   )
   )
  )


(defun forge-list-labeled-issues-main (id label)
  "List issues of the repo with given ID that have LABEL.

This is intended to be called by forge-list-labeled-issues if
used interactively.

If user passes nil for ID, then we use forge-get-repository to find repo.
This makes it so that the user can bind a key to a command like

  (forge-list-labeled-issues-main nil \"priority:%\")

to easily get prioritized issues.

This function calls forge-topic-list-setup to display issues
using standard forge functions based on the output of
forge-list-labeled-issues-query (after tweaking the formatting
with forge-list-labeled-issues-put-label-in-title).  Ideally, we
should figure out how to work with the rest of the forge code so
that we can set a value in the forge-topic-list-columns variable
to tell it to display labels.  Since that seems complicated, it
seemed easier to just use
forge-list-labeled-issues-put-label-in-title as a simple hack for
now to display the label."
  (let ((id (or id  ;; if id is nil, try to load it ourselves
		(slot-value (forge-get-repository t) 'id))))
    (forge-topic-list-setup ;; function which displays results
	#'forge-issue-list-mode id nil nil
      (lambda () ;; we pass forge-topic-list-setup function to get the data
	(mapc    ;; use mapc to post-process results of our own forge-query
	 'forge-list-labeled-issues-put-label-in-title
	 (forge-list-labeled-issues-query id label)
	 )
	)
      )
    )
  )

(defun forge-list-labeled-issues-multi-query (id-list label)
  "Query multiple ID values for LABEL in ID-LIST and combine.

This is like forge-list-labeled-issues-query but combines
together lists from multiple repos.  The problem is that this does
not quite work the way you might want because the

  forge-list-labeled-issues-main

function calls the macro

  #'forge-issue-list-mode

which actually wants an ID not a list.  Partly it wants an ID not
a list because it wants to be able to know what repo an issue is
from in case you get multiple issues with the same ID."
  (let ((full-list nil))
    (dolist (my-id id-list full-list)
      (setq full-list
	    (append full-list
		    (forge-list-labeled-issues-query my-id "priority:%")
		    )
	    )
      )
    )
  )

(defun forge-list-labeled-issues-pquery (id label)
  "Return property list of data for issues from repo with ID and LABEL.

This queries a repository with the given ID for a repo with the
given LABEL.  You can obtain this ID using something
like `(slot-value (forge-get-repository t) 'id)`.

For example, if you use a label of `\"priority:%\"` then you will
get all labels which look like a priority setting."
  (forge-sql
   [:select
    [ 'id issue:id
	  'label label:name
	  'title issue:title
	  'number issue:number
	  'repo repository:name
    ]
   :from [issue issue_label label repository] :where
   (and
    (= issue:repository repository:id) ;; join issue to repository
    (= issue_label:issue issue:id)
    (= issue_label:id    label:id)
    (= issue:repository  $s1) ; $s1 means first thing in params below
    (like label:name     $s2) ; $s2 means second thing in params below
    (isnull issue:closed)
    )
   :order-by [(asc label:name updated)]
   ]
   id label ;; params list referenced by query
   )
  )


(defun forge-orgit-clean-string (my-str ttrim &optional ender)
  "Clean a string MY-STR by trimming it if longer than TTRIM and adding ENDER.

If MY-STR is too long, we trim it to length TTRIM and add the ending ENDER."
  (if ttrim
      (let ((slen (length my-str)))
	(if (> slen ttrim)
	    (concat
	     (substring my-str 0 ttrim)
	     (or ender "")
	     )
	  my-str
	  )
	)
    my-str
    )
  )

    
(defun forge-orgit-format-labeled-issue
    (issue-plist &optional ttrim ender)
  "Convert ISSUE-PLIST into string org-table row.

This takes as input an ISSUE-PLIST as produced by
forge-list-labeled-issues-pquery and converts it into an
org-table row.  This is meant to be called by the
forge-list-orgit-labeled-issues-table function.

Optionally you can provide TTRIM as a length to trim the title to
and ENDER as something to append when the title is
trimmed (e.g., \"..\")."
  (format "| [[orgit-topic:%s][%s#%s]] | %s | %s |"
	  (plist-get issue-plist 'id)
	  (plist-get issue-plist 'repo)
	  (plist-get issue-plist 'number)
	  (forge-orgit-clean-string
	   (plist-get issue-plist 'title) ttrim ender)
	  (plist-get issue-plist 'label)
	  )
  )

(defun forge-list-orgit-labeled-issues-table
    (id-list label &optional do-insert ttrim ender)
  "Convert ID-LIST of magit repos using LABEL into an org-table string.

The ID-LIST should be a list of orgit-forge repos.  You can obtain
this ID using something like `(slot-value (forge-get-repository t) 'id)`
and then make a list of those IDs.  You can also call forge-get-repo-ids.

The LABEL should be a SQL pattern such as \"priority:%\" for the
label you want to query.

The result is a string representing an org-table (sorted by
label) showing the matching issues.

If you provide the optional DO-INSERT, then we insert the result
into the current buffer in addition to providing it as a return
value.

The TTRIM and ENDER are passed down to forge
forge-orgit-format-labeled-issue.  See there for
details.  Suggested values are 45 and \"..\"."
  (let ((result nil))
    (dolist (my-id id-list)
      (setq result
	    (append result (forge-list-labeled-issues-pquery
			    my-id label)))
      )
    (setq result
	  (sort result
		(lambda (first second)
		  (string-lessp
		   (plist-get first 'label)
		   (plist-get second 'label)
		   )
		  )
		)
	  )
    (setq result
	  (concat "| Link | Title | Label |\n"
		  (mapconcat
		   (lambda (item)
		     (forge-orgit-format-labeled-issue
		      item ttrim ender)
		     ) result "\n")
		  )
	  )
    (if do-insert (insert result))
    result
    )
  )

;;; _
(provide 'magit-priority-example)
;;; magit-priority-example.el ends here
