;;; magit-priority-example.el --- Example code for magit/forge  -*- lexical-binding: t; -*-

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
;; apply labels like "priority:1" and "priority:2" to your issues.
;;
;; The main commands are:
;;
;;   - `magit-priority-list-issues' shows open issues from every
;;     repository Forge tracks, sorted by priority, in a list buffer.
;;   - `magit-priority-org-table' inserts the same list as an Org
;;     table at point.
;;
;; Both ask for an extra label (which may contain SQL `%' wildcards).
;; Only issues carrying that label are listed, prioritized ones first
;; and unprioritized ones at the bottom.  If you leave the extra label
;; empty you get every prioritized issue.  With a prefix argument
;; (C-u) only issues assigned to you are listed.
;;
;; The older entry points `forge-list-labeled-issues' and
;; `forge-list-orgit-labeled-issues-table' still work; see the README
;; at https://github.com/emin63/magit-priority-example for details.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
;; Forge renamed forge-list.el to forge-topics.el in April 2024, so we
;; require the main `forge' feature instead of `forge-list'.
(require 'forge)

(declare-function org-table-align "org-table" ())

;;;; Making untrusted text safe to display
;;
;; Issue titles and label names come from the forge, so anyone who
;; can open an issue (or later edit its title) controls them.  Before
;; showing them we clean them as follows.
;;
;;   - Newlines and other control characters become spaces, so the
;;     text cannot start new lines.  In an Org buffer a new line could
;;     otherwise become a new table row, a heading, a code block or a
;;     #+TBLFM formula line.
;;
;;   - In Org table cells, "|" becomes \vert{} (Org's entity for a
;;     vertical bar) so it cannot end the cell early.
;;
;;   - In Org table cells, two things Org would otherwise *act on* are
;;     defused by inserting an invisible ZERO WIDTH SPACE (U+200B):
;;
;;       * A cell starting with "=" or ":=" is an Org table formula.
;;         With Org's default `org-table-formula-evaluate-inline',
;;         pressing TAB or RET in such a cell evaluates it as Lisp
;;         WITHOUT asking.  A zero width space in front of the "="
;;         stops Org from seeing a formula.
;;
;;       * "[[" starts an Org link, whose visible text can lie about
;;         where it goes.  "[<zero width space>[" is not a link.
;;
;;     The zero width space is invisible, so the text looks unchanged,
;;     but it is really there: searching for or copying such a title
;;     includes it.  If you see a title that does not match what you
;;     typed into a search, this is probably why.
;;
;; Other Org markup (e.g. *bold*) is left alone.  Org asks before
;; running inline code or following elisp:/shell: links, so we do not
;; try to defuse those.

(defconst magit-priority--zero-width-space "\u200b"
  "Invisible character used to stop Org from interpreting text.
See the comment above `magit-priority--one-line'.")

(defun magit-priority--one-line (string)
  "Return STRING with each run of control characters replaced by a space.
Newlines and tabs are control characters.  A nil STRING gives \"\"."
  (replace-regexp-in-string "[[:cntrl:]]+" " " (or string "")))

(defun magit-priority--org-cell (string)
  "Return STRING made safe to insert as plain text in an Org table cell.
This puts STRING on one line, replaces \"|\" with \\vert{}, and adds
invisible zero width spaces to stop Org treating the text as a
formula or a link.  See the comment above `magit-priority--one-line'
for why."
  (let ((zws magit-priority--zero-width-space)
        (cell (string-trim (magit-priority--one-line string))))
    (setq cell (string-replace "[[" (concat "[" zws "[") cell))
    (setq cell (string-replace "|" "\\vert{}" cell))
    (if (string-match-p "\\`:?=" cell) ; would be an Org table formula
        (concat zws cell)
      cell)))

(defun magit-priority--nonblank (string)
  "Return STRING, or nil if it is nil or contains only whitespace."
  (and string (not (string-blank-p string)) string))

(defun forge-list-labeled-issues (id label)
  "List issues of the repo with given ID that have LABEL.

Note that LABEL can be a SQL like pattern such as \"priority:%\" so
that you can get a sorted list of a set of similar issues.
Basically, you apply labels like \"priority:1\" and \"priority:2\" to
your issues then you can call forge-list-labeled-issues with
LABEL as \"priority:%\" to get prioritized issues.

This is now a thin wrapper around `magit-priority-list-issues',
restricted to a single repository.  To list issues from every
repository Forge tracks, use `magit-priority-list-issues' directly.

See examples at https://github.com/emin63/magit-priority-example for details.

Issues are listed in a separate buffer."
  (interactive  ;; interactively get desired arguments from user
   (let ((repo (forge-get-repository :tracked))) ;; current magit repo
     (list
      (oref repo id)
      (completing-read  ;; use completing-read so we can set require-match nil
       "Label: "        ;; will ask user for desired label
       (mapcar #'cadr (oref repo labels)) ;; gets known labels
       nil ;; predicate for completing-read means keep all labels
       nil ;; allow nil so user can use a % pattern
       )
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
	(message "%s" info))
    info)
  )


(defun forge-list-labeled-issues-query (id label)
  "Return open issues in the repo with given ID that have LABEL.

LABEL can be a like pattern such as \"priority:%\" to get things
which match multiple labels.

Older versions of this function returned rows in the format used
by Forge's old list buffers, which no longer exist.  It now
returns the same property lists as `magit-priority-issues' (which
see), with one entry per issue even if several labels match."
  (magit-priority-issues nil nil (list id) label)
  )
(make-obsolete 'forge-list-labeled-issues-query
               'magit-priority-issues "2026-10-20")

(defun forge-list-labeled-issues-put-label-in-title (issue-info)
  "Helper to take issue label and put it into the title.

This function takes in an ISSUE-INFO (i.e., an element of the list
produced by the old forge-list-labeled-issues-query function) and puts
the label into the issue title.

It is no longer used: `magit-priority-list-issues' shows the
priority in its own column."
  (setcar
   (nthcdr 2 issue-info) ;; title is in element 2
   (format "[%s] %s"
	   (nth 3 issue-info) ;; label is in element 3
	   (nth 2 issue-info)
	   )
   )
  )
(make-obsolete 'forge-list-labeled-issues-put-label-in-title
               nil "2026-10-20")


(defun forge-list-labeled-issues-main (id label)
  "List issues of the repo with given ID that have LABEL.

This is intended to be called by forge-list-labeled-issues if
used interactively.

If user passes nil for ID, then we use forge-get-repository to find repo.
This makes it so that the user can bind a key to a command like

  (forge-list-labeled-issues-main nil \"priority:%\")

to easily get prioritized issues.

This used to build a Forge topic list buffer, but Forge removed
the functions it relied on.  It now calls
`magit-priority-list-issues' restricted to the repo with ID,
treating LABEL as the priority pattern, so matching issues are
sorted by LABEL and the matching label is shown in the Pri
column."
  (let ((id (or id  ;; if id is nil, try to load it ourselves
		(oref (forge-get-repository :tracked) id))))
    (magit-priority-list-issues nil nil (list id) label)
    )
  )

(defun forge-list-labeled-issues-multi-query (id-list label)
  "Query multiple ID values for LABEL in ID-LIST and combine.

This is like forge-list-labeled-issues-query but combines
together lists from multiple repos.  You probably want
`magit-priority-issues' instead, which accepts a list of repo IDs
directly (or nil for all repos) and sorts the combined result."
  (let ((full-list nil))
    (dolist (my-id id-list full-list)
      (setq full-list
	    (append full-list
		    (magit-priority-issues nil nil (list my-id) label)
		    )
	    )
      )
    )
  )
(make-obsolete 'forge-list-labeled-issues-multi-query
               'magit-priority-issues "2026-10-20")

(defun forge-list-labeled-issues-pquery (id label)
  "Return property list of data for issues from repo with ID and LABEL.

This queries a repository with the given ID for a repo with the
given LABEL.  You can obtain this ID using something
like `(oref (forge-get-repository :tracked) id)`.  If ID is nil,
every repository Forge tracks is queried.

LABEL is an SQL LIKE pattern: `%' matches any run of characters,
`_' matches any single character, and matching ignores case.

For example, if you use a label of `\"priority:%\"` then you will
get all labels which look like a priority setting."
  (forge-sql
   `[:select
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
    ,@(if id
          `((= issue:repository ,id))
        '((= repository:condition :tracked))) ;; nil id means all tracked
    (like label:name     ,label)
    (isnull issue:closed)
    )
   :order-by [(asc label:name updated)]
   ]
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
trimmed (e.g., \"..\").

The title and label come from the forge, so they are passed through
`magit-priority--org-cell', which may add invisible zero width spaces
to stop Org running them as formulas or treating them as links."
  (format "| [[orgit-topic:%s][%s#%s]] | %s | %s |"
	  (plist-get issue-plist 'id)
	  (plist-get issue-plist 'repo)
	  (plist-get issue-plist 'number)
	  (magit-priority--org-cell
	   (forge-orgit-clean-string
	    (plist-get issue-plist 'title) ttrim ender))
	  (magit-priority--org-cell (plist-get issue-plist 'label))
	  )
  )

(defun forge-list-orgit-labeled-issues-table
    (id-list label &optional do-insert ttrim ender)
  "Convert ID-LIST of magit repos using LABEL into an org-table string.

The ID-LIST should be a list of orgit-forge repos.  You can obtain
this ID using something like `(oref (forge-get-repository :tracked) id)`
and then make a list of those IDs.  You can also call forge-get-repo-ids.
If ID-LIST is nil, every repository Forge tracks is used.

The LABEL should be a SQL pattern such as \"priority:%\" for the
label you want to query.  As in any SQL LIKE pattern, `_' also
matches any single character and matching ignores case.

The result is a string representing an org-table (sorted by
label) showing the matching issues.

If you provide the optional DO-INSERT, then we insert the result
into the current buffer in addition to providing it as a return
value.

The TTRIM and ENDER are passed down to forge
forge-orgit-format-labeled-issue.  See there for
details.  Suggested values are 45 and \"..\".

See also `magit-priority-org-table', which can additionally
filter by a second label and by assignee."
  (let ((result nil))
    (dolist (my-id (or id-list '(nil))) ;; '(nil) means all repos
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
	  (concat "|------+-------+-------|\n"
		  "| Link | Title | Label |\n"
		  "|------+-------+-------|\n"
		  (mapconcat
		   (lambda (item)
		     (forge-orgit-format-labeled-issue
		      item ttrim ender)
		     ) result "\n")
		  "\n|---|\n")
	  )
    (if do-insert (insert result))
    result
    )
  )

;;;; Priority-sorted issues across all repositories

(defgroup magit-priority nil
  "List Forge issues sorted by priority:N labels."
  :group 'forge)

(defcustom magit-priority-label-pattern "priority:%"
  "SQL LIKE pattern matching the labels that set an issue's priority.
The first number in a matching label is the priority, and lower
numbers sort first, so \"priority:1\" comes before \"priority:2\"."
  :type 'string)

(defcustom magit-priority-title-width 60
  "Trim titles longer than this in `magit-priority-org-table'.
Trimmed titles end in \"..\".  If nil, titles are never trimmed."
  :type '(choice (const :tag "Never trim" nil) integer))

(defun magit-priority--open-issues (pattern repo-ids)
  "Return open issues that have a label matching PATTERN.
PATTERN is an SQL LIKE pattern.  Each row has the form
\(ID NUMBER TITLE OWNER NAME REPO-ID).  If REPO-IDS is non-nil,
only issues from those repositories are returned; otherwise only
issues from repositories Forge tracks.  (Forge also stores some
repositories you do not track, e.g. ones it learned about while
pulling notifications.)"
  (forge-sql
   `[:select :distinct [issue:id issue:number issue:title
                        repository:owner repository:name repository:id]
     :from issue
     :join repository  :on (= repository:id issue:repository)
     :join issue-label :on (= issue-label:issue issue:id)
     :join label       :on (= label:id issue-label:id)
     :where (and (in issue:state [open])
                 (like label:name ,pattern)
                 ,@(if repo-ids
                       `((in issue:repository ,(vconcat repo-ids)))
                     '((= repository:condition :tracked))))]))

(defun magit-priority--matching-labels (pattern issue-ids)
  "Return (ISSUE-ID LABEL) rows for labels matching PATTERN on ISSUE-IDS."
  (and issue-ids
       (forge-sql
        `[:select [issue-label:issue label:name]
          :from issue-label
          :join label :on (= label:id issue-label:id)
          :where (and (like label:name ,pattern)
                      (in issue-label:issue ,(vconcat issue-ids)))])))

(defun magit-priority--assignees (issue-ids)
  "Return (ISSUE-ID LOGIN) rows for the assignees of ISSUE-IDS."
  (and issue-ids
       (forge-sql
        `[:select [issue-assignee:issue assignee:login]
          :from issue-assignee
          :join assignee :on (= assignee:id issue-assignee:id)
          :where (in issue-assignee:issue ,(vconcat issue-ids))])))

(defun magit-priority--rank (label)
  "Return the first number in LABEL, or nil if there is none."
  (and label
       (string-match "[0-9]+" label)
       (string-to-number (match-string 0 label))))

(defun magit-priority--label< (a b)
  "Return non-nil if priority label A should sort before label B.
Labels containing a number come first, lowest number first.  Then
come labels without a number, alphabetically.  A nil label (an
unprioritized issue) comes last."
  (let ((ra (magit-priority--rank a))
        (rb (magit-priority--rank b)))
    (cond ((and ra rb) (< ra rb))
          (ra t)
          (rb nil)
          ((and a b) (string< a b))
          (t (and a (not b))))))

(defun magit-priority--issue< (a b)
  "Return non-nil if issue plist A should sort before issue plist B.
Sort by priority label, then by repository, then by issue number."
  (let ((la (plist-get a :label))
        (lb (plist-get b :label))
        (ra (plist-get a :repo))
        (rb (plist-get b :repo)))
    (cond ((magit-priority--label< la lb) t)
          ((magit-priority--label< lb la) nil)
          ((string= ra rb) (< (plist-get a :number) (plist-get b :number)))
          (t (string< ra rb)))))

(defun magit-priority--my-usernames (repo-ids)
  "Return an alist mapping each of REPO-IDS to your username there.
Forge stores usernames per forge host (e.g. in the git variable
`github.user'), so they can differ between repositories.
Repositories where the username cannot be determined are left
out, with a message naming them."
  (let (alist failed)
    (dolist (id (delete-dups (copy-sequence repo-ids)))
      (let ((repo (forge-get-repository :id id)))
        (condition-case nil
            (push (cons id (ghub--username repo)) alist)
          (error (push (if repo (oref repo slug) id) failed)))))
    (when failed
      (message "magit-priority: no username configured for %s; skipped"
               (string-join failed ", ")))
    alist))

(defun magit-priority--only-mine (issues)
  "Return the members of ISSUES that are assigned to you.
Usernames are compared exactly (case matters), as Forge does."
  (let ((me (magit-priority--my-usernames
             (mapcar (lambda (i) (plist-get i :repo-id)) issues)))
        (assigned (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id ,login)
                   (magit-priority--assignees
                    (mapcar (lambda (i) (plist-get i :id)) issues)))
      (push login (gethash id assigned)))
    (cl-remove-if-not
     (lambda (issue)
       (when-let* ((login (cdr (assoc (plist-get issue :repo-id) me))))
         (member login (gethash (plist-get issue :id) assigned))))
     issues)))

(defun magit-priority-issues (&optional extra-label assigned-to-me
                                        repo-ids priority-pattern)
  "Return open issues sorted by priority.

If EXTRA-LABEL is a non-blank string, return issues that have a
label matching it.  It is an SQL LIKE pattern, so \"area:%\"
matches any label starting with \"area:\".  Note that, as in any
LIKE pattern, `_' also matches any single character (so
\"needs_review\" also matches \"needs-review\") and matching
ignores case.  If EXTRA-LABEL is nil, empty or only whitespace,
return issues that have a priority label instead.

Issues are sorted by their priority label (see
`magit-priority-label-pattern'), most urgent first, with issues
lacking one at the end.  If an issue has several priority labels
the most urgent one counts.  Ties are broken by repository and
then issue number.

If ASSIGNED-TO-ME is non-nil, only return issues assigned to you.

If REPO-IDS is a list of Forge repository IDs, only search those
repositories; if nil, search every repository Forge tracks.  See
`forge-get-repo-ids' for finding IDs.

PRIORITY-PATTERN overrides `magit-priority-label-pattern'.

Each issue is a property list with the keys :id (the Forge ID),
:number, :title (on one line; see `magit-priority--one-line'),
:repo (\"owner/name\"), :name (the repository name without the
owner), :repo-id, :label (the priority label, or nil) and
:priority (the number in :label, or nil)."
  (let* ((ppat (or priority-pattern magit-priority-label-pattern))
         (extra (magit-priority--nonblank extra-label))
         (rows (magit-priority--open-issues (or extra ppat) repo-ids))
         (best (make-hash-table :test #'equal))
         (issues nil))
    (pcase-dolist (`(,id ,label)
                   (magit-priority--matching-labels ppat (mapcar #'car rows)))
      (let ((old (gethash id best)))
        (when (or (not old) (magit-priority--label< label old))
          (puthash id label best))))
    (pcase-dolist (`(,id ,number ,title ,owner ,name ,repo-id) rows)
      (let ((label (gethash id best)))
        (push (list :id id :number number
                    :title (magit-priority--one-line title)
                    :repo (format "%s/%s" owner name) :name name
                    :repo-id repo-id
                    :label label :priority (magit-priority--rank label))
              issues)))
    (when assigned-to-me
      (setq issues (magit-priority--only-mine issues)))
    (sort issues #'magit-priority--issue<)))

(defun magit-priority--read-label ()
  "Read an extra label with completion, or return nil if left blank."
  (magit-priority--nonblank
   (completing-read
    "Extra label (% is a wildcard, empty for all prioritized): "
    (delete-dups
     (mapcar #'car (forge-sql [:select :distinct [name] :from label]))))))

(defun magit-priority--describe (issue)
  "Return the text for the priority column of ISSUE.
A label without a number is shown as is (on one line)."
  (let ((priority (plist-get issue :priority))
        (label (plist-get issue :label)))
    (cond (priority (number-to-string priority))
          (label (magit-priority--one-line label))
          (t "-"))))

(defun magit-priority--filter-description (extra-label assigned-to-me)
  "Describe the filter given by EXTRA-LABEL and ASSIGNED-TO-ME."
  (concat (if (magit-priority--nonblank extra-label)
              (format "label %s" extra-label)
            "all prioritized")
          (and assigned-to-me ", assigned to you")))

;;;;; Org table

(defun magit-priority--org-table-string (issues)
  "Return an Org table string listing ISSUES.
The columns are Issue (a link labeled with the repository name,
without the owner, and the issue number), Title and Pri.
Text from the forge goes through `magit-priority--org-cell', which
may add invisible zero width spaces; see the comment above
`magit-priority--one-line'."
  (concat
   "|-------+-------+-----|\n"
   "| Issue | Title | Pri |\n"
   "|-------+-------+-----|\n"
   (mapconcat
    (lambda (issue)
      (format "| [[orgit-topic:%s][%s#%s]] | %s | %s |\n"
              (plist-get issue :id)
              (plist-get issue :name)
              (plist-get issue :number)
              (magit-priority--org-cell
               (if magit-priority-title-width
                   (truncate-string-to-width (plist-get issue :title)
                                             magit-priority-title-width
                                             nil nil "..")
                 (plist-get issue :title)))
              (magit-priority--org-cell (magit-priority--describe issue))))
    issues "")
   "|-------+-------+-----|\n"))

(defun magit-priority-org-table (extra-label &optional assigned-to-me
                                             repo-ids no-insert)
  "Insert an Org table of open issues with EXTRA-LABEL, sorted by priority.

Interactively, read EXTRA-LABEL with completion; it may contain
SQL `%' wildcards, and if left empty every prioritized issue is
listed.  With a prefix argument, only list issues assigned to
you (ASSIGNED-TO-ME).

Issues come from every repository Forge tracks unless REPO-IDS
is a list of repository IDs.  Each row links to the issue with an
orgit-topic link, which opens it in Forge if `orgit-forge' is
installed.  The table is inserted at point, and aligned if the
buffer is in Org mode, unless NO-INSERT is non-nil.  It is
returned as a string either way.

See `magit-priority-issues' for how issues are chosen and sorted."
  (interactive (list (magit-priority--read-label) current-prefix-arg))
  (let* ((issues (magit-priority-issues extra-label assigned-to-me repo-ids))
         (table (magit-priority--org-table-string issues)))
    (unless no-insert
      (unless (bolp) (insert "\n"))
      (let ((start (point)))
        (insert table)
        (when (derived-mode-p 'org-mode)
          (save-excursion
            (goto-char start)
            (org-table-align)))))
    (when (called-interactively-p 'interactive)
      (message "%d issue(s): %s" (length issues)
               (magit-priority--filter-description extra-label
                                                   assigned-to-me)))
    table))

;;;;; List buffer

(defvar-local magit-priority--list-args nil
  "Arguments to `magit-priority-issues' for the current list buffer.")

(defvar-keymap magit-priority-list-mode-map
  :doc "Keymap for `magit-priority-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'magit-priority-visit-issue
  "b"   #'magit-priority-browse-issue)

(defun magit-priority--sort-by-priority (a b)
  "Compare tabulated list entries A and B by priority."
  (magit-priority--issue< (car a) (car b)))

(defun magit-priority--sort-by-number (a b)
  "Compare tabulated list entries A and B by issue number."
  (< (plist-get (car a) :number) (plist-get (car b) :number)))

(define-derived-mode magit-priority-list-mode tabulated-list-mode
  "Priority Issues"
  "Major mode for issues listed by `magit-priority-list-issues'.

Click a column header to sort by it.  Type \\[revert-buffer] to re-run
the query (e.g. after `forge-pull'), \\[magit-priority-visit-issue] to open
the issue in Forge, or \\[magit-priority-browse-issue] to open it in a browser.

\\{magit-priority-list-mode-map}"
  (setq tabulated-list-format
        [("Pri" 4 magit-priority--sort-by-priority)
         ("Repository" 28 t)
         ("#" 6 magit-priority--sort-by-number :right-align t)
         ("Title" 0 t)])
  (setq tabulated-list-padding 1)
  ;; Start in the order `magit-priority-issues' returns, even if the
  ;; buffer was previously sorted by another column.
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'magit-priority--refresh nil t)
  (tabulated-list-init-header))

(defun magit-priority--refresh ()
  "Re-run the query for the current `magit-priority-list-mode' buffer."
  (setq tabulated-list-entries
        (mapcar (lambda (issue)
                  (list issue
                        (vector (magit-priority--describe issue)
                                (plist-get issue :repo)
                                (number-to-string (plist-get issue :number))
                                (plist-get issue :title))))
                (apply #'magit-priority-issues magit-priority--list-args))))

(defun magit-priority-list-issues (extra-label &optional assigned-to-me
                                               repo-ids priority-pattern)
  "List open issues with EXTRA-LABEL, sorted by priority, in a buffer.

Interactively, read EXTRA-LABEL with completion; it may contain
SQL `%' wildcards, and if left empty every prioritized issue is
listed.  With a prefix argument, only list issues assigned to
you (ASSIGNED-TO-ME).

Issues come from every repository Forge tracks unless REPO-IDS
is a list of repository IDs.  PRIORITY-PATTERN overrides
`magit-priority-label-pattern'.  See `magit-priority-issues' for
how issues are chosen and sorted, and `magit-priority-list-mode'
for the keys available in the buffer."
  (interactive (list (magit-priority--read-label) current-prefix-arg))
  (with-current-buffer (get-buffer-create "*magit-priority*")
    (magit-priority-list-mode)
    (setq magit-priority--list-args
          (list extra-label assigned-to-me repo-ids priority-pattern))
    (setq mode-line-process
          (format " [%s]" (magit-priority--filter-description
                           extra-label assigned-to-me)))
    (magit-priority--refresh)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))
    (message "%d issue(s)" (length tabulated-list-entries))))

(defun magit-priority--issue-at-point ()
  "Return the Forge issue object on the current line."
  (let ((entry (or (tabulated-list-get-id)
                   (user-error "No issue on this line"))))
    (or (forge-get-issue (plist-get entry :id))
        (user-error "Issue no longer in the Forge database; press g"))))

(defun magit-priority-visit-issue ()
  "Open the issue on the current line in a Forge buffer."
  (interactive)
  (forge-topic-setup-buffer (magit-priority--issue-at-point)))

(defun magit-priority-browse-issue ()
  "Open the issue on the current line in a web browser."
  (interactive)
  (browse-url (forge-get-url (magit-priority--issue-at-point))))

;;; _
(provide 'magit-priority-example)
;;; magit-priority-example.el ends here
