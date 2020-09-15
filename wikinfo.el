;;; wikinfo.el --- Scrape Wikipedia Infoboxes -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/wikinfo
;; Created: September 14, 2020
;; Keywords: org, convenience
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; wikinfo's aim is to provide a simple elisp interface to Wikipedia's infoboxes.
;;

;;; Code:

(require 'url)
(require 'json)
(require 'dom)

;;; Custom Options
(defgroup wikinfo nil
  "Wikipedia infobox to Elisp bridge"
  :group 'development
  :prefix "wikinfo-")

(defcustom wikinfo-api-endpoint "https://en.wikipedia.org/w/api.php?"
  "API endpoint for queries and searches."
  :type 'string)

(defcustom wikinfo-search-params '("&action=query"
                                   "&generator=search"
                                   "&gsrsearch=hastemplate:infobox+"
                                   "%s"
                                   "&gsrlimit=20"
                                   "&gsrinfo=suggestion"
                                   "&gsrnamespace=0"
                                   "&gsrwhat=text"
                                   "&prop=extracts"
                                   "&exintro"
                                   "&explaintext"
                                   "&exlimit=max"
                                   "&exsentences=3"
                                   "&format=json")
  "Search query parameters."
  :type 'string)

(defcustom wikinfo-parse-params '("&action=parse"
                                  "&pageid="
                                  "%s"
                                  "&prop=text"
                                  "&section=0"
                                  "&format=json")
  "Page parsing query parameters."
  :type 'string)

(defface wikinfo-search-title '((t (:weight bold :height 1.05)))
  "Face for search result extracts.")

(defun wikinfo--plist-path (plist &rest path)
  "Recusrively retrive PATH from PLIST."
  (unless (listp plist)
    (user-error "Plist is not a list"))
  (while path
    (setq plist (plist-get plist (pop path))))
  plist)

(defun wikinfo--url-params (param-list query)
  "Replace query symbol in PARAM-LIST with QUERY string."
  (format (string-join param-list) query))

(defun wikinfo--json (url)
  "Get JSON from URL. Return a JSON object."
  (message "API URL: %s" url)
  (with-current-buffer (url-retrieve-synchronously url)
    (kill-region (point-min)
                 (save-match-data
                   (re-search-forward "^\n" nil t)
                   (point)))
    (let* ((json-object-type 'plist)
           (json-array-type 'list))
      (json-read-from-string (buffer-string)))))

;;@UNFINISHED: auto implementation
(defun wikinfo-search (&optional query _auto)
  "Search wikipedia for QUERY.
Return plist with page metadata.
If AUTO is non-nil, return first search result."
  (interactive)
  (if-let* ((query (or query (read-string "query: ")))
            (url (concat wikinfo-api-endpoint
                         (wikinfo--url-params wikinfo-search-params query)))
            (JSON (wikinfo--json url))
            (pages (cdr (wikinfo--plist-path JSON :query :pages)))
            (candidates
             (mapcar (lambda (page)
                       (when-let ((extract (plist-get page :extract))
                                  (id      (plist-get page :pageid))
                                  (title   (plist-get page :title))
                                  (index   (plist-get page :index)))
                         (cons (concat (propertize title
                                                   'face 'wikinfo-search-title)
                                       "\n" extract "\n")
                               `( :extract ,extract
                                  :index   ,index
                                  :title   ,title
                                  :title   ,title
                                  :id      ,id))))
                     pages))
            (sorted (sort (delq nil candidates)
                          (lambda (a b)
                            (< (plist-get (cdr a) :index)
                               (plist-get (cdr b) :index)))))
            (choice (completing-read "wikinfo: "
                                     (mapcar #'car sorted)
                                     nil 'require-match)))
      (alist-get choice sorted nil nil #'string=)
    ;;@TODO: Fix this. Needs to be more robust.
    (user-error "Query \"%s\" failed" query)))

;;@TODO:
(defun wikinfo--sanitize-string (string)
  "Remove unwanted characters, trim STRING."
  string)
(defun wikinfo--string-to-keyword (string)
  "Return keyword from STRING."
  string)

;;@TODO: extract from wikinfo-infobox
(defun wikinfo--clean-key (string)
  "Return a keyword from STRING."
  (intern
   (concat ":" (replace-regexp-in-string "\\((\\|)\\)" "" string))))

(defun wikinfo-infobox (page-id)
  "Return wikipedia infobox as plist for page with PAGE-ID."
  (let* ((url (concat wikinfo-api-endpoint
                      (wikinfo--url-params wikinfo-parse-params page-id)))
         (JSON (wikinfo--json url))
         (wikitext-html (wikinfo--plist-path JSON :parse :text :*))
         (html (with-temp-buffer
                 (insert wikitext-html)
                 (libxml-parse-html-region (point-min) (point-max))))
         ;;@ERROR if not found
         (table (dom-by-class html "infobox.*"))
         (rows (dom-by-tag table 'tr))
         (entity (dom-texts (car rows)))
         result)
    (dolist (row rows result)
      (when-let* ((header (dom-by-tag row 'th))
                  (data (car (mapcar #'dom-strings
                                     ;;@TODO: decompose into function
                                     ;;remove unwanted elements


                                     (mapcar (lambda (td)
                                               (seq-filter (lambda (el) (not (member (car-safe el) '(style))))
                                                           td))
                                             (dom-by-tag row 'td)))))
                  (header-texts (thread-last
                                    (downcase (dom-texts header))
                                  (replace-regexp-in-string "\\(?:[[:space:]]\\)" "-")
                                  (replace-regexp-in-string "[^[:alnum:]-]" "")
                                  (replace-regexp-in-string "--" "-")
                                  (replace-regexp-in-string "-$" "")
                                  (replace-regexp-in-string "^-" ""))))
        (setq result (plist-put result
                                (intern (concat ":" header-texts))
                                (thread-last
                                    data
                                  (mapcar #'string-trim)
                                  (mapcar (lambda (el)
                                            (replace-regexp-in-string " " " " el)))
                                  (seq-filter
                                   (lambda (el)
                                     (not (or (string-match-p "^[^[:alnum:]]*$" el)
                                              (string-match-p "\\(?:\\[[[:digit:]]*]\\)" el))))))))))
    (plist-put result :wikinfo-entity (list (string-trim entity)))))

;;@TODO: need to think about interface...
;; there should be a way to do this programmatically
;; e.g. google's im-feelin-lucky, but allow user to define what "luck" is
;; by accepting a sorting predicate before taking car of results
(defun wikinfo (&optional _arg search)
  "Return infobox plist for SEARCH.
If ARG is non-nil, use first result (a la google's \"I'm feelin' lucky\")."
  (let ((query (wikinfo-search search)))
    (plist-put (wikinfo-infobox (plist-get query :id))
                                :wikinfo-extract (plist-get query :extract))))

(provide 'wikinfo)

;;; wikinfo.el ends here
