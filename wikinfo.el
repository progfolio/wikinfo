;;; wikinfo.el --- Scrape Wikipedia Infoboxes -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/wikinfo
;; Created: September 14, 2020
;; Keywords: org, convenience
;; Package-Requires: ((emacs "27.1"))
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
(require 'dom)
(eval-when-compile (require 'subr-x))

;;; Custom Options
(defgroup wikinfo nil
  "Wikipedia infobox to Elisp bridge"
  :group 'development
  :prefix "wikinfo-")

(defcustom wikinfo-base-url "https://en.wikipedia.org"
  "Base URL used for API URLS."
  :type 'string)

(defcustom wikinfo-api-endpoint "/w/api.php?"
  "API endpoint for queries and searches."
  :type 'string)

;;@TODO: grab page urls in this query?
(defcustom wikinfo-search-params '("&action=query"
                                   "&generator=search"
                                   "&gsrsearch=hastemplate:\"infobox\"+"
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

(defun wikinfo--url ()
  "RETURN base URL for QUERY."
  (concat wikinfo-base-url wikinfo-api-endpoint))

(defun wikinfo--json (url)
  "Get JSON from URL. Return a JSON object."
  (message "API URL: %s" url)
  (with-current-buffer (url-retrieve-synchronously url)
    (kill-region (point-min)
                 (save-match-data
                   (re-search-forward "^\n" nil t)
                   (point)))
    (json-parse-string (buffer-string)
                       :object-type 'plist)))

(defun wikinfo-search (&optional query predicate)
  "Search wikipedia for QUERY. Return plist with page metadata.
PREDICATE must be a unary function which accepts the QUERY result list.
It must return a single result. If nil, the user is prompted."
  (if-let* ((query (or query (read-string "query: ")))
            (url (concat (wikinfo--url)
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
                                       "\n" extract)
                               `( :extract ,extract
                                  :index   ,index
                                  :title   ,title
                                  :id      ,id))))
                     pages))
            (sorted (sort (delq nil candidates)
                          (lambda (a b)
                            (< (plist-get (cdr a) :index)
                               (plist-get (cdr b) :index))))))
      (if predicate
          (funcall predicate (mapcar #'cdr sorted))
        (alist-get (completing-read "wikinfo: "
                                    (mapcar #'car sorted)
                                    nil 'require-match)
                   sorted nil nil #'string=))
    ;;@TODO: Fix this. Needs to be more robust.
    (user-error "Query %S failed" query)))

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
  (let* ((url (concat (wikinfo--url)
                      (wikinfo--url-params wikinfo-parse-params page-id)))
         (JSON (wikinfo--json url))
         (wikitext-html (wikinfo--plist-path JSON :parse :text :*))
         (html (with-temp-buffer
                 (insert wikitext-html)
                 (libxml-parse-html-region (point-min) (point-max))))
         ;;@ERROR if not found
         (table (dom-by-class html "infobox.*"))
         (rows (dom-by-tag table 'tr))
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
        (setq result
              (plist-put result
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
    result))

(defun wikinfo (&optional search predicate)
  "Return infobox plist for SEARCH.
PREDICATE and SEARCH are passed to `wikinfo-search'."
  (let* ((query (wikinfo-search search predicate))
         (infobox (wikinfo-infobox (plist-get query :id))))
    (plist-put infobox :wikinfo query)))

(provide 'wikinfo)

;;; wikinfo.el ends here
