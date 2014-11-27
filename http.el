;;; http.el --- An HTTP client for Emacs -*- lexical-binding: t -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/http.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (s "1.9.0") (request "0.2.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; [![Travis build status](https://travis-ci.org/emacs-pe/http.el.png?branch=master)](https://travis-ci.org/emacs-pe/http.el)
;;
;; `http.el' provides an easy way to interact with the HTTP protocol.

;;; Usage:
;; Create a file with the following contents, and set `http-mode' as major mode.
;;
;;     # -*- http -*-
;;
;;     POST https://httpbin.org/post?val=key
;;     User-Agent: Emacs24
;;     Content-Type: application/json
;;
;;     {
;;       "foo": "bar"
;;     }
;;
;; Move the cursor somewhere within the description of the http request and
;; execute `M-x http-process` or press `C-c C-c`, if everything is went
;; well should show an buffer when the response of the http request:
;; ![http.el screenshot](misc/screenshot.png)
;;
;; More examples are included in file [misc/example.txt](misc/example.txt)

;;; TODO
;; + [ ] Add code block support for org-mode

;;; Related projects
;; + [httprepl.el](https://github.com/gregsexton/httprepl.el): An HTTP REPL for Emacs.
;; + [restclient.el](https://github.com/pashky/restclient.el): HTTP REST client tool for Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'json)
(require 'rfc2231)
(require 'url-util)

(require 's)
(require 'request)

(defgroup http nil
  "HTTP client for Emacs."
  :prefix "http-"
  :group 'applications)

(defcustom http-buffer-response-name "*HTTP Response*"
  "Name for response buffer."
  :type 'string
  :group 'http)

(defcustom http-show-response-headers t
  "Show response headers."
  :type 'boolean
  :group 'http)

(defcustom http-response-comment-start "//"
  "String used as `comment-start' when was not possible to guess a response content-type."
  :type 'string
  :group 'http)

(defconst http-methods-list
  '("GET" "POST" "DELETE" "PUT" "HEAD" "OPTIONS" "PATCH"))

(defconst http-keywords-regexp
  (concat "^\\s-*" (regexp-opt http-methods-list 'words)))

(defconst http-header-regexp
  "^\\([A-Za-z0-9_-]+\\):[ ]*\\(.*\\)")

(defconst http-content-type-alist
  '(("text/xml" . xml-mode)
    ("application/xml" . xml-mode)
    ("application/atom+xml" . xml-mode)
    ("application/atomcat+xml" . xml-mode)
    ("application/json" . js-mode)
    ("text/html" . html-mode)
    ("text/plain" . text-mode)
    ("image/gif" . image-mode)
    ("image/png" . image-mode)
    ("image/jpeg" . image-mode)
    ("image/x-icon" . image-mode)))

(defun http-query-alist (query)
  "Return an alist of QUERY string."
  (cl-loop for (key val) in (url-parse-query-string query)
           collect (cons key val)))

(defun http-parse-headers (start end)
  "Return the parsed http headers from START to END point."
  (let* ((hlines (split-string (buffer-substring-no-properties start end) "\n"))
         (header-alist (mapcar (lambda (hdrline)
                                 (when (string-match http-header-regexp hdrline)
                                   (cons
                                    (downcase (match-string 1 hdrline))
                                    (match-string 2 hdrline))))
                               hlines)))
    (remove nil header-alist)))

(defun http-capture-headers-and-body (start end)
  "Return a list of the form `(header body)` with the captured valued from START to END point."
  (let* ((sep-point (save-excursion (goto-char start) (re-search-forward "^\\s-*$" end t)))
         (headers (http-parse-headers start (or sep-point end)))
         (body (and sep-point (buffer-substring-no-properties sep-point end))))
    (list headers body)))

(defun http-end-parameters (&optional start)
  "Locate the end of request body from START point."
  (save-excursion
    (when start (goto-char start))
    (end-of-line)
    (or (and (re-search-forward (concat "^#\\|" http-keywords-regexp) nil t) (point-at-bol))
        (point-max))))

(cl-defun http-callback (&key data response error-thrown &allow-other-keys)
  (with-current-buffer (get-buffer-create http-buffer-response-name)
    (erase-buffer)
    (when error-thrown
      (message "Error: %s" error-thrown))
    (let* ((ctype-header (request-response-header response "content-type"))
           (ctype-list (and ctype-header (rfc2231-parse-string ctype-header)))
           (guessed-mode (cdr (assoc (car ctype-list) http-content-type-alist)))
           (charset (cdr (assq 'charset (cdr ctype-list))))
           (coding-system (and charset (intern (downcase charset)))))
      (cond ((eq guessed-mode 'image-mode)
             (fundamental-mode)
             ;;; TODO: Somehow the curl backend of `request.el' mess this up.
             (insert-image (create-image data (image-type-from-data data) t)))
            (t
             (when (stringp data)
               (insert data))
             (when coding-system
               (set-buffer-file-coding-system coding-system))
             (cond ((eq guessed-mode 'js-mode)
                    (when (and (fboundp 'json-pretty-print-buffer) (/= 0 (buffer-size)))
                      (json-pretty-print-buffer))
                    (js-mode))
                   ((fboundp guessed-mode)
                    (funcall guessed-mode))))))
    (when http-show-response-headers
      (let ((hstart (point))
            (raw-header (request-response--raw-header response))
            (comment-start (or comment-start http-response-comment-start)))
        (unless (string= "" raw-header)
          (insert "\n" raw-header)
          (comment-region hstart (point)))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun http-process ()
  "Process a http request."
  (interactive)
  (save-excursion
    (let* ((start (save-excursion (end-of-line) (re-search-backward (concat http-keywords-regexp "\\(.*\\)$"))))
           (type (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
           (url (s-trim (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
           (urlobj (url-generic-parse-url url))
           (end (http-end-parameters start))
           (headers)
           (data))
      (cl-multiple-value-setq (headers data) (http-capture-headers-and-body start end))
      (let* ((path-and-query (url-path-and-query urlobj))
             (path (car path-and-query))
             (query (cdr path-and-query))
             (params (and query (http-query-alist query))))
        ;; XXX: remove the querystring to make it compatible with request.el
        (setf (url-filename urlobj) path)
        (request (url-recreate-url urlobj)
                 :type type
                 :params params
                 :data data
                 :headers headers
                 :parser 'buffer-string
                 :success 'http-callback
                 :error 'http-callback)))))

(defvar http-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for http mode files.")

(defvar http-font-lock-keywords
  (regexp-opt (append http-methods-list)))

(defun http-font-lock-keywords ()
  `((,http-keywords-regexp . font-lock-keyword-face)
    (,http-header-regexp 1 font-lock-builtin-face)
    (,http-header-regexp 2 font-lock-constant-face)))

;;;###autoload
(define-derived-mode http-mode prog-mode "HTTP Client"
  "Major mode for HTTP client."
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (local-set-key (kbd "C-c C-c") 'http-process)
  (setq font-lock-defaults '((http-font-lock-keywords)))
  (set-syntax-table http-mode-syntax-table))

(provide 'http)

;;; http.el ends here
