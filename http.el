;;; http.el --- Yet another HTTP client              -*- lexical-binding: t; -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/http.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (request "0.2.0"))

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
;;
;; Usage:
;;
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
;;
;; ![http.el screenshot](misc/screenshot.png)
;;
;; More examples are included in file [misc/example.txt](misc/example.txt)
;;
;; Customisation:
;;
;; If you want to use a custom mode for rendering a response with
;; content-type equal to "application/json", say [json-mode][]:
;;
;;     (add-to-list 'http-content-type-mode-alist
;;                  '("application/json" . json-mode))
;;
;; If you want to use a custom prettify function for a response with
;; content equal to "application/json", say [json-reformat][]:
;;
;;     (require 'json-reformat)
;;     (defun my/pretty-json-buffer ()
;;       (json-reformat-region (point-min) (point-max)))
;;     (add-to-list 'http-pretty-callback-alist
;;                  '("application/json" . my/pretty-json-buffer))

;; Related projects:
;;
;; + [httprepl.el][]: An HTTP REPL for Emacs.
;;
;; + [restclient.el][]: HTTP REST client tool for Emacs. You can use
;;   both projects indistinctly, the main differences between both
;;   are:
;;
;;              | `restclient.el'   | `http.el'
;;   ---------- | ----------------- | -------------
;;   backend    | `url.el'          | `request.el'
;;   variables  | yes               | no
;;
;; [httprepl.el]: https://github.com/gregsexton/httprepl.el "An HTTP REPL for Emacs"
;; [restclient.el]: https://github.com/pashky/restclient.el "HTTP REST client tool for Emacs"
;; [json-reformat]: https://github.com/gongo/json-reformat "Reformat tool for JSON"
;; [json-mode]: https://github.com/joshwnj/json-mode "Major mode for editing JSON files with emacs"

;; TODO:
;;
;; + [ ] Add code block support for org-mode

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x nil 'noerror))

(require 'json)
(require 'outline)
(require 'request)
(require 'rfc2231)
(require 'url-util)

(eval-and-compile
  ;; `string-blank-p' from `subr-x' for Emacs 24.3 and bellow
  (unless (fboundp 'string-blank-p)
    (defsubst string-blank-p (string)
      "Check whether STRING is either empty or only whitespace."
      (string-match-p "\\`[ \t\n\r]*\\'" string))))

(defgroup http nil
  "Yet another HTTP client."
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

(defcustom http-show-response-headers-top nil
  "If non nil inserts response headers at the top."
  :type 'boolean
  :group 'http)

(defcustom http-fallback-comment-start "//"
  "Fallback string used as `comment-start'.

Used only when was not possible to guess a response content-type."
  :type 'string
  :group 'http)

(defvar-local http-hostname nil
  "Default hostname used when url is an endpoint.")
(put 'http-hostname 'safe-local-variable 'stringp)

(defvar http-methods-list
  '("GET" "POST" "DELETE" "PUT" "HEAD" "OPTIONS" "PATCH")
  "List of http methods.")

(defconst http-mode-outline-regexp
  (regexp-opt http-methods-list))

(defconst http-mode-outline-regexp-alist
  (mapcar (lambda (method) (cons method 1)) http-methods-list))

(defconst http-request-line-regexp
  (rx-to-string `(: line-start
                    (group (or symbol-start ,@http-methods-list symbol-end))
                    (+ space)
                    (group (* any) (not (in "\n" blank))))
                t))

(defconst http-mode-imenu-generic-expression
  (mapcar (lambda (method)
            (list method
                  (rx-to-string `(: line-start ,method (+ space) (group (* any) (not (in "\n" blank)))) t)
                  1))
          http-methods-list))

(defconst http-header-regexp
  (rx line-start (group (+ (in "_-" alnum))) ":" (* space) (group (+ any) (not blank))))

(defconst http-header-body-sep-regexp
  (rx line-start (* blank) line-end))

(defvar http-content-type-mode-alist
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
    ("image/x-icon" . image-mode))
  "Mapping between 'content-type' and a Emacs mode.

Used to fontify the response buffer and comment the response headers.")

(defvar http-pretty-callback-alist
  '(("application/json" . http-json-print-buffer))
  "Mapping between 'content-type' and a pretty callback.")

(defvar http-json-pretty-special-chars
  (remq (assq ?/ json-special-chars) json-special-chars)
  "Same as `json-special-chars' but without the ?/ character.

Used for pretty print a JSON reponse.")

(defun http-query-alist (query)
  "Return an alist of QUERY string."
  (cl-loop for (key val) in (url-parse-query-string query)
           collect (cons key val)))

(defun http-parse-headers (start end)
  "Return the parsed http headers from START to END point."
  (let* ((hdrlines (split-string (buffer-substring-no-properties start end) "\n"))
         (header-alist (mapcar (lambda (hdrline)
                                 (and (string-match http-header-regexp hdrline)
                                      (cons (downcase (match-string 1 hdrline))
                                            (match-string 2 hdrline))))
                               hdrlines)))
    (remove nil header-alist)))

(defun http-capture-headers-and-body (start end)
  "Return a list of the form `(header body)` with the captured valued from START to END point."
  (let* ((sep-point (save-excursion (goto-char start) (re-search-forward http-header-body-sep-regexp end t)))
         (headers (http-parse-headers start (or sep-point end)))
         (rest (and sep-point (buffer-substring-no-properties sep-point end)))
         (body (and rest (not (string-blank-p rest)) rest)))
    (list headers body)))

(defun http-end-parameters (&optional start)
  "Locate the end of request body from START point."
  (save-excursion
    (and start (goto-char start))
    (end-of-line)
    (or (and (re-search-forward (concat "^#\\|" http-request-line-regexp) nil t) (1- (point-at-bol)))
        (point-max))))

(defun http-json-pretty-encode-char (char)
  "Encode CHAR as a JSON string."
  (setq char (json-encode-char0 char 'ucs))
  (let ((control-char (car (rassoc char http-json-pretty-special-chars))))
    (if control-char
        (format "\\%c" control-char) ;; Special JSON character (\n, \r, etc.).
      (format "%c" char))))

;;;###autoload
(defun http-json-print-buffer ()
  "Pretty print json buffer."
  (interactive)
  (and (fboundp 'json-pretty-print-buffer)
       (cl-letf (((symbol-function 'json-encode-char) #'http-json-pretty-encode-char))
         (json-pretty-print-buffer))))

(cl-defun http-callback (&key data response error-thrown &allow-other-keys)
  (with-current-buffer (get-buffer-create http-buffer-response-name)
    (erase-buffer)
    (when error-thrown
      (message "Error: %s" error-thrown))
    (let* ((ctype-header (request-response-header response "content-type"))
           (ctype-list (and ctype-header (rfc2231-parse-string ctype-header)))
           (charset (cdr (assq 'charset (cdr ctype-list))))
           (coding-system (and charset (intern (downcase charset))))
           (ctype-name (car ctype-list))
           (guessed-mode (cdr (assoc ctype-name http-content-type-mode-alist)))
           (pretty-callback (cdr (assoc ctype-name http-pretty-callback-alist))))
      (cond ((eq guessed-mode 'image-mode)
             (fundamental-mode)
             ;; TODO: Somehow the curl backend of `request.el' mess this up.
             (insert-image (create-image data (image-type-from-data data) t)))
            (t
             (and (stringp data) (insert data))
             (and coding-system (set-buffer-file-coding-system coding-system))
             (and (functionp pretty-callback)
                  (not (zerop (buffer-size)))
                  (funcall pretty-callback))
             (and (fboundp guessed-mode) (funcall guessed-mode)))))
    (when http-show-response-headers
      (goto-char (if http-show-response-headers-top (point-min) (point-max)))
      (let ((hstart (point))
            (raw-header (request-response--raw-header response))
            (comment-start (or comment-start http-fallback-comment-start)))
        (unless (string= "" raw-header)
          (or http-show-response-headers-top (insert "\n"))
          (insert raw-header)
          (comment-region hstart (point)))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun http-process ()
  "Process a http request."
  (interactive)
  (let* ((start (save-excursion (end-of-line) (re-search-backward http-request-line-regexp)))
         (type (match-string-no-properties 1))
         (endpoint (match-string-no-properties 2))
         (url (if (and http-hostname (file-name-absolute-p endpoint)) (url-expand-file-name endpoint http-hostname) endpoint))
         (urlobj (url-generic-parse-url url))
         (end (http-end-parameters start))
         (path-and-query (url-path-and-query urlobj))
         (path (car path-and-query))
         (query (cdr path-and-query))
         (params (and query (http-query-alist query))))
    ;; XXX: remove the query string to make it compatible with `request.el'
    (setf (url-filename urlobj) path)
    (cl-multiple-value-bind (headers data) (http-capture-headers-and-body start end)
      (request (url-recreate-url urlobj)
               :type type
               :params params
               :data data
               :headers headers
               :parser 'buffer-string
               :success 'http-callback
               :error 'http-callback))))

(defvar http-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for http mode files.")

(defvar http-font-lock-keywords
  `((,http-request-line-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,http-header-regexp
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face))))

(defvar http-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'http-process)
    (define-key map (kbd "C-c C-n") 'outline-next-heading)
    (define-key map (kbd "C-c C-p") 'outline-previous-heading)
    (define-key map (kbd "C-c C-t") 'outline-toggle-children)
    map))

;;;###autoload
(define-derived-mode http-mode prog-mode "HTTP Client"
  "Major mode for HTTP client.

\\{http-mode-map}"
  :syntax-table http-mode-syntax-table
  (setq outline-regexp http-mode-outline-regexp
        outline-heading-alist http-mode-outline-regexp-alist
        imenu-generic-expression http-mode-imenu-generic-expression)
  (add-to-invisibility-spec '(outline . t))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(http-font-lock-keywords))
  (imenu-add-to-menubar "Contents"))

(provide 'http)

;;; http.el ends here
