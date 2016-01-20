;;; http.el --- Yet another HTTP client              -*- lexical-binding: t; -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/http.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (request "0.2.0"))

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
;; execute `M-x http-process` or press <kdb>C-c C-c</kbd>, if everything is went
;; well should show an buffer when the response of the http request:
;;
;; ![http.el screenshot](misc/screenshot.png)
;;
;; More examples are included in file [misc/example.txt](misc/example.txt)

;; Customization:
;;
;; Fontify response:
;;
;; If you want to use a custom mode for the fontification of the response buffer
;; with content-type equal to `http-content-type-mode-alist'.  For example, to
;; use [json-mode][] for responses with content-type "application/json":
;;
;;     (add-to-list 'http-content-type-mode-alist
;;                  '("application/json" . json-mode))
;;
;; Prettify response:
;;
;; If you want to use a custom function to prettify the response body you need
;; to add it to `http-pretty-callback-alist', the function is called without
;; arguments.  For example, to use [json-reformat][] for responses with
;; content-type "application/json":
;;
;;     (require 'json-reformat)
;;
;;     (defun my/pretty-json-buffer ()
;;       (json-reformat-region (point-min) (point-max)))
;;
;;     (add-to-list 'http-pretty-callback-alist
;;                  '("application/json" . my/pretty-json-buffer))

;; Related projects:
;;
;; + [httprepl.el][]: An HTTP REPL for Emacs.
;;
;; + [restclient.el][]: HTTP REST client tool for Emacs.  You can use both
;;   projects indistinctly, the main differences between both are:
;;
;;              | `restclient.el'   | `http.el'
;;   ---------- | ----------------- | -------------
;;   backend    | `url.el'          | `request.el'
;;   variables  | yes               | no
;;
;; [httprepl.el]: https://github.com/gregsexton/httprepl.el "An HTTP REPL for Emacs"
;; [restclient.el]: https://github.com/pashky/restclient.el "HTTP REST client tool for Emacs"
;; [json-mode]: https://github.com/joshwnj/json-mode "Major mode for editing JSON files with Emacs"
;; [json-reformat]: https://github.com/gongo/json-reformat "Reformat tool for JSON"

;; TODO:
;;
;; + [ ] Add code block support for org-mode

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'outline))

(require 'request)
(require 'rfc2231)
(require 'url-util)

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

(defcustom http-prettify-response t
  "If non nil inserts response headers at the top."
  :type 'boolean
  :safe 'booleanp
  :group 'http)

(defcustom http-fallback-comment-start "//"
  "Fallback string used as `comment-start'.

Used only when was not possible to guess a response content-type."
  :type 'string
  :group 'http)

;;;###autoload
(defvar-local http-hostname nil
  "Default hostname used when url is an endpoint.")
;;;###autoload
(put 'http-hostname 'safe-local-variable #'stringp)

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
    ("application/x-javascript" . js-mode)
    ("application/json" . js-mode)
    ("text/javascript" . js-mode)
    ("text/html" . html-mode)
    ("text/plain" . text-mode)
    ("image/gif" . image-mode)
    ("image/png" . image-mode)
    ("image/jpeg" . image-mode)
    ("image/x-icon" . image-mode))
  "Mapping between 'content-type' and a Emacs mode.

Used to fontify the response buffer and comment the response headers.")

(declare-function json-pretty-print-buffer "json")

;; XXX: Emacs<25 doesn't escapes the slash character https://github.com/emacs-mirror/emacs/commit/58c8605
(defvar http-pretty-callback-alist
  '(("application/json" . json-pretty-print-buffer))
  "Mapping between 'content-type' and a pretty callback.")

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
         (body (and rest (not (string-blank-p rest)) (string-trim rest))))
    (list headers body)))

(defun http-end-parameters (&optional start)
  "Locate the end of request body from START point."
  (save-excursion
    (and start (goto-char start))
    (end-of-line)
    (or (and (re-search-forward (concat "^#\\|" http-request-line-regexp) nil t) (1- (point-at-bol)))
        (point-max))))

(cl-defun http-callback (&key data response error-thrown &allow-other-keys)
  (with-current-buffer (get-buffer-create http-buffer-response-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and error-thrown (message (error-message-string error-thrown)))
      (let* ((ctype-header (request-response-header response "content-type"))
             (ctype-list (and ctype-header (rfc2231-parse-string ctype-header)))
             (charset (cdr (assq 'charset (cdr ctype-list))))
             (coding-system (and charset (intern (downcase charset))))
             (ctype-name (car ctype-list))
             (guessed-mode (assoc-default ctype-name http-content-type-mode-alist))
             (pretty-callback (assoc-default ctype-name http-pretty-callback-alist)))
        (if (eq guessed-mode 'image-mode)
            (let* ((data-p t)
                   (data (string-make-unibyte data))
                   (type (if (fboundp 'imagemagick-types)
                             'imagemagick
                           (image-type data nil data-p)))
                   (image (create-image data type data-p)))
              (insert-image image))
          (when (stringp data)
            (let ((text (if http-prettify-response (http-prettify-text data pretty-callback) data)))
              (insert (http-fontify-text text guessed-mode))))
          (and coding-system (set-buffer-file-coding-system coding-system))))
      (when http-show-response-headers
        (goto-char (if http-show-response-headers-top (point-min) (point-max)))
        (let ((hstart (point))
              (raw-header (request-response--raw-header response)))
          (unless (string-empty-p raw-header)
            (or http-show-response-headers-top (insert "\n"))
            (insert raw-header)
            (let ((comment-start (or comment-start http-fallback-comment-start)))
              (comment-region hstart (point)))
            (put-text-property hstart (point) 'face 'font-lock-comment-face))))
      (http-response-mode))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun http-prettify-text (text object)
  "Prettify using TEXT using calling OBJECT in a temporal buffer."
  (if (not (functionp object))
      text
    (with-temp-buffer
      (erase-buffer)
      (insert text)
      (funcall object)
      (buffer-string))))

;; Stolen from `ansible-doc'.
(defun http-fontify-text (text mode)
  "Add `font-lock-face' properties to TEXT using MODE.

Return a fontified copy of TEXT."
  ;; Graciously inspired by http://emacs.stackexchange.com/a/5408/227
  (if (not (fboundp mode))
      text
    (with-temp-buffer
      (erase-buffer)
      (insert text)
      ;; Run mode without any hooks
      (delay-mode-hooks
        (funcall mode)
        (font-lock-mode))
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          ;; Suppress warning about non-interactive use of
          ;; `font-lock-fontify-buffer' in Emacs 25.
          (font-lock-fontify-buffer)))
      ;; Convert `face' to `font-lock-face' to play nicely with font lock
      (goto-char (point-min))
      (while (not (eobp))
        (let ((pos (point)))
          (goto-char (next-single-property-change pos 'face nil (point-max)))
          (put-text-property pos (point) 'font-lock-face
                             (get-text-property pos 'face))))
      (buffer-string))))

(defun http-capture ()
  "Capture a http request.

Return a list of the form: \(URL TYPE PARAMS DATA HEADERS\)"
  (interactive)
  (let* ((start (save-excursion (end-of-line) (re-search-backward http-request-line-regexp)))
         (type (match-string-no-properties 1))
         (endpoint (match-string-no-properties 2))
         (url (if (and http-hostname (not (string-match-p url-nonrelative-link endpoint)))
                  (url-expand-file-name endpoint http-hostname)
                endpoint))
         (urlobj (url-generic-parse-url url))
         (end (http-end-parameters start)))
    (cl-destructuring-bind (path . query)
        (url-path-and-query urlobj)
      (let ((params (and query (http-query-alist query))))
        ;; XXX: remove the query string to make it compatible with `request.el'
        (setf (url-filename urlobj) path)
        (cl-multiple-value-bind (headers data) (http-capture-headers-and-body start end)
          (list (url-recreate-url urlobj) type params data headers))))))

;;;###autoload
(defun http-process ()
  "Process a http request."
  (interactive)
  (cl-multiple-value-bind (url type params data headers)
      (http-capture)
    (request url
             :type type
             :params params
             :data data
             :headers headers
             :parser 'buffer-string
             :success 'http-callback
             :error 'http-callback)))

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
(define-derived-mode http-mode text-mode "HTTP Client"
  "Major mode for HTTP client.

\\{http-mode-map}"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(http-font-lock-keywords))
  (setq outline-regexp http-mode-outline-regexp)
  (setq outline-heading-alist http-mode-outline-regexp-alist)
  (setq imenu-generic-expression http-mode-imenu-generic-expression)
  (add-to-invisibility-spec '(outline . t))
  (imenu-add-to-menubar "Contents"))

;;;###autoload
(define-derived-mode http-response-mode special-mode "HTTP Response"
  "Major mode for HTTP responses from `http-mode'

\\{http-response-mode-map}"
  (setq buffer-read-only t
        buffer-auto-save-file-name nil))

(provide 'http)

;;; http.el ends here
