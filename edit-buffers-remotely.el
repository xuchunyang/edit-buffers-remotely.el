;;; edit-buffers-remotely.el --- An HTTP server to edit buffers in browser -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/edit-buffers-remotely.el
;; Package-Requires: ((emacs "25.1") (web-server "20200330.1407"))
;; Keywords: tools
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; edit-buffers-remotely.el provides an HTTP server for editing buffers from a
;; web browser.
;;
;; To start the server, type M-x edit-buffers-remotely-server-start

;;; Code:

(require 'json)
(require 'web-server)
(require 'seq)
(eval-when-compile (require 'subr-x))   ; `when-let'

(defgroup edit-buffers-remotely nil
  "Edit Emacs buffers from a web browser."
  :group 'external)

(defcustom edit-buffers-remotely-server-host "0.0.0.0"
  "The host address of the server."
  :type 'string)

(defcustom edit-buffers-remotely-server-port 8080
  "The port of the server."
  :type 'integer)

(defcustom edit-buffers-remotely-server-log-buffer nil
  "The buffer name which used by the server for logging."
  :type '(choice
          (const :tag "Don't log" nil)
          (string :tag "Buffer name")))

(defvar edit-buffers-remotely-server nil
  "The ws-server instance.")

(defconst edit-buffers-remotely--load-dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "The directory of the package.")

(defun edit-buffers-remotely-buffer-list ()
  "Return a list of buffers."
  (seq-filter
   (lambda (buffer)
     (not (string-prefix-p " " (buffer-name buffer))))
   (buffer-list)))

(defun edit-buffers-remotely-server-start ()
  "Start the server, if there is server running, stop it firstly."
  (interactive)
  (when edit-buffers-remotely-server
    (message "[edit-buffers-remotely] Stopping the server")
    (ws-stop edit-buffers-remotely-server))
  (setq edit-buffers-remotely-server
        (ws-start
         (list '((:GET . "^/api/buffer-list/?$") .
                 edit-buffers-remotely-server--/api/buffer-list)
               '((:GET . "^/$") .
                 edit-buffers-remotely-server--/)
               '((:GET . "^/edit/") .
                 edit-buffers-remotely-server--/edit/)
               `(,(lambda (_request) t) .
                 edit-buffers-remotely-server--404))
         edit-buffers-remotely-server-port
         edit-buffers-remotely-server-log-buffer
         :host edit-buffers-remotely-server-host))
  (message "[edit-buffers-remotely] Listening at %s"
           (propertize
            (format "http://%s:%s/"
                    edit-buffers-remotely-server-host
                    edit-buffers-remotely-server-port)
            'face 'link)))

(defun edit-buffers-remotely-server-stop ()
  "Stop the server."
  (interactive)
  (if edit-buffers-remotely-server
      (progn (ws-stop edit-buffers-remotely-server)
             (setq edit-buffers-remotely-server nil))
    (user-error "No server is running")))

;; `httpd-etag'
(defun edit-buffers-remotely-server--etag (file)
  "Compute the ETag for FILE."
  (concat "\"" (substring (sha1 (prin1-to-string (file-attributes file))) -16)
          "\""))

(defun edit-buffers-remotely-server--send-file (request path type)
  (with-slots (headers process) request
    (let ((request-etag (alist-get :IF-NONE-MATCH headers))
          (etag (edit-buffers-remotely-server--etag path)))
      (if (equal request-etag etag)
          (ws-response-header process 304)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path)
          (ws-response-header
           process 200
           (cons "Content-Type" type)
           (cons "Content-Length" (buffer-size))
           (cons "Etag" etag))
          (process-send-region process (point-min) (point-max)))))))

(defun edit-buffers-remotely-server--/ (request)
  (edit-buffers-remotely-server--send-file
   request
   (expand-file-name "index.html" edit-buffers-remotely--load-dir)
   "text/html; charset=utf-8"))

(defun edit-buffers-remotely-server--404 (request)
  (let ((proc (oref request process))
        (body "<h1>404 Not Found</h1>"))
    (ws-response-header
     proc 404
     '("Content-Type" . "text/html; charset=utf-8")
     `("Content-Length" . ,(string-bytes body)))
    (process-send-string proc body)))

(defun edit-buffers-remotely-server--/api/buffer-list (request)
  (let ((proc (oref request process))
        (body (json-encode
               (mapcar (lambda (buffer)
                         (with-current-buffer buffer
                           `((name . ,(buffer-name))
                             (mode . ,(format-mode-line mode-name nil nil
                                                        (current-buffer)))
                             (size . ,(buffer-size))
                             (file . ,(buffer-file-name))
                             (proc . ,(when-let ((proc (get-buffer-process buffer)))
                                        (process-name proc))))))
                       (edit-buffers-remotely-buffer-list)))))
    (ws-response-header
     proc 200
     '("Content-Type" . "application/json; charset=utf-8")
     `("Content-Length" . ,(string-bytes body)))
    (process-send-string proc body)))

(defun edit-buffers-remotely-server--/edit/ (request)
  (with-slots ((proc process) headers) request
    (let* ((bufname (url-unhex-string
                     (substring (alist-get :GET headers) (length "/edit/"))))
           (buffer (get-buffer bufname)))
      (if buffer
          (if (ws-web-socket-connect
               request
               (lambda (proc string)
                 (condition-case err
                     (with-current-buffer buffer
                       (erase-buffer)
                       (insert string))
                   (error
                    (process-send-string
                     proc
                     (ws-web-socket-frame (error-message-string err)))))))
              (prog1 :keep-alive
                (process-send-string
                 proc
                 (ws-web-socket-frame
                  (with-current-buffer buffer
                    (encode-coding-string
                     (buffer-substring-no-properties
                      (point-min) (point-max))
                     'utf-8)))))
            (edit-buffers-remotely-server--send-file
             request
             (expand-file-name "edit.html" edit-buffers-remotely--load-dir)
             "text/html; charset=utf-8"))
        (edit-buffers-remotely-server--404 request)))))

(provide 'edit-buffers-remotely)
;;; edit-buffers-remotely.el ends here
