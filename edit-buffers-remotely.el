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

(defun edit-buffers-remotely-server--/ (request)
  (ws-send-file
   (oref request process)
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
        (body (funcall
               (if (fboundp 'json-serialize)
                   'json-serialize
                 #'json-encode-array)
               (seq-into
                (mapcar #'buffer-name (edit-buffers-remotely-buffer-list))
                'vector))))
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
                 (let (err)
                   (if (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (if buffer-read-only
                             (setq err
                                   (format "Can't edit %s which is read-only" buffer))
                           (erase-buffer)
                           (insert string)))
                     (setq err (format "Buffer %s does not exist" bufname)))
                   (when err
                     (process-send-string proc (ws-web-socket-frame err))))))
              (prog1 :keep-alive
                (process-send-string
                 proc
                 (ws-web-socket-frame
                  (with-current-buffer buffer
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))
            (ws-send-file
             proc
             (expand-file-name "edit.html" edit-buffers-remotely--load-dir)
             "text/html; charset=utf-8"))
        (edit-buffers-remotely-server--404 request)))))

(provide 'edit-buffers-remotely)
;;; edit-buffers-remotely.el ends here
