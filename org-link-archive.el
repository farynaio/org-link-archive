;; -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026  Free Software Foundation, Inc.

;; Author: Adam Faryna

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages provides functionality to replace org-mode link with archived URL produced by archive.org.
;; The replaced URL has to be in org-mode link format - `https://orgmode.org/manual/Link-Format.html'.

;;; Code:

(defgroup org-link-archive nil
  "Replace links with archived ones by using `https://archive.org'."
  :group 'matching
  :prefix "org-link-archive-")

(defconst org-link-archive-archiveorg-link-prefix "https://web.archive.org/save/"
  "URL prefix used to request URL to archived version of the web page.")

(defun org-link-archive-at-point (url)
  "Replace org URL at a point with archived version.
The `URL' should info node `org:Link Format'."
  (interactive (browse-url-interactive-arg "URL: "))
  (if url
    (org-link-archive-process url)
    (user-error "URL has to be provided!")))

(defun org-link-archive-process (url &optional func)
  "This function does 3 things:
1. request archived `URL' from archive.org
2. save it in on the top of the kill ring
3. update buffer content
If `FUNC' is not provided the `org-link-archive-replace' will be used."
  (let ((func (or func #'org-link-archive-replace)))
    (if url
      (progn
        (let ((url-automatic-caching t)
               (url-inhibit-uncompression t)
               (url-request-method "GET")
               (link-marker (point-marker)))
          (url-retrieve
            (concat org-link-archive-archiveorg-link-prefix url)
            (lambda (status)
              (let ((redirect (plist-get status ':redirect)))
                (if redirect
                  (progn
                    (message redirect)
                    (kill-new redirect)
                    (set-buffer (marker-buffer link-marker))
                    (let ((cur-point (point))
                           (cur-marker (point-marker)))
                      (set-marker-insertion-type cur-marker nil)
                      (goto-char (marker-position link-marker))
                      (funcall func link redirect)
                      (goto-char (marker-position cur-marker))
                      (set-marker cur-marker nil)))
                  (user-error "Something went wrong!")))
              (set-marker link-marker nil)))))
      (user-error "URL has to be provided!"))))

(defun org-link-archive-replace (url-old url-new)
  "Replace single occourance of URL `URL-OLD' with new one `URL-NEW'.
If the replacement was succesful return t, otherwise return nil."
  (let ((case-fold-search nil))
    (beginning-of-line)
    (when (search-forward url-old nil t)
      (replace-match url-new))))

(defun org-link-archive-replace-all (url-old url-new)
  "Replace all occourances of URL `URL-OLD' with new one `URL-NEW'.
If the replacement was succesful return t, otherwise return nil."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (search-forward url-old nil t)
      (replace-match url-new))))

(provide 'org-link-archive-at-point)
;;; org-link-archive.el ends here