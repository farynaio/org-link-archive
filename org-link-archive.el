;; org-link-archive.el -*- lexical-binding: t -*-

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

;; This packages provides functionality to replace org-mode link with archivised URL produced by archive.org.
;; The replaced URL has to be in org-mode link format - `https://orgmode.org/manual/Link-Format.html'.

;;; Code:

(defgroup org-archive-link nil
  "Replace links with archivised ones by using `https://archive.org'."
  :group 'matching
  :prefix "org-archive-link-")

(defconst org-archive-link-archiveorg-link-prefix "https://web.archive.org/save/"
  "URL prefix used to request URL to archived version of the web page.")

(defun org-archive-link (link &optional rest)
  "Replace org link at a point with archived version produced by archive.org. The `link' has to be in org-mode link format (`https://orgmode.org/manual/Link-Format.html'). `rest' parameter is unused."
  (interactive (browse-url-interactive-arg "URL: "))
  (if link
    (org-archive-link-process link)
    (user-error "URL has to be provided!")))

(defun org-archive-link-all (link &optional rest)
  "Replace every occourance of org link at a point with archived version produced by archive.org. The `link' has to be in org-mode link format (`https://orgmode.org/manual/Link-Format.html'). `rest' parameter is unused."
  (interactive (browse-url-interactive-arg "URL: "))
  (if link
    (org-archive-link-process link #'org-archive-link-replace-all)
    (user-error "URL has to be provided!")))

(defun org-archive-link-process (link &optional func)
  "Request archived URL from archive.org, print in in Messages buffer, save it on the top of the kill ring and call `func' to update buffer content. By default the `org-archive-link-replace' is called which replace occourance at the point with new URL."
  (let ((func (or func #'org-archive-link-replace)))
    (if link
      (progn
        (let* ((url-automatic-caching t)
                (url-inhibit-uncompression t)
                (cur-buf (current-buffer))
                (case-fold-search nil)
                (cur-point (point))
                (url-request-method "GET"))
          (with-temp-buffer
            (url-retrieve
              (concat org-archive-link-archiveorg-link-prefix link)
              (lambda (status)
                (let* (
                        (redirect (plist-get status ':redirect))
                        (new-point (point)))
                  (if redirect
                    (progn
                      (message redirect)
                      (kill-new redirect)
                      (save-excursion
                        (set-buffer cur-buf)
                        (with-current-buffer cur-buf
                          (goto-char cur-point)
                          (funcall func link redirect))
                      ))
                    (user-error "Something went wrong!"))))))))
      (user-error "URL has to be provided!"))))

(defun org-archive-link-replace (link-old link-new)
" Replace single occourance of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (beginning-of-line)
  (when (search-forward link-old nil t)
    (replace-match link-new)))

(defun org-archive-link-replace-all (link-old link-new)
"Replace all occourances of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (goto-char (point-min))
  (while (search-forward link-old nil t)
    (replace-match link-new)))

(provide 'org-link-archive)