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

(defgroup org-link-archive nil
  "Replace links with archivised ones by using `https://archive.org'."
  :group 'matching
  :prefix "org-link-archive-")

(defconst org-link-archive-archiveorg-link-prefix "https://web.archive.org/save/"
  "URL prefix used to request URL to archived version of the web page.")

(defun org-link-archive (link &optional rest)
  "Replace org link at a point with archived version produced by archive.org. The `link' has to be in org-mode link format (`https://orgmode.org/manual/Link-Format.html'). `rest' parameter is unused."
  (interactive (browse-url-interactive-arg "URL: "))
  (if link
    (org-link-archive-process link)
    (user-error "URL has to be provided!")))

(defun org-link-archive-all (link &optional rest)
  "Replace every occourance of org link at a point with archived version produced by archive.org. The `link' has to be in org-mode link format (`https://orgmode.org/manual/Link-Format.html'). `rest' parameter is unused."
  (interactive (browse-url-interactive-arg "URL: "))
  (if link
    (org-link-archive-process link #'org-link-archive-replace-all)
    (user-error "URL has to be provided!")))

(defun org-link-archive-process (link &optional func)
  "Request archived URL from archive.org, print in in Messages buffer, save it on the top of the kill ring and call `func' to update buffer content. By default the `org-link-archive-replace' is called which replace occourance at the point with new URL."
  (let ((func (or func #'org-link-archive-replace)))
    (if link
      (progn
        (let ((url-automatic-caching t)
               (url-inhibit-uncompression t)
               (url-request-method "GET")
               (link-marker (point-marker)))
          (url-retrieve
            (concat org-link-archive-archiveorg-link-prefix link)
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

(defun org-link-archive-replace (link-old link-new)
  " Replace single occourance of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (let ((case-fold-search nil))
    (beginning-of-line)
    (when (search-forward link-old nil t)
      (replace-match link-new))))

(defun org-link-archive-replace-all (link-old link-new)
  "Replace all occourances of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (search-forward link-old nil t)
      (replace-match link-new))))

(provide 'org-link-archive)
