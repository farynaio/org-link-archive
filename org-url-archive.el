;;; org-url-archive.el --- tests for ivy -*- lexical-binding: t -*-

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

;; This packages provides functionality to replace all occourances of org-mode URL at point with archivised URL produced by archive.org.
;; The replaced URL has to be in org-mode format - "[[url][optional-description]]".

;;; Code:

(defun farynaio/org-archive-url (url &optional args)
  "Replace org URL with archived version produced by archive.org."
  (interactive (browse-url-interactive-arg "URL: "))
  (if url
    (progn
      (let* ((url-automatic-caching t)
              (url-inhibit-uncompression t)
              (cur-buf (current-buffer))
              (case-fold-search nil)
              (url-request-method "GET"))
        (with-temp-buffer
          (url-retrieve
            (concat "https://web.archive.org/save/" url)
            (lambda (status)
              (let* ((redirect (plist-get status ':redirect))
                     (old-url-org (concat "[" url "]"))
                     (new-url-org (concat "[" redirect "]")))
                (if redirect
                  (progn
                    (message redirect)
                    (kill-new redirect)
                    (save-excursion
                      (with-current-buffer cur-buf
                        (goto-char (point-min))
                        (while (search-forward old-url-org nil t)
                          (replace-match new-url-org)))))
                  (user-error "Something went wrong!"))))))))
    (user-error "URL has to be provided!")))

(provide 'org-url-archive)