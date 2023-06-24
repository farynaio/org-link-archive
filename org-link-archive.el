;;; org-link-archive.el -- This packages provides functionality to replace org-mode link at point with archivised link produced by archive.org. The replaced link has to be in (org-mode link format)[https://orgmode.org/manual/Link-Format.html]. -*- lexical-binding: t -*-

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

(defvar org-link-archive-link-insert-method 'append
  "Possible options include 'append or 'replace. If set to 'append, then place the archive link on the line after the original link, otherwise replace it.")

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
    (org-link-archive-process link)
    (user-error "URL has to be provided!")))

(defun org-link-archive-process (link &optional func)
  "Request archived URL from archive.org, print in in Messages buffer, save it on the top of the kill ring and call `func' to update buffer content. By default the `org-link-archive-replace' is called which replace occourance at the point with new URL."
  (let ((func (or func (pcase org-link-archive-link-insert-method (('replace  #'org-link-archive-replace)
																	('append  #'org-link-archive-append))))))
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

(defun org-link-archive-append (link-old link-new)
  " Replace single occourance of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (let ((case-fold-search nil))
    (beginning-of-line)
    (when (search-forward link-old nil t)
	  (move-end-of-line nil)
	  (insert "
")
	  (message link-new)
      (org-insert-link "https:" link-new))))

(defun org-link-archive-replace-all (link-old link-new)
  "Replace all occourances of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (search-forward link-old nil t)
      (replace-match link-new))))

(defun org-link-archive-append-all (link-old link-new)
  "Replace all occourances of URL `link-old' with new one `link-new'. If the replacement was succesful return `t', otherwise return `nil'."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (search-forward link-old nil t)
	  	  (move-end-of-line nil)
	  (insert "
")
	  (org-insert-link "https:" link-new))))


(provide 'org-link-archive)
