;;; project-utils.el --- Project utilities for Emacs

;; Copyright (C) 2009 Travis B. Hartwell (nafai AT travishartwell DOT net)

;; Version: 0.02-SNAPSHOT

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; TODO: Summary goes here
;;; History:
;; Version: 0.01
;; Date: May 7, 2009

(require 'esh-util)

(defun project-util-find (desired-dir-p dir)
  (when (file-directory-p dir)
    (or (char-equal ?/ (aref dir (1- (length dir))))
	(setq dir (file-name-as-directory dir)))
    (if (funcall desired-dir-p dir) dir
      (let ((files (directory-files dir t nil t))
	    fullname file)
	(eshell-flatten-list (remove-if
			      'null
			      (mapcar
			       (lambda (file)
				 (unless
				     (or
				      (string= (substring file -3) "/..")
				      (string= (substring file -2) "/."))
				   (project-util-find desired-dir-p file)))
			       files)))))))


(defun project-util-action-ido (find-func top-level-dirs get-top-dir action-function prompt arg)
  (interactive "P")
  (let* ((project-dirs (eshell-flatten-list (mapcar find-func top-level-dirs)))
	 (buffer-dir (when (not (null buffer-file-name))
		       (expand-file-name (if (file-directory-p buffer-file-name)
					     buffer-file-name
					   (file-name-directory buffer-file-name)))))
	 (this-project-dir (if (and
				(not (null buffer-dir))
				(null arg))
			       (let ((project-dir-temp (funcall get-top-dir buffer-dir)))
				 (when (not (null project-dir-temp))
				   (file-name-as-directory project-dir-temp)))
			     nil)))
    (funcall action-function (ido-completing-read prompt project-dirs nil nil this-project-dir))))

(provide 'project-utils)

