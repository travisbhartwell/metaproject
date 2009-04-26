;;; metaproject.el --- Project support and utilities for Emacs

;; Copyright (C) 2009 Travis B. Hartwell (nafai AT travishartwell DOT net)

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

(require 'project-utils)

(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration
files for each project")

; Probably should be a customization
(defvar metaproject-project-dirs nil
  "This is a list of the directories that contain project directories")

(defun metaproject-read-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((project-config (read (current-buffer))))
      (plist-put project-config 'project-base-dir (file-name-directory file)))))

(defun metaproject-open-project-files (project-config)
  (let ((files (plist-get project-config 'files))
        (project-base-dir (plist-get project-config 'project-base-dir)))
    (mapc
     (lambda (f)
       (find-file (expand-file-name f project-base-dir)))
     files)))

(defun metaproject-open-project (project-dir)
  (let ((project-file-name
         (expand-file-name metaproject-config-file-name project-dir)))
    (if (file-exists-p project-file-name)
        (let ((project-config (metaproject-read-from-file project-file-name)))
	  (metaproject-open-project-files project-config))
      (message "No project found in directory %s" project-dir))))


(defun metaproject-is-metaproject (dir)
  (file-exists-p (expand-file-name metaproject-config-file-name dir)))

(defun metaproject-get-top-dir (dir)
  (let ((top-dir (locate-dominating-file dir metaproject-config-file-name)))
       (when (not (null top-dir))
	      (expand-file-name top-dir))))

(defun metaproject-find-metaprojects (dir)
  (project-util-find #'metaproject-is-metaproject dir))

(defun metaproject-open-ido (arg)
  (interactive "P")
  (project-util-action-ido
   #'metaproject-find-metaprojects
   metaproject-project-dirs
   #'metaproject-get-top-dir
   #'metaproject-open-project
   "Metaproject Project to open? "
   arg))

(provide 'metaproject)
