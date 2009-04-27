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


;;; History:
;; 

(require 'cl)
(require 'project-utils)

;;; Code:
(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration
files for each project")

; Probably should be a customization
(defvar metaproject-project-dirs nil
  "This is a list of the directories that contain project directories.")

(defun metaproject-read-from-file (file)
  (save-excursion
    (find-file file)
    (add-hook 'kill-buffer-query-functions 'metaproject-close-project t t)
    (beginning-of-buffer)
    (make-variable-buffer-local 'project-config)
    (setq project-config (read (current-buffer)))
    (plist-put project-config 'project-base-dir (file-name-directory file))
    (plist-put project-config 'project-config-buffer (current-buffer))
    (plist-put project-config 'project-buffers nil)))

;;; Currently not comfortable with this because it relies on dynamic variables
;;; I know that is a built-in-functionality, but I wonder if this could be written cleaner
(defun metaproject-open-project-files (project-config)
  (let ((files (plist-get project-config 'files))
        (project-base-dir (plist-get project-config 'project-base-dir))
        (project-buffers (plist-get project-config 'project-buffers)))
    (mapc 'metaproject-open-and-setup-file files)
    (setq project-config (plist-put project-config 'project-buffers project-buffers))))

(defun metaproject-open-and-setup-file (file)
  (find-file (expand-file-name file project-base-dir))
  (make-variable-buffer-local 'project-config-buffer)
  (setq project-config-buffer (plist-get project-config 'project-config-buffer))
  (add-to-list 'project-buffers (current-buffer))
  (add-hook 'kill-buffer-hook 'metaproject-remove-buffer-from-open-buffers t t))

(defun metaproject-teardown-buffer (buffer)
  (set-buffer buffer)
  (makunbound 'project-config-buffer)
  (remove-hook 'kill-buffer-hook 'metaproject-remove-buffer-from-open-buffers t))

(defun metaproject-close-project (&optional config-buffer)
  (let ((config-buffer (when (and (null config-buffer)
				  (boundp 'project-config))
			   (current-buffer))))
    (if (not (null config-buffer))
	(progn
	  (set-buffer config-buffer)
	  (let* ((project-base-dir (plist-get project-config 'project-base-dir))
		 (project-buffers (plist-get project-config 'project-buffers))
		 (close-project-p (y-or-n-p (format "Closing this file will close the project.  Close project %s? " project-base-dir))))
	    (if close-project-p
		(progn
		  (if (y-or-n-p "Close all project files and buffers? ")
		      (mapc 'kill-buffer project-buffers)
		    (mapc 'metaproject-teardown-buffer project-buffers))
		  t)
	      nil)))
      t)))

(defun metaproject-get-project-config-from-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (set-buffer project-config-buffer)
    project-config))

(defun metaproject-remove-buffer-from-open-buffers ()
  (let ((project-config (metaproject-get-project-config-from-buffer (current-buffer)))
	(project-buffers (plist-get project-config 'project-buffers)))
      (setq project-config (plist-put project-config 'project-buffers (delq (current-buffer) project-buffers)))))
      	 
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

(provide 'metaproject)

;;; metaproject.el ends here
