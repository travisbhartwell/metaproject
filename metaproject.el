;;; metaproject.el --- Project support and utilities for Emacs

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
(require 'cl)

;; Metaproject Constants
(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration files for each project.")

(defconst metaproject-version "0.02-SNAPSHOT"
  "This is the current version of metaproject.")

;; Metaproject Customization items
(defgroup metaproject nil
  "Project support and utilities.")

(defcustom metaproject-project-dirs nil
  "This is a list of the directories that contain project directories."
  :type '(repeat directory)
  :group 'metaproject)

;; Variables
(defvar metaproject-keymap-prefix "\C-cp"
  "The prefix for all minor mode bindings.")

(defvar metaproject-keymap (make-sparse-keymap)
  "Keymap for the metaproject minor mode.")

(defvar metaproject-minor-mode nil
  "Minor mode for metaproject.")

;; General utilities
(defun metaproject-is-metaproject (dir)
  (file-exists-p (expand-file-name metaproject-config-file-name dir)))

(defun metaproject-get-top-dir (dir)
  (let ((top-dir (locate-dominating-file dir metaproject-config-file-name)))
       (when (not (null top-dir))
	 (expand-file-name top-dir))))

(defun metaproject-find-metaprojects (dir)
  (project-util-find #'metaproject-is-metaproject dir))

;; Buffer handling
(defun metaproject-setup-buffer (project-config buffer)
  (save-excursion
    (let ((project-buffers (metaproject-config-get project-config 'project-buffers)))
      (set-buffer buffer)
      (metaproject-minor-mode t)
      (make-variable-buffer-local 'project-config-buffer)
      (setq project-config-buffer
	    (metaproject-config-get project-config 'project-config-buffer))
      (add-to-list 'project-buffers (current-buffer))
      (metaproject-config-put project-config 'project-buffers project-buffers)
      (add-hook 'kill-buffer-hook 'metaproject-remove-buffer-from-open-buffers t t))))

(defun metaproject-project-member-p ()
  (and (boundp 'project-config-buffer)
       (not (null project-config-buffer))
       (not (equal (current-buffer) project-config-buffer))))

(defun metaproject-remove-buffer-from-open-buffers ()
  (let ((project-config
	 (metaproject-get-project-config-from-buffer (current-buffer)))
	(project-buffers (metaproject-config-get project-config 'project-buffers)))
    (setq project-config
	  (metaproject-config-put
	   project-config
	   'project-buffers
	   (delq (current-buffer) project-buffers)))))

;; Action registry
(defvar metaproject-action-registry '()
  "This is an alist that maps from config action names to functions that process their configuration.")

(defun metaproject-register-action (action action-function)
  (let ((action-sym (if (not (symbolp action)) (make-symbol action) action)))
    (setq metaproject-action-registry
	  (acons action-sym action-function metaproject-action-registry))))

(defun metaproject-call-action (project-config action)
  (let ((args (metaproject-config-get project-config action))
	(action-function (cdr (assoc-string action metaproject-action-registry))))
    (when (fboundp action-function)
      (funcall action-function project-config args))))

;; Project config handling
(defun metaproject-config-get (project-config variable)
  (plist-get project-config variable))

(defun metaproject-config-put (project-config variable value)
  (plist-put project-config variable value))

(defun metaproject-read-from-file (file)
  (save-excursion
    (find-file file)
    (add-hook 'kill-buffer-query-functions 'metaproject-close-project t t)
    (metaproject-minor-mode t)
    (beginning-of-buffer)
    (make-variable-buffer-local 'project-config)
    (setq project-config (read (current-buffer)))
    (metaproject-config-put
     project-config 'project-base-dir (file-name-directory file))
    (metaproject-config-put
     project-config 'project-config-buffer (current-buffer))
    (metaproject-config-put
     project-config 'project-buffers nil)))

(defun metaproject-get-project-config-from-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (set-buffer project-config-buffer)
    project-config))

;; Project opening and closing
(defun metaproject-open-project (project-dir)
  (let ((project-file-name
         (expand-file-name metaproject-config-file-name project-dir)))
    (if (file-exists-p project-file-name)
        (let* ((project-config (metaproject-read-from-file project-file-name))
	       (project-config-keys
		(loop for (k v) on project-config by #'cddr collect k)))
	  (mapc (lambda (action)
		  (metaproject-call-action project-config action))
		project-config-keys))
      (message "No project found in directory %s" project-dir))))

(defun metaproject-close-project-from-anywhere ()
  (interactive)
  (let* ((project-config (metaproject-get-project-config-from-buffer (current-buffer)))
	 (config-buffer (metaproject-config-get project-config 'project-config-buffer)))
    (when (not (null config-buffer))
	(kill-buffer config-buffer))))

(defun metaproject-close-project (&optional config-buffer)
  (let ((config-buffer (if (and (null config-buffer)
				  (boundp 'project-config))
			   (current-buffer)
			 config-buffer)))
    (if (not (null config-buffer))
	(progn
	  (set-buffer config-buffer)
	  (let* ((project-base-dir (metaproject-config-get project-config 'project-base-dir))
		 (project-buffers (metaproject-config-get project-config 'project-buffers))
		 (close-project-p
		  (y-or-n-p
		   (format
		    "Closing this file will close the project.  Close project %s? "
		    project-base-dir))))
	    (if close-project-p
		(progn
		  (if (y-or-n-p "Close all project files and buffers? ")
		      (progn
			(save-some-buffers nil #'metaproject-project-member-p)
			(mapc 'kill-buffer project-buffers))
			
		    (mapc 'metaproject-teardown-buffer project-buffers))
		  t)
	      nil)))
      t)))

(defun metaproject-teardown-buffer (buffer)
  (set-buffer buffer)
  (makunbound 'project-config-buffer)
  (remove-hook 'kill-buffer-hook 'metaproject-remove-buffer-from-open-buffers t))

;; File opening and closing
(defun metaproject-open-project-files (project-config files)
    (mapc (lambda (file) (metaproject-open-and-setup-file project-config file)) files))

(metaproject-register-action "files" 'metaproject-open-project-files)

(defun metaproject-open-and-setup-file (project-config file)
  (let ((project-base-dir (metaproject-config-get project-config 'project-base-dir)))
    (find-file (expand-file-name file project-base-dir))
    (metaproject-setup-buffer project-config (current-buffer))))

(defun metaproject-add-binding-to-keymap (key function)
  (define-key metaproject-keymap (concat metaproject-keymap-prefix key) function))

;; metaproject minor mode, to apply to all buffers belonging to a project
(define-minor-mode metaproject-minor-mode
  "A minor mode for providing a common keymap for access to project-level
functionality."
  nil
  " Metaproject"
  metaproject-keymap
  :group 'metaproject
  (setq metaproject-minor-mode
	(if (null arg) (not metaproject-minor-mode)
	  (> (prefix-numeric-value arg) 0))))

;; Bindings for core functionality
(metaproject-add-binding-to-keymap "c" 'metaproject-close-project-from-anywhere)

(provide 'metaproject)
;;; metaproject.el ends here
