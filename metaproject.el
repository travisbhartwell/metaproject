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

;; TODO: I'm not comfortable with my plist usage, I feel like it needs to be abstracted away,
;; But it is getting messy, again it is probably time to look into macros and that should clean
;; this up a ton.
;;; Code:
; The following suggested by (info "(elisp)Coding Conventions")
(eval-when-compile
  (require 'cl))

;;;; Constants
(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration files for each project.")

(defconst metaproject-version "0.02-SNAPSHOT"
  "This is the current version of metaproject.")

;; Error messages
(defconst metaproject-error-directory-not-found
  "The specified directory '%s' does not exist or is not a directory")

(defconst metaproject-error-not-valid-file
  "The file '%s' is not a valid file or doesn't exist under project directory")

;;;; Customization items
(defgroup metaproject nil
  "Project support and utilities.")

(defcustom metaproject-project-dirs nil
  "This is a list of the directories that contain project directories."
  :type '(repeat directory)
  :group 'metaproject)

;;;; Currently Open Projects
(defvar metaproject-current-projects (make-hash-table :test 'equal)
  "This is the group of all of the projects that are currently open.
It is a hash table where the keys are the top level directories of
each project and the values are property lists containing the configuration
information and state of each project.")

(defun metaproject-current-projects-get-project-by-path (path)
  "Get the currently open project that has the top level directory PATH.
Returns nil if a project from that path is not currently open."
  (gethash path metaproject-current-projects nil))

(defun metaproject-current-projects-remove-project (project)
  "Remove the project specified by PROJECT from the current project group.
This does not close the project or any of its buffers, this may be
done elsewhere."
  (remhash project metaproject-current-projects))

;;;; General Project Functions
(defun metaproject-project-p (dir)
  "Return t if the directory DIR is the top level of a Metaproject project."
  (file-exists-p (expand-file-name metaproject-config-file-name dir)))

;; QUESTION: Should this function add the created project to the open project group,
;; or is this something done by a higher-level function?
;; TODO-MAYBE: Check if this directory is in an existing project and bail.
(defun metaproject-project-create (top-dir &optional name)
  "Create a project with the root TOP-DIR and optionally, named NAME.
If NAME is not specified, the name of TOP-DIR (not the full path) is used.  If
the directory TOP-DIR does not exist, or is not a directory,
an error is signaled."
  (let ((top-dir (expand-file-name top-dir)))
    (if (and (file-exists-p top-dir)
	     (file-directory-p top-dir))
	(let* ((name (if (null name)
			 (file-name-directory top-dir)
		       name))
	       ;; TODO: Fix this so it abstracts what the underlying
	       ;; data structure is.
	       (new-project (list 'top-dir top-dir 'name name)))
	  new-project)
      (error metaproject-error-directory-not-found top-dir))))

;;;; Project state and configuration handling
;; QUESTION: Should the configuration and state be stored separately?
;; This would make it easier to persist just the config and differentiate
;; at runtime.
;; TODO-MAYBE: Write macros for retrieving specific a) module configurations,
;; b) variable values, and so on.  I have a lot of boiler-plate code that is
;; currently necessary, and this is a great opportunity to use macros.  These
;; would use metadata-project-data-get, etc underneath.
(defun metaproject-project-data-get (project variable)
  "Return from PROJECT the value of VARIABLE.
Note that this does not differentiate between a variable having a null value
and the variable not existing.  Use `metaproject-project-data-member' if
concerned about null values."
  (plist-get project variable))

(defun metaproject-project-data-member (project variable)
  "Return from PROJECT the cons of VARIABLE and its value or nil if not found."
  (plist-member project variable))

(defun metaproject-project-data-put (project variable value)
  "On PROJECT, set VARIABLE to VALUE.
If VARIABLE exists, overwrite the existing value."
  (plist-put project variable value))

;;;; Metaproject Files module definition.

;; The Files module specifies the files in a project.
;; Its configuration is a plist with the following properties:
;; - files - list of file paths, all relative to the project top-dir
;; - find-all-at-open - find all files at project open if t
;; - TODO: Define other keys as needed, possible are include-regexp and exclude-regexp
;;
;; It is one of the default modules that is always loaded and core Metaproject
;; depends upon, but for simplicity, in many cases, such as project load and store
;; time, it is treated like any other module.  It just happens to live in the same file.

;; TODO-MAYBE: Rename metaproject-project-get-open-files to something more apropos.
(defun metaproject-files-get-from-project (project)
  "Return the list of files belonging to PROJECT.
These files are not necessarily currently open.  Use
`metaproject-project-get-open-files' to get a list of the project files
that are currently open."
  (let ((files-config (metaproject-project-data-get project 'files)))
	 (plist-get files-config 'files)))

(defun metaproject-files-put-to-project (project files)
  "Set on PROJECT the list of FILES."
  (let ((files-config (metaproject-project-data-get project 'files)))
    (setq files-config (plist-put files-config 'files files))
    (metaproject-project-data-put project 'files files-config)))
     
(defun metaproject-file-valid-in-project-p (file project)
  "Return t if FILE exists, is a regular file, and is under the PROJECT's directory."
  (let ((expanded-file-name (expand-file-name file))
	(top-dir (metaproject-project-data-get project 'top-dir)))
    (and
     (not (null top-dir))
     (file-exists-p expanded-file-name)
     (file-regular-p expanded-file-name)
      ;; The result of `file-relative-name' will start with "../" when the
      ;; file is not under TOP-DIR
     (not
      (string= "../"
	       (substring
		(file-relative-name expanded-file-name top-dir) 0 3))))))
  
(defun metaproject-project-add-file (project file)
  "Add to the project PROJECT the file FILE.
Only add FILE if it isn't already a member of PROJECT.  If FILE is not
a valid file (i.e., not a valid file or under the project's
directory, an error is signaled."
  (if (metaproject-file-valid-in-project-p file project)
      (let* ((expanded-file-name (expand-file-name file))
	     (top-dir (metaproject-project-data-get project 'top-dir))
	     (relative-file-name (file-relative-name expanded-file-name top-dir))
	     (project-files (metaproject-files-get-from-project project)))
	(when (not (member relative-file-name project-files))
	  (metaproject-files-put-to-project
	   project
	   (append project-files (list relative-file-name)))))
    (error metaproject-error-not-valid-file file)))
  
(provide 'metaproject)
;;; metaproject.el ends here
