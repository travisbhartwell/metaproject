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

;;; Code:
; The following suggested by (info "(elisp)Coding Conventions")
(eval-when-compile
  (require 'cl))

;;;; Constants
(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration files for each project.")

(add-to-list 'auto-mode-alist '("\\.metaproject$" . emacs-lisp-mode))

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
  "Remove the project specified by PROJECT from the current projects group.
This does not close the project or any of its buffers, this may be
done elsewhere."
  (let ((top-dir (metaproject-project-state-get project 'top-dir)))
    (remhash top-dir metaproject-current-projects)))

(defun metaproject-current-projects-add-project (project)
  "Add the project specified by PROJECT to the current projects group.
It is assumed that the project itself is opened and any related operations
are done elsewhere."
  (let ((top-dir (metaproject-project-state-get project 'top-dir)))
    (assert (not (null top-dir)))
    (puthash top-dir project metaproject-current-projects)))

;;;; General Project Functions
(defconst metaproject-project-empty-template '((state . nil) (config . nil))
  "Template for empty in-memory project structures.
'state' will contain the current state of the project: open buffers, etc.
'config' will contain the configuration to be persisted across project
loads.")

(defun metaproject-project-p (dir)
  "Return t if the directory DIR is the top level of a Metaproject project."
  (file-exists-p (expand-file-name metaproject-config-file-name dir)))

(defun metaproject-project-create (top-dir &optional name)
  "Create a project with the root TOP-DIR and optionally, named NAME.
If TOP-DIR is not an absolute path name, it is assumed to be relative
to the current buffer's `default-directory', which could be
unexpected, so it is preferred to call this with an absolute path.
If NAME is not specified, the directory name of TOP-DIR (not the full
path) is used.  If the directory TOP-DIR does not exist, or is not a
directory, an error is signaled."
  (let ((top-dir (expand-file-name top-dir)))
    (if (and (file-exists-p top-dir)
	     (file-directory-p top-dir))
	(let* ((name (if (null name)
			 (file-name-directory top-dir)
		       name)))
	  (setq new-project (copy-alist metaproject-project-empty-template)
		new-project (metaproject-project-config-put new-project 'name name)
		new-project (metaproject-project-state-put new-project 'top-dir top-dir))
	  (metaproject-current-projects-add-project new-project))
      (error metaproject-error-directory-not-found top-dir))))

;;;; Project state and configuration handling
(defun metaproject-project-data-get (project sym variable)
  "Return from PROJECT in SYM's list the value of VARIABLE.
Note that this does not differentiate between a variable having a null value
and the variable not existing.  Use `metaproject-project-data-member' if
concerned about null values."
  (let ((data (cdr (assoc sym project))))
    (plist-get data variable)))

(defun metaproject-project-data-member (project sym variable)
  "Return from PROJECT in SYM's list the cons of VARIABLE and its value or nil if not found."
  (let ((data (cdr (assoc sym project))))
    (plist-member data variable)))

(defun metaproject-project-data-put (project sym variable value)
  "On PROJECT in SYM's list, set VARIABLE to VALUE.
If VARIABLE exists, overwrite the existing value.  Returns the updated
project."
  (let* ((data-assoc (assoc sym project))
	 (data (cdr data-assoc)))
    (setcdr data-assoc (plist-put data variable value))
    project))

(defun metaproject-project-accessor-name-gen (sym suffix)
  "Generate the accessor name for symbol SYM with suffix SUFFIX."
   (intern (concat "metaproject-project-" (symbol-name sym) suffix)))

(defmacro metaproject-project-data-accessors (sym)
  "Generate the accessor functions for SYM in a project.
This includes `metaproject-project-SYM-get', `metaproject-project-SYM-member',
and `metaproject-project-*-put'."
  (let ((name (symbol-name sym))
	(get-fn-name (metaproject-project-accessor-name-gen sym "-get"))
	(member-fn-name (metaproject-project-accessor-name-gen sym "-member"))
	(put-fn-name (metaproject-project-accessor-name-gen sym "-put")))
    `(progn
       (defun ,get-fn-name (project variable)
	 ,(concat "Return from the PROJECT " name " the value of VARIABLE.
Note that this does not differentiate between a variable having a null value
and the variable not existing.  Use `metaproject-project-" name "-member' if
concerned about null values.")
	 (metaproject-project-data-get project ',sym variable))
       (defun ,member-fn-name (project variable)
	 ,(concat "Return from the PROJECT " name " the cons of VARIABLE and its value or nil if not found.")
	   (metaproject-project-data-get project ',sym variable))
       (defun ,put-fn-name (project variable value)
	 ,(concat "On PROJECT, set VARIABLE to VALUE in the " name ".
If VARIABLE exists, overwrite the existing value.  Returns the updated
project.")
	 (metaproject-project-data-put project ',sym variable value)))))

(metaproject-project-data-accessors config)
(metaproject-project-data-accessors state)

(defun metaproject-project-config-store (project)
  "Store the configuration for PROJECT to disk."
  (let* ((config (cdr (assoc 'config project)))
	 (top-dir (metaproject-project-state-get project 'top-dir))
	 (config-file-name (expand-file-name metaproject-config-file-name top-dir))
	 (config-buffer (find-file config-file-name)))
    (save-excursion
      (set-buffer config-buffer)
      (erase-buffer)
      (print config (current-buffer))
      (save-buffer)
      (kill-buffer))))

(defun metaproject-project-config-load (project-file-name)
  "Load the configuration for a project from PROJECT-FILE-NAME."
  (let ((top-dir (or (file-name-directory project-file-name) default-directory))
	(new-project (copy-alist metaproject-project-empty-template)))
    (if (metaproject-project-p top-dir)
	(save-excursion
	  (find-file project-file-name)
	  (beginning-of-buffer)
	  (setq project-config (read (current-buffer)))
	  (setcdr (assoc 'config new-project) project-config)
	  (metaproject-project-state-put new-project 'top-dir top-dir)
	  (metaproject-current-projects-add-project new-project)
	  (kill-buffer)
	  new-project))))
    
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

;; TODO-MAYBE: Rename metaproject-project-get-open-files to something more apropos when implementing.
(defun metaproject-files-get-from-project (project)
  "Return the list of files belonging to PROJECT.
These files are not necessarily currently open.  Use
`metaproject-project-get-open-files' to get a list of the project files
that are currently open."
  (let ((files-config (metaproject-project-config-get project 'files)))
	 (plist-get files-config 'files)))

(defun metaproject-files-put-to-project (project files)
  "Set on PROJECT the list of FILES."
  (let ((files-config (metaproject-project-config-get project 'files)))
    (setq files-config (plist-put files-config 'files files))
    (metaproject-project-config-put project 'files files-config)))
     
(defun metaproject-files-valid-file-in-project-p (file project)
  "Return t if FILE exists, is a regular file, and is under the PROJECT's directory."
  (let* ((top-dir (metaproject-project-state-get project 'top-dir))
	 (default-directory top-dir)
	 (expanded-file-name (file-relative-name (expand-file-name file top-dir) top-dir)))
    (and
     (not (null top-dir))
     ;; The result of `file-relative-name' will start with "../" when the
     ;; file is not under TOP-DIR.  Not sure if this is portable to Windows.
     (not (string= "../" (substring expanded-file-name 0 3)))
     (file-exists-p expanded-file-name)
     (file-regular-p expanded-file-name))))
  
(defun metaproject-project-add-file (project file)
  "Add to the project PROJECT the file FILE.
Only add FILE if it isn't already a member of PROJECT.  FILE is
assumed to be an absolute pathname or relative to PROJECT's top-dir.
If FILE is not a valid file (i.e., not a valid file or under the
project's directory), an error is signaled."
  (if (metaproject-files-valid-file-in-project-p file project)
      (let* ((top-dir (metaproject-project-state-get project 'top-dir))
	     (expanded-file-name (expand-file-name file top-dir))
	     (relative-file-name (file-relative-name expanded-file-name top-dir))
	     (project-files (metaproject-files-get-from-project project)))
	(when (not (member relative-file-name project-files))
	  (metaproject-files-put-to-project
	   project
	   (append project-files (list relative-file-name)))))
    (error metaproject-error-not-valid-file file)))
  
(provide 'metaproject)
;;; metaproject.el ends here
