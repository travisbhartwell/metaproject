(defconst metaproject-config-file-name ".metaproject"
  "This is the default filename used for the metaproject configuration
files for each project")

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

(provide 'metaproject)

