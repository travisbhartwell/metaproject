Metaproject for Emacs
=====================

Intended for simple project support in Emacs to allow for opening,
closing, and finding related files together and having common buffers
for a project -- like a magit status buffer, and eshell buffer, etc.

Notes and plans:

- Open project
  - open all files in a project, including project supporting buffers
  - open just project supporting buffers, including:
    - magit status buffer
    - dired at top level dir
    - supporting org-mode file, if defined
    - open shell buffer (ansi-term or eshell) at top-level dir
  - run any project initialization commands, i.e.:
    - open rope project at appropriate dir (top-level?)
    - start SLIME

- Close project
  - Close all associated files and buffers

- Open file in project
  - Methods:
    - ido
    - dired or dired-like
    - speedbar
  - Use defined list of files in project
  - Use all files in project directories
  - Use all files in project directories sans those ignored by git
    (see git-dired.el)

- Open project among known projects (like magit-status-repos)

- Switch to open project buffer (ido)

- Jump to or open project buffer:
  - magit status
  - dired (top-level, parent, current)
  - org-mode file
  - shell buffer
  - speedbar or dired for all project files

- Add a keyboard sub-map for the buffers in the project that adds:
  - open file in project
  - close project
  - jump to or open project buffer
  - switch to open project buffer
