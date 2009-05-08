Metaproject for Emacs
=====================

Intended for simple project support in Emacs to allow for opening,
closing, and finding related files together and having common buffers
for a project -- like a magit status buffer, and eshell buffer, etc.

It started when I had a medium-sized Python project I was working on.
Whenever I started Emacs, I would want to open all of the Python files
in the project so I could quickly navigate between them.  It got old
to reopen a few dozen files in a few directories manually.

NOTE:

This is very limited in functionality right now, I'm just getting it
out so people can try it out and give feedback.  I have some plans
(see notes.org) for the future that essentially require a re-write of
some of the core pieces and make it easier for others to hook in and
make non-essential things optional.

I assume you have a basic understanding of Emacs and configuring it; I
hope to be more newbie friendly in a later version.

To use:

 - Add the directory where you put these files to the emacs load path.

 - (require 'metaproject)

 - For what will be optional functionality in the future (as it
   requires ido), but required to open projects now, you need to
   customize or set the metaproject-project-dirs variable.

   M-x customize-variable metaproject-project-dirs

 - Create a .metaproject file for your project.  It is an emacs lisp
   file containing a plist, with only property supported right now --
   the files property.  Its value is a list of relative pathnames for
   the files that belong to the project.  The pathnames are relative to
   the top-level directory, the directory in which the .metaproject
   file is in.

   See the .metaproject file included with the source for an example.
   I hand created it by doing a find command, piping it to a file, and
   then using some keyboard macros to translate it into what I wanted.

 - To open a project, run:

   M-x metaproject-open-ido

   select the directory that is the project that you wish to open.
   This works like any ido prompt for those familiar with it.  I'll
   provide an interactive means of opening up projects without ido in
   the next version.

   This opens up all of the files associated with the project, or
   associates them with the project if they are already open in a
   buffer.

 - To close a project, switch to the corresponding .metaproject buffer
   and kill it.  It will prompt you if you wish to close the project
   and then if you wish to close all of the associated files.  In the
   next version there will be other means to close a project.


That's it for now.  I'm open to comments on the code, as this is my
first serious elisp.  Some of my future design ideas are sketched out
briefly in notes.org, so feel free to check that out.
