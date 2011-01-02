;;; -*- emacs-lisp -*-
;;; gnus-desktop-notify.el: Gnus Desktop Notification global minor mode

;; Author: Yuri D'Elia <wavexx AT users.sf.net>
;; Contributors: Philipp Haselwarter <philipp.haselwarter AT gmx.de>
;; Version: 1.0 (2010/12/26)
;; URL: http://www.thregr.org/~wavexx/hacks/gnus-desktop-notify/
;; GIT: git://src.thregr.org/gnus-desktop-notify/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Desktop notification integration in Gnus!? Ohh goody!
;;
;; gnus-desktop-notify.el provides a simple mechanism to notify external
;; programs when new messages are received. For basic usage, to be used in
;; conjunction with gnus-daemon, put the following:
;;
;; (require 'gnus-desktop-notify)
;; (gnus-desktop-notify-mode)
;; (gnus-demon-add-scanmail)
;;
;; into your .gnus file. You'll need the 'notify-send' program, which (in
;; Debian) is available in the 'libnotify-bin' package. Each time a group
;; receives new mail, the 'notify-send' program is called, creating a small
;; popup message containing the name of the group and the number of new
;; messages.
;;
;; By default, all groups are notified when new messages are received. You can
;; exclude a single group by setting the `group-notify' group parameter to
;; t. You can also selectively monitor groups instead by changing the
;; `gnus-desktop-notify-groups' variable to `gnus-desktop-notify-explicit' and
;; then manually selecting which groups to include. Press 'G c' in the group
;; buffer to customize group parameters interactively.
;;
;; You can actually call any program by changing the
;; `gnus-desktop-notify-exec-program' variable, or change the behavior entirely
;; by setting a different `gnus-desktop-notify-function' function, or change
;; the way the popup is generated with `gnus-desktop-notify-send-mode'.
;;
;; See the `gnus-desktop-notify' customization group for more details.
;;
;; Feel free to send suggestions and patches to wavexx AT users.sf.net

;;; Code:
(require 'assoc)
(require 'gnus-group)

(defgroup gnus-desktop-notify nil
  "Gnus external notification framework"
  :group 'gnus)

(define-minor-mode gnus-desktop-notify-mode
  "Gnus Desktop Notification mode uses libnotify's 'notify-send'
program to generate popup messages or call external executables
whenever a group receives new messages through gnus-demon (see
`gnus-demon-add-handler').

  You can actually call any program by changing the
`gnus-desktop-notify-exec-program' variable, or change the
behavior entirely by setting a different
`gnus-desktop-notify-function' function.

  See the `gnus-desktop-notify' customization group for more
details."
  :init-value nil
  :group 'gnus-desktop-notify
  :require 'gnus
  :global t
  (cond
    (gnus-desktop-notify-mode
      (add-hook 'gnus-after-getting-new-news-hook 'gnus-desktop-notify-check)
      (add-hook 'gnus-started-hook 'gnus-desktop-notify-check))
    (t
      (remove-hook 'gnus-after-getting-new-news-hook 'gnus-desktop-notify-check)
      (remove-hook 'gnus-started-hook 'gnus-desktop-notify-check))))


;; Custom variables
(defcustom gnus-desktop-notify-function 'gnus-desktop-notify-send
  "Function called when a group has new messages. The first
argument will be an alist containing the groups and the number of
new messages.  The default is to use `gnus-desktop-notify-send'.

  The following functions are already available (see the
documentation for each function):

`gnus-desktop-notify-send': use the 'notify-send' program.
`gnus-desktop-notify-exec': call a customizable program."
  :type 'function)

(defcustom gnus-desktop-notify-exec-program "xmessage"
  "Executable called by the `gnus-desktop-notify-exec'
function. Each argument will be of the form:

  'number of new messages:mailbox name'"
  :type 'file)

(defcustom gnus-desktop-notify-send-program
  "notify-send -i /usr/share/icons/gnome/32x32/actions/mail_new.png"
  "Path and default arguments to the 'notify-send' program (part
of libnotify's utilities)."
  :type 'file)

(defcustom gnus-desktop-notify-send-mode 'gnus-desktop-notify-multi
  "`gnus-desktop-notify-send' behavior. Can be either:

'gnus-desktop-notify-single: display a single notification for
                             each group,
'gnus-desktop-notify-multi: display a multi-line notification for
                            all groups at once."
  :type 'symbol)

(defcustom gnus-desktop-notify-groups 'gnus-desktop-notify-all-except
  "Gnus group notification mode. Can be either:

'gnus-desktop-notify-all-except: monitor all groups by
                                 default except excluded ones,
'gnus-desktop-notify-explicit: monitor only requested groups.

  Groups can be included or excluded by setting the
'group-notify' group parameter to 't'.  This can be set either in
the `gnus-parameters' variable, or interactively by pressing 'G
c' in the group buffer."
  :type 'symbol)


;; Group parameters
(gnus-define-group-parameter
  group-notify
   :type bool
   :parameter-type '(const :tag "Include/exclude this group from
the notification of new messages (depending on the value of
`gnus-desktop-notify-groups')." t))

;; Functions
(defun gnus-desktop-notify-exec (groups)
  "Call a program defined by `gnus-desktop-notify-exec-program' with
each argument being of the form 'number of new messages:mailbox name'."
  (let ( (args "") )
    (dolist (g groups)
      (setq args
	(concat args " "
	  (shell-quote-argument (format "%d:%s" (cdr g) (car g))))))
    (call-process-shell-command
      gnus-desktop-notify-exec-program nil 0 nil args)))

(defun gnus-desktop-notify-escape-html-entities (str)
  (setq str (replace-regexp-in-string "&" "&amp;" str))
  (setq str (replace-regexp-in-string "<" "&lt;" str))
  (setq str (replace-regexp-in-string ">" "&gt;" str))
  str)

(defun gnus-desktop-notify-send (groups)
  "Call 'notify-send' (as defined by `gnus-desktop-notify-send-program'),
with the behavior defined by `gnus-desktop-notify-send-mode'."
  (case gnus-desktop-notify-send-mode
    ('gnus-desktop-notify-multi
      (let ( (text "New mail:") )
	(dolist (g groups)
	  (setq text
	    (concat text
	      (format "<br/><tt>  <b>%d</b>:<b>%s</b></tt>"
		(cdr g) (gnus-desktop-notify-escape-html-entities (car g))))))
	(call-process-shell-command gnus-desktop-notify-send-program nil 0 nil "--"
	  (shell-quote-argument text))))
    ('gnus-desktop-notify-single
      (dolist (g groups)
	(call-process-shell-command gnus-desktop-notify-send-program nil 0 nil "--"
	  (shell-quote-argument
	    (format "New mail: <tt><b>%d</b>:<b>%s</b></tt>"
	      (cdr g) (gnus-desktop-notify-escape-html-entities (car g)))))))))


;; Internals
(setq gnus-desktop-notify-counts '())

(defun gnus-desktop-notify-check (&rest ignored)
  (interactive)
  (let ( (updated-groups '()) )
    (dolist (g gnus-newsrc-alist)
      (let* ( (name (gnus-info-group g))
              (read (or
                     (and (listp (car (gnus-info-read g)))
                          (cdar (gnus-info-read g)))
                     (cdr (gnus-info-read g))))
              (unread (gnus-group-unread name)) )
	(when (and (numberp read) (numberp unread))
	  (let* ( (count (+ read unread))
		  (old-count (cdr (assoc name gnus-desktop-notify-counts)))
		  (notify (gnus-group-find-parameter name 'group-notify)) )
	    (when (or
		    (and (eq gnus-desktop-notify-groups 'gnus-desktop-notify-all-except) (not notify))
		    (and (eq gnus-desktop-notify-groups 'gnus-desktop-notify-explicit) notify))
	      (aput 'gnus-desktop-notify-counts name count)
	      (when (and
		      unread (> unread 0)
		      old-count (> count old-count))
		(setq updated-groups
		  (cons (cons name (- count old-count))
		    updated-groups))))))))
    (when (and updated-groups (not (called-interactively-p 'any)))
      (funcall gnus-desktop-notify-function updated-groups))))


(provide 'gnus-desktop-notify)
