;;; -*- emacs-lisp -*-
;;; gnus-notify.el: External mail notification for Gnus

;; Author: Yuri D'Elia <wavexx AT users.sf.net>
;; Contributors: Philipp Haselwarter <philipp.haselwarter AT gmx.de>
;; URL: http://www.thregr.org/~wavexx/hacks/gnus-notify.el

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

;; gnus-notify.el provides a simple mechanism to notify external programs when
;; new messages are received. For basic usage, to be used in conjunction with
;; gnus-daemon, put the following:
;;
;; (require 'gnus-notify)
;; (gnus-demon-add-scanmail)
;;
;; into your .gnus file. You'll need the 'notify-send' program, which (in
;; Debian) is available in the 'libnotify-bin' package. Each time a group
;; receives new mail, the 'notify-send' program is called, creating a small
;; popup message containing the name of the group and the number of new
;; messages.
;;
;; By default, all groups are notified when new messages are received. You can
;; exclude a single group by setting the 'group-notify' group parameter to
;; t. You can also selectively monitor groups instead by changing the
;; 'gnus-notify-mode' variable to 'gnus-notify-explicit and then manually
;; selecting which groups to include. Press 'G c' in the group buffer to
;; customize group parameters interactively.
;;
;; You can actually call any program by changing the 'gnus-notify-exec-program'
;; variable, or change the behavior entirely by setting a different
;; 'gnus-notify-function' function.
;;
;; See the 'gnus-notify' customization group for more details.
;;
;; Feel free to send suggestions and patches to wavexx AT users.sf.net

;;; Code:
(require 'assoc)

;; Custom variables
(defgroup gnus-notify nil
  "Gnus external notification framework"
  :group 'gnus)

(defcustom gnus-notify-function 'gnus-notify-exec
  "Function called when a group has new messages. The first
argument will be an alist containing the groups and the number of
new messages.  The default is to use the 'gnus-notify-exec'
function to call an executable."
  :type 'function
  :group 'gnus-notify)

(defcustom gnus-notify-exec-program "notify-send -i /usr/share/icons/gnome-colors-common/16x16/apps/email.png"
  "Executable called by the 'gnus-notify-exec' function. The
first argument will be the notification text. The default is to
use the libnotify's 'notify-send' program."
  :type 'file
  :group 'gnus-notify)

(defcustom gnus-notify-mode 'gnus-notify-all-except
  "Gnus group notification mode. Can be either
'gnus-notify-all-except (to monitor all groups by default except
excluded ones), or 'gnus-notify-explicit (to monitor only
requested groups).

  Groups can be included or excluded by setting the
'group-notify' group parameter to 't'.  This can be set either in
the 'gnus-parameters' variable, or interactively by pressing 'G c'
in the group buffer."
  :type 'symbol
  :group 'gnus-notify)


;; Group parameters
(gnus-define-group-parameter
  group-notify
   :type bool
   :parameter-type '(const :tag "Include/exclude this group from
the notification of new messages (depending on the value of
'gnus-notify-mode')." t))

;; Functions
(defun gnus-notify-exec (groups)
  "Call a program defined by gnus-notify-exec-program with the
first argument being the notification text. The default is to use
the libnotify's 'notify-send' program."
  (dolist (g groups)
    (call-process-shell-command gnus-notify-exec-program nil 0 nil
      "--" (shell-quote-argument (format "New mail in %s:%d" (car g) (cdr g))))))

;; Internals
(setq gnus-notify-counts '())

(defun gnus-notify-check (&rest ignored)
  (interactive)
  (let ( (updated-groups '()) )
    (dolist (g gnus-newsrc-alist)
      (let ( (read (or
                    (and (listp (car (gnus-info-read g)))
                         (cdar (gnus-info-read g)))
                    (cdr (gnus-info-read g)))) )
	(when read
	  (let* ( (name (gnus-info-group g))
		  (unread (gnus-group-unread (car g)))
		  (count (+ read unread))
		  (old-count (cdr (assoc name gnus-notify-counts)))
		  (notify (gnus-group-find-parameter name 'group-notify)) )
	    (when (or
		    (and (eq gnus-notify-mode 'gnus-notify-all-except) (not notify))
		    (and (eq gnus-notify-mode 'gnus-notify-explicit) notify))
	      (aput 'gnus-notify-counts name count)
	      (when (and
		      unread (> unread 0)
		      old-count (> count old-count))
		(setq updated-groups
		  (cons (cons name (- count old-count))
		    updated-groups))))))))
    (when updated-groups
      (funcall gnus-notify-function updated-groups))))


;; Hooks into gnus
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notify-check)
(add-hook 'gnus-started-hook 'gnus-notify-check)

(provide 'gnus-notify)
