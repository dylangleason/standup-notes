;;; standup-notes.el --- Interactive standup-note generator  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Dylan Gleason

;; Author: Dylan Gleason <dgleason8384@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interactively describe standup notes for the previous and current
;; workdays and print to the help buffer.

;;; Code:

(defvar standup-template-alist '((previous . ("Non-development work" "Tickets"))
                                 (current  . ("Non-development work" "Tickets" "PRs to review" "PRs needed"))
                                 (next     . ("Tickets"))
                                 (blockers . ("Do you have any blockers?"))
                                 (notes    . ("Do you need to meet with anyone?")))
  "The template to be used when generating standup-notes.
  The standup template contains template tasks for the keys
  PREVIOUS, CURRENT and NEXT. Override by binding this to a new
  value")

(defun standup-notes ()
  "Generate standup-notes interactively. The user will be
prompted to enter various information for the previous and
current work days, and a nicely formatted report will be
generated in a help buffer."
  (interactive)

  (letrec
      ((day-of-week
        (lambda ()
          (format-time-string "%u")))

       (beginning-of-weekp
        (lambda ()
          (member (funcall day-of-week) '("0" "1" "7"))))

       (end-of-weekp
        (lambda ()
          (member (funcall day-of-week) '("5" "6" "7"))))

       (prev-workday
        (lambda ()
          (if (funcall beginning-of-weekp)
              "Friday"
            "yesterday")))

       (next-workday
        (lambda ()
          (if (funcall end-of-weekp)
              "Monday"
            "tomorrow")))

       (prompt-notes
        (lambda (notes)
          (let ((input (read-string (concat notes " (Defaults to \"no\"): ")
                                    nil nil "no")))
            (if (equal input "no")
                ""
              input))))

       (prompt-items
        (lambda (items)
          (read-string
           (concat items " (Defaults to \"none\"): ") nil nil "none")))

       (prompt-previous
        (lambda (subject)
          (concat subject ": "
                  (funcall prompt-items
                           (concat subject
                                   " for "
                                   (funcall prev-workday))))))
       (prompt-current
        (lambda (subject)
          (concat subject ": "
                  (funcall prompt-items (concat subject " for today")))))

       (prompt-next
        (lambda (subject)
          (concat subject ": "
                  (funcall prompt-items
                           (concat subject
                                   " for "
                                   (funcall next-workday))))))

       (join-newline
        (lambda (lst)
          (string-trim (mapconcat 'identity lst "\n"))))

       (prompt-slackify
        (lambda (str)
          (let ((input (funcall prompt-notes "Create slack friendly output?")))
            (if (equal input "")
                str
                (concat "```\n" str "\n```")))))

       (make-prompt
        (lambda (type item)
          (case type
            ('previous (funcall prompt-previous item))
            ('current  (funcall prompt-current item))
            ('next     (funcall prompt-next item))
            ('blockers (funcall prompt-notes item))
            (t         (funcall prompt-notes item)))))

       (make-header
        (lambda (header)
          (case header
            ('previous (funcall prev-workday))
            ('current  "today")
            ('next     (funcall next-workday))
            ('blockers "blockers")
            (t         "notes"))))

       (make-item
        (lambda (header tasks)
          (let ((str (funcall join-newline
                              (cl-loop for task in tasks
                                       collect
                                       (funcall make-prompt header task)))))
            (if (equal "" str)
                str
              (concat "\n## " (upcase (funcall make-header header)) "\n" str)))))

       (make-from-template
        (lambda ()
          (cl-loop for (header . tasks) in standup-template-alist
                   collect (funcall make-item header tasks)))))

    (with-help-window "*standup-notes*"
      (princ
       (funcall prompt-slackify
                (funcall join-newline (funcall make-from-template)))))))

(provide 'standup-notes)
;;; standup-notes.el ends here
