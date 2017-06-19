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
                                 (meetings . ("Do you need to meet with anyone?")))
  "The template to be used when generating standup-notes.
  The standup template contains template tasks for the keys
  PREVIOUS, CURRENT and NEXT. Override by binding this to a new
  value")

(defun standup-notes ()
  "Generate standup-notes interactively. The user will be
prompted to enter various information for the previous and
current work days, and a nicely formatted report will be
generated in a help buffer. "
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
          (member (funcall day-of-week) '("1" "6" "7"))))

       (prev-workday-abbv
        (lambda ()
          (if (funcall beginning-of-weekp)
              "F"
            "Y")))

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

       (prompt-question
        (lambda (question)
          (let ((input (read-string (concat question " (Defaults to \"no\"): ")
                                    nil nil "no")))
            (if (equal input "no")
                ""
              input))))

       (prompt-todos
        (lambda (todos)
          (read-string
           (concat todos " (Defaults to \"none\"): ") nil nil "none")))

       (prompt-previous
        (lambda (subject)
          (concat (funcall prev-workday-abbv) ": " subject ": "
                  (funcall prompt-todos
                           (concat subject
                                   " for "
                                   (funcall prev-workday))))))
       (prompt-current
        (lambda (subject)
          (concat "T: " subject ": "
                  (funcall prompt-todos (concat subject " for today")))))

       (prompt-next
        (lambda (subject)
          (concat "Next: " subject ": "
                  (funcall prompt-todos
                           (concat subject
                                   " for "
                                   (funcall next-workday))))))

       (join-newline
        (lambda (lst)
          (mapconcat 'identity lst "\n")))

       (make-prompt
        (lambda (type task)
          (case type
            ('previous (funcall prompt-previous task))
            ('current  (funcall prompt-current task))
            ('next     (funcall prompt-next task))
            (t         (funcall prompt-question task)))))

       (make-from-template
        (lambda ()
          (cl-loop for (header . tasks) in standup-template-alist
                   collect
                   (funcall join-newline
                            (cl-loop for task in tasks
                                     collect
                                     (funcall make-prompt header task)))))))

    (with-help-window "*standup-notes*"
      (princ
       (funcall join-newline (funcall make-from-template))))))

(provide 'standup-notes)
;;; standup-notes.el ends here
