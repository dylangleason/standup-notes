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

(defvar standup-notes-alist '((previous . ("Non-development work"
                                           "Tickets"))
                              (current  . ("Non-development work"
                                           "Tickets"
                                           "PRs to review"
                                           "PRs needed"))
                              (next     . ("Tickets"))
                              (blockers . ("Do you have any blockers?"))
                              (notes    . ("Do you need to meet with anyone?")))
  "The template to be used when generating standup-notes.
  The standup template contains template tasks for the keys
  PREVIOUS, CURRENT and NEXT. Override by binding this to a new
  value")

(defun standup-notes--day-of-week ()
  "Get the current day of week as an integer"
  (format-time-string "%u"))

(defun standup-notes--beginning-of-week-p ()
  "Predicate evaluates to T if the current day is the beginning of the
week, otherwise NIL"
  (member (standup-notes--day-of-week) '("0" "1" "7")))

(defun standup-notes--end-of-week-p ()
  "Predicate evaluates to T if the current day is the end of the
week, otherwise NIL"
  (member (standup-notes--day-of-week) '("5" "6" "7")))

(defun standup-notes--prev-work-day ()
  (if (standup-notes--beginning-of-week-p)
      "Friday"
    "yesterday"))

(defun standup-notes--next-work-day ()
  (if (standup-notes--end-of-week-p)
      "Monday"
    "tomorrow"))

(defun standup-notes--prompt-notes (subject)
  "Prompt the user to input any notes on SUBJECT. If none, return
an empty string"
  (let ((input (read-string
                (format "%s (Defaults to \"no\"): " subject) nil nil "no")))
    (if (string= input "no") "" input)))

(defun standup-notes--prompt-task (task day)
  "Prompt the user to input a TASK for the DAY. Return a
formatted string indicating the user's work done for that TASK."
  (format "%s: %s"
          task
          (read-string (format "%s for %s (Defaults to \"none\"): " task day)
                       nil nil "none")))

(defun standup-notes--prompt-prev (task)
  "Prompt the user to input a TASK for the previous work
day. Returns a formatted string to include in final output."
  (standup-notes--prompt-task task (standup-notes--prev-work-day)))

(defun standup-notes--prompt-next (task)
  "Prompt the user to input a TASK for the next work
day. Returns a formatted string to include in final output."
  (standup-notes--prompt-task task (standup-notes--next-work-day)))

(defun standup-notes--prompt-curr (task)
  "Prompt the user to input a TASK for the next work
day. Returns a formatted string to include in final output."
  (standup-notes--prompt-task task "today"))

(defun standup-notes--make-prompt (type item)
  "Generate the prompt based on the standup key TYPE and an ITEM
or (task) for that TYPE."
  (case type
    ('previous (standup-notes--prompt-prev item))
    ('current  (standup-notes--prompt-curr item))
    ('next     (standup-notes--prompt-next item))
    ('blockers (standup-notes--prompt-notes item))
    (t         (standup-notes--prompt-notes item))))

(defun standup-notes--make-header (header)
  "Return a nicely formatted HEADER for the standup note topic."
  (case header
    ('previous (standup-notes--prev-work-day))
    ('current  "today")
    ('next     (standup-notes--next-work-day))
    ('blockers "blockers")
    (t         "notes")))

(defun standup-notes--make-tasks (header tasks)
  "Return a string for grouped TASKS under the specified HEADER
name. "
  (let ((str (funcall (lambda ()
                        (let ((append-to ""))
                          (dolist (task tasks)
                            (setq append-to
                                  (format "%s%s\n"
                                          append-to
                                          (standup-notes--make-prompt header task))))
                          (string-trim append-to))))))
    (if (string= "" str)
        str
      (format "\n## %s\n%s"
              (upcase (standup-notes--make-header header)) str))))

(defun standup-notes--prompt-slackify (str)
  "Prompt the user if they want to see a Slack-friendly
output, i.e. wraps final output STR in '```'."
  (let ((input (standup-notes--prompt-notes "Create slack-friendly output?")))
    (if (string= input "")
        str
      (format "```\n%s\n```" str))))

(defun standup-notes ()
    "Generate standup-notes interactively. The user will be
prompted to enter various information for the previous and
current work days, and a nicely formatted report will be
generated in a help buffer."
  (interactive)
  (with-help-window "*standup-notes*"
    (princ
     (standup-notes--prompt-slackify
      (string-trim
       (mapconcat 'identity
                  (cl-loop for (header . tasks) in standup-notes-alist
                           collect (standup-notes--make-tasks header tasks))
                  "\n"))))))

(provide 'standup-notes)
;;; standup-notes.el ends here
