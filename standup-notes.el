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

(defun standup-notes ()
  "Generate standup-notes interactively. The user will be
prompted to enter various information for the previous and
current work days, and a nicely formatted report will be
generated in a help buffer. "
  (interactive)
  (letrec
      ((was-fridayp
        (lambda ()
          (let ((today (format-time-string "%u")))
            (or (equal today "0")
                (equal today "1")
                (equal today "7")))))

       (prev-workday-abbv
        (lambda ()
          (if (funcall was-fridayp) "F" "Y")))

       (prev-workday-full
        (lambda ()
          (if (funcall was-fridayp) "Friday" "yesterday")))

       (prompt-question
        (lambda (question)
          (let ((input (read-string (concat question " (Defaults to \"no\"): ")
                                    nil nil "no")))
            (if (equal input "no")
                ""
              (concat input "\n")))))

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
                                   (funcall prev-workday-full))) "\n")))

       (prompt-current
        (lambda (subject)
          (concat "T: " subject ": "
                  (funcall prompt-todos (concat subject " for today")) "\n"))))

    ;; TODO: Add support for standup templates. Currently only a fixed
    ;; number of tasks are supported.
    (with-help-window "*standup-notes*"
      (princ
       (concat (funcall prompt-previous "Non-development work")
               (funcall prompt-previous "Tickets")
               (funcall prompt-current  "Non-development work")
               (funcall prompt-current  "Tickets")
               (funcall prompt-current  "PR reviews")
               (funcall prompt-current  "PR reviews needed")
               (funcall prompt-question "Do you have any blockers?")
               (funcall prompt-question "Do you need to talk to anyone?"))))))

(provide 'standup-notes)
;;; standup-notes.el ends here
