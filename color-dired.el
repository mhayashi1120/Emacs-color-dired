;;; color-dired.el --- Make colorize dired buffer by modified time.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: dired color
;; URL: http://github.com/mhayashi1120/Emacs-color-dired/raw/master/color-dired.el
;; Emacs: GNU Emacs 22 or later
;; Version: 1.2.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; color-dired provides colored text that recently changed. (`Today', `This week'
;; ,`Last week' and `Last week before')

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'color-dired)
;;
;; After that, file that is recently changed is emphasized in `dired' buffer.
;; If you have changed date format in `dired',
;; set `color-dired-date-format' like following.
;;
;;     (setq color-dired-date-format "%Y-%m-%d")

;;; History:

;; This program is inspired from following url.
;; http://www.bookshelf.jp/soft/meadow_25.html#SEC288
;; http://homepage1.nifty.com/blankspace/emacs/dired.html

;;; Usage:

;; You can change the face by changing following variables.
;; `color-dired-changed-today-face'
;; `color-dired-changed-this-week-face'
;; `color-dired-changed-last-week-face'
;; `color-dired-changed-last-week-before-face'

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar emacs-major-version)

(defgroup color-dired nil
  "Colored dired."
  :group 'dired
  :prefix "color-dired-")

(defface color-dired-changed-today-face
  '(
    (((class color)
      (background light))
     (:foreground "Green" :weight bold))
    (((class color)
      (background dark))
     (:foreground "light green" :weight bold)))
  "Font lock mode face used to highlight changed in this day."
  :group 'color-dired)

(defface color-dired-changed-this-week-face
  '(
    (((class color)
      (background light))
     (:foreground "SpringGreen" :weight bold))
    (((class color)
      (background dark))
     (:foreground "SpringGreen" :weight bold)))
  "Font lock mode face used to highlight changed in this week."
  :group 'color-dired)

(defface color-dired-changed-last-week-face
  '(
    (((class color)
      (background light))
     (:foreground "MediumSpringGreen"))
    (((class color)
      (background dark))
     (:foreground "MediumSpringGreen")))
  "Font lock mode face used to highlight changed in last week."
  :group 'color-dired)

(defface color-dired-changed-last-week-before-face
  '(
    (((class color)
      (background light))
     (:foreground "GreenYellow"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow")))
  "Font lock mode face used to highlight changed in last week before."
  :group 'color-dired)

(defun color-dired-guessed-date-format ()
  (let (date)
    (catch 'done
      (with-temp-buffer
        (insert-directory "~" "-la")
        (dolist (x (split-string (buffer-string) "\n" t))
          (when (string-match "^d.* [0-9]+ \\(.*?\\) [0-9]+:[0-9]+" x)
            (setq date (match-string 1 x))
            (throw 'done t))))
      ;; for windows.
      (require 'ls-lisp)
      (when (fboundp 'ls-lisp-format-time)
        (let ((string (or (condition-case nil
                              (funcall 'ls-lisp-format-time
                                       (file-attributes "~/") nil (current-time))
                            (error nil))
                          (funcall 'ls-lisp-format-time
                                   (file-attributes "~/") nil))))
          (setq date (car (split-string string))))))
    (cond
     ((null date)
      "%b %e")
     ((string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
      "%Y-%m-%d")
     ((string-match "^[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
      "%y-%m-%d")
     ((string-match "^[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
      "%m-%d")
     (t
      "%b %e"))))

;; `regexp-opt' is generating different regexp.
(if (> emacs-major-version 22)
    (defun color-dired-regexp-opt (strings)
      (regexp-opt strings))
  (defun color-dired-regexp-opt (strings)
    (concat "\\(?:" (regexp-opt strings) "\\)")))

(defcustom color-dired-date-format (color-dired-guessed-date-format)
  "Format of dired displaying time. See `format-time-string'"
  :group 'color-dired
  :type 'string)

(defcustom color-dired-time-regexp
  (concat
   " "
   (color-dired-regexp-opt
    (append
     (mapcar (lambda (x) (format "%02d" x)) (number-sequence 0 23))
     (mapcar (lambda (x) (format "%2d" x)) (number-sequence 0 23))
     (mapcar 'number-to-string (number-sequence 0 23))))
   ":[0-5][0-9]")
  "Regexp to match datetime"
  :group 'color-dired
  :type 'string)

(defconst color-dired-search-keywords
  '((color-dired--search-today . 'color-dired-changed-today-face)
    (color-dired--search-this-week . 'color-dired-changed-this-week-face)
    (color-dired--search-last-week . 'color-dired-changed-last-week-face)
    (color-dired--search-last-week-before . 'color-dired-changed-last-week-before-face)))

(defun color-dired--time-sequence (num start-seconds)
  (let ((step (* 24 60 60))
	(i 0)
	(time start-seconds)
        (res '()))
    (while (< i num)
      (setq res
	    (cons (seconds-to-time time) res))
      (setq time (+ step time))
      (setq i (1+ i)))
    (nreverse res)))

(defun color-dired--date-diff-seconds (days)
  (*
   ;; sec per day
   24 60 60.0
   ;; past day from last week sunday (except today)
   (+ (color-dired--week-number) days)))

(defun color-dired--week-number ()
  (string-to-number (format-time-string "%w")))

(defun color-dired--generate-regexp (num start-seconds)
  "Return NUM of day regexp depend upon `color-dired-date-format'.
START-SECONDS means start time as float value."
  (when color-dired-date-format
    (let* ((times (color-dired--time-sequence num start-seconds))
	   (day-list (mapcar
		      (lambda (time)
			(format-time-string color-dired-date-format time))
		      times)))
      (and
       day-list
       (concat " "
	       (color-dired-regexp-opt day-list)
	       color-dired-time-regexp
	       " ")))))

(defun color-dired--search-today (bound)
  "font-lock search function for dired."
  (let ((regexp (color-dired--generate-regexp 1 (float-time))))
    (re-search-forward regexp bound t)))

(defun color-dired--search-this-week (bound)
  (let* ((start-with (- (float-time)
                        (color-dired--date-diff-seconds 0)))
         (regexp (color-dired--generate-regexp 0 start-with)))
    (and regexp (re-search-forward regexp bound t))))

(defun color-dired--search-last-week (bound)
  (let* ((start-with (- (float-time)
                        (color-dired--date-diff-seconds 7)))
         (regexp (color-dired--generate-regexp 7 start-with)))
    (and regexp (re-search-forward regexp bound t))))

(defun color-dired--search-last-week-before (bound)
  (let* ((start-with (- (float-time)
                        (color-dired--date-diff-seconds 14)))
         (regexp (color-dired--generate-regexp 7 start-with)))
    (and regexp (re-search-forward regexp bound t))))

(font-lock-add-keywords 'dired-mode color-dired-search-keywords)

(defun color-dired--modify-tramp-remote-environment (name &optional value)
  (setq tramp-remote-process-environment
        (loop with regexp = (format "^%s=" (regexp-quote name))
              with done
              with cell = (format "%s=%s" name value)
              for v in tramp-remote-process-environment
              if (string-match regexp v)
              append (progn
                       (setq done t)
                       (and value (list cell)))
              into res
              else
              collect v into res
              finally return
              (if (or done (null value))
                  res
                (cons cell res)))))

(provide 'color-dired)

;;; color-dired.el ends here
