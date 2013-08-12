;;; color-dired.el --- Make colorize dired buffer by modified time.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: dired color
;; URL: https://github.com/mhayashi1120/Emacs-color-dired/raw/master/color-dired.el
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

;;; History:

;; This program is inspired from following url.
;; http://www.bookshelf.jp/soft/meadow_25.html#SEC288
;; http://homepage1.nifty.com/blankspace/emacs/dired.html

;;; Usage:

;;  M-x customize-group (color-dired)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar emacs-major-version)

(defgroup color-dired nil
  "Colored dired."
  :group 'dired
  :prefix "color-dired-")

(defface color-dired-changed-today-face
  '((((class color)
      (background light))
     (:foreground "#00ff00" :weight bold))
    (((class color)
      (background dark))
     (:foreground "#ff00ff" :weight bold)))
  "Font lock mode face used to highlight changed in this day."
  :group 'color-dired)

(defface color-dired-changed-this-week-face
  '((((class color)
      (background light))
     (:foreground "#00ee08" :weight bold))
    (((class color)
      (background dark))
     (:foreground "#ff22f8" :weight bold)))
  "Font lock mode face used to highlight changed in this week."
  :group 'color-dired)

(defface color-dired-changed-last-week-face
  '((((class color)
      (background light))
     (:foreground "#00ff00"))
    (((class color)
      (background dark))
     (:foreground "#ff00ff")))
  "Font lock mode face used to highlight changed in last week."
  :group 'color-dired)

(defface color-dired-changed-last-week-before-face
  '((((class color)
      (background light))
     (:foreground "#00cc18"))
    (((class color)
      (background dark))
     (:foreground "#ff66e8")))
  "Font lock mode face used to highlight changed in last week before."
  :group 'color-dired)

(make-obsolete 'color-dired-modify-tramp-remote-environment nil "1.2.0")

(defun color-dired-modify-tramp-remote-environment (name &optional value)
  (message "This is obsolete function."))

(defun color-dired-guessed-date-format (dir)
  (let (date)
    (catch 'done
      ;; try to get dired buffer date format
      (with-temp-buffer
        (insert-directory dir dired-listing-switches)
        (dolist (x (split-string (buffer-string) "\n" t))
          (when (string-match "^d.* [0-9]+ \\(.*?\\) [0-9]+:[0-9]+" x)
            (setq date (match-string 1 x))
            (throw 'done t))))
      ;; for windows.
      (require 'ls-lisp)
      (when (fboundp 'ls-lisp-format-time)
        (let ((string (or (condition-case nil
                              (funcall 'ls-lisp-format-time
                                       (file-attributes dir) nil (current-time))
                            (error nil))
                          (funcall 'ls-lisp-format-time
                                   (file-attributes dir) nil))))
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

(defcustom color-dired-date-format (color-dired-guessed-date-format "~/")
  "Default format of displaying time. See `format-time-string'
This variable indirectly point format of displaying time.
`color-dired-buffer-date-format' variable directly point format of
displaying time to current buffer."
  :group 'color-dired
  :type 'string)

(defcustom color-dired-suppress-guessing-at-remote t
  "Suppress high cost calculation in `file-remote-p' return non-nil directory."
  :group 'color-dired
  :type 'boolean)

(defcustom color-dired-buffer-date-format nil
  "Format of displaying time of current buffer.
This variable is used as cached variable of this package.

Cache variable of calculating format.

e.g.
\(add-hook 'some-major-mode-hook
          (lambda () (setq color-dired-buffer-date-format \"%Y-%m-%d\")))
"
  :group 'color-dired
  :type 'string)

(make-variable-buffer-local 'color-dired-buffer-date-format)

(make-obsolete-variable 'color-dired-time-regexp
                        'color-dired-general-time-regexp "1.2.0")

(defvar color-dired-general-time-regexp
  (concat
   " "
   (color-dired-regexp-opt
    (append
     (mapcar (lambda (x) (format "%02d" x)) (number-sequence 0 23))
     (mapcar (lambda (x) (format "%2d" x)) (number-sequence 0 23))
     (mapcar 'number-to-string (number-sequence 0 23))))
   ":[0-5][0-9]")
  "Regexp to match datetime")

(defun color-dired--time-sequence (num start-seconds)
  (loop repeat num
        for sec = start-seconds then (+ sec (* 24 60 60))
        collect (seconds-to-time sec)))

(defun color-dired--week-start-seconds (week)
  (-
   (float-time)
   (*
    ;; sec per day
    24 60 60
    ;; past day from last week sunday
    (+ (color-dired--week-number) (* week 7)))))

(defun color-dired--date-format ()
  (or color-dired-buffer-date-format
      (setq color-dired-buffer-date-format
            (or (and default-directory
                     (or (not color-dired-suppress-guessing-at-remote)
                         (not (file-remote-p default-directory)))
                     (color-dired-guessed-date-format default-directory))
                color-dired-date-format))))

(defun color-dired--week-number ()
  (string-to-number (format-time-string "%w")))

(defun color-dired--generate-regexp (num start-seconds)
  "Return NUM of day regexp depend upon `color-dired-date-format'.
START-SECONDS means start time as float value."
  (let ((fmt (color-dired--date-format)))
    (cond
     ((null fmt))
     (t
      (let* ((times (color-dired--time-sequence num start-seconds))
             (day-list (mapcar
                        (lambda (time)
                          (format-time-string fmt time))
                        times)))
        (and day-list
             (concat " "
                     (color-dired-regexp-opt day-list)
                     color-dired-general-time-regexp
                     " ")))))))

(defun color-dired--search-today (bound)
  "font-lock search function for dired."
  (let ((regexp (color-dired--generate-regexp 1 (float-time))))
    (re-search-forward regexp bound t)))

(defun color-dired--search-this-week (bound)
  ;; search current week except today
  (let* ((start-with (color-dired--week-start-seconds 0))
         (count (color-dired--week-number))
         (regexp (color-dired--generate-regexp count start-with)))
    (and regexp (re-search-forward regexp bound t))))

(defun color-dired--search-last-week (bound)
  (let* ((start-with (color-dired--week-start-seconds 1))
         (regexp (color-dired--generate-regexp 7 start-with)))
    (and regexp (re-search-forward regexp bound t))))

(defun color-dired--search-last-week-before (bound)
  (let* ((start-with (color-dired--week-start-seconds 2))
         (regexp (color-dired--generate-regexp 7 start-with)))
    (and regexp (re-search-forward regexp bound t))))

;;;###autoload
(defconst color-dired-search-keywords
  '((color-dired--search-today . 'color-dired-changed-today-face)
    (color-dired--search-this-week . 'color-dired-changed-this-week-face)
    (color-dired--search-last-week . 'color-dired-changed-last-week-face)
    (color-dired--search-last-week-before . 'color-dired-changed-last-week-before-face)))

;;;###autoload
(font-lock-add-keywords 'dired-mode color-dired-search-keywords)

(provide 'color-dired)

;;; color-dired.el ends here
