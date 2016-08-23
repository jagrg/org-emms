;;; org-emms.el --- Play multimedia files from org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jonathan Gregory

;; Author: Jonathan Gregory <jgrg at autistici dot org>
;; Keywords: multimedia

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

;; This package provides basic emms support for org-mode. It adds a
;; new org link type to the list of `org-link-types', which can be
;; used to playback multimedia files from an org file. If the link
;; contains a track position, playback will start at that position.
;; For example:

;; [[emms:../path/to/audio.mp3::00:21:43]] Playback starts at 21 min 43 sec.

;; The two main commands are `org-emms-insert-track' and
;; `org-emms-insert-track-position'.

;; See also: http://orgmode.org/worg/code/elisp/org-player.el

;;; Code:

(require 'org)
(require 's)
(require 'emms)
(require 'emms-playing-time)

(defun org-emms-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  (cond
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
          (min (string-to-number (match-string 2 s)))
          (sec (string-to-number (match-string 3 s))))
      (+ (* hour 3600) (* min 60) sec)))
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s)))
          (sec (string-to-number (match-string 2 s))))
      (+ (* min 60) sec)))
   ((stringp s) (string-to-number s))
   (t s)))

(defun org-emms-play (file)
  "Play multimedia FILE from org-mode.
If link contains a track position, start there. Otherwise, playback
from the start."
  (let* ((path (s-split "::" file))
	 (track (car path))
	 (time (org-emms-time-string-to-seconds (cadr path))))
    (emms-play-file track)
    (when time
      (emms-seek-to time))))

(org-add-link-type
 "emms"
 'org-emms-play
 (lambda (path desc format)
   (if desc
       (format "\[%s\]" desc)
     (format "\[%s\]" path))))

;;;###autoload
(defun org-emms-insert-link (arg)
  "Insert org link using completion.
Prompt for a file name and link description. With a prefix ARG, prompt
for a track position."
  (interactive "P")
  (let ((file (read-file-name "File: ")))
    (if arg
	(let ((tp (read-string "Track position (hh:mm:ss): ")))
	  (insert (format "[[emms:%s::%s][%s]]" file tp tp)))
      (let ((desc (read-string "Description: ")))
	(insert (format "[[emms:%s][%s]]" file desc))))))

;;;###autoload
(defun org-emms-insert-track ()
  "Insert current selected track as an org link."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
	 (file (emms-track-name track))
	 (title (emms-track-get track 'info-title)))
    (when (eq major-mode 'org-mode)
      (insert
       (format "[[emms:%s][%s]]" file title)))))

;;;###autoload
(defun org-emms-insert-track-position ()
  "Insert current track position as an org link."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
	 (file (file-relative-name (emms-track-name track)))
	 (hh (/ emms-playing-time 3600))
	 (mm (/ emms-playing-time 60))
	 (ss (% emms-playing-time 60))
	 (tp (format "%02d:%02d:%02d" hh mm ss)))
    (insert
     (if (eq major-mode 'org-mode)
	 (format "[[emms:%s::%s][%s]]" file tp tp)
       (format "[%s]" tp)))))

(provide 'org-emms)
;;; org-emms.el ends here
