;;; spotlight.el --- Highlight the cursor whenever it moves long distances  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5"))

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

;; This is a global minor-mode.  Turn it on everywhere with
;;     (spotlight-mode 1)

;;; Code:

(require 'cl-lib)

(defgroup spotlight nil
  "Customization group for spotlight."
  :group 'emacs
  :prefix "spotlight-")

(defvar spotlight--timer nil)

(defcustom spotlight-minimum-distance 15
  "Minimum movement distance in lines to blink the spotlight."
  :type 'integer)

(defcustom spotlight-push-mark nil
  "Should the mark be pushed before long movements?"
  :type 'boolean)

(defcustom spotlight-blink-when-buffer-changes t
  "Should the spotlight blink when changing buffer?"
  :type 'boolean)

(defcustom spotlight-blink-duration 0.3
  "Time, in seconds, that the blink should last."
  :type 'number)

(defcustom spotlight-blink-delay 0.3
  "Time, in seconds, before starting to fade the spotlight."
  :type 'number)

(defcustom spotlight-size 15
  "Size of the spotlight in characters."
  :type 'number)

(defcustom spotlight-brightness 0.5
  "Brightness as a float between 0 and 1."
  :type 'number)


;;; Overlays
(defvar spotlight--ovs nil)

(defun spotlight--colored-overlay (color)
  "Put an overlay at point with background COLOR."
  (let ((ov (make-overlay (point) (1+ (point)))))
    (overlay-put ov 'face (list :background color))
    (overlay-put ov 'spotlight t)
    (push ov spotlight--ovs)))

(defun spotlight--ov-at-point ()
  (car (cl-member-if (lambda (o) (overlay-get o 'spotlight))
                     (overlays-at (point)))))

(defun spotlight--vanish ()
  "Turn off the spotlight."
  (when (timerp spotlight--timer)
    (cancel-timer spotlight--timer))
  (mapc #'delete-overlay spotlight--ovs)
  (setq spotlight--ovs nil))


;;; Colors
(defun spotlight--int-range (a b)
  "Return a list of integers between A inclusive and B exclusive.
Only returns `spotlight-size' elements."
  (let ((d (/ (- b a) spotlight-size))
        (out (list a)))
    (dotimes (_ (1- spotlight-size))
      (push (+ (car out) d) out))
    (nreverse out)))

(defun spotlight--color-range ()
  "Return a list of background colors for the spotlight."
  (let ((bg (color-values (face-attribute 'default :background))))
    (apply #'cl-mapcar (lambda (r g b) (format "#%04x%04x%04x" r g b))
           (if (< (color-distance "black" bg)
                  (color-distance "white" bg))
               (mapcar (lambda (n) (butlast (spotlight--int-range (* spotlight-brightness 65535) n))) bg)
             (mapcar (lambda (n) (cdr (spotlight--int-range (* (- 1 spotlight-brightness) 65535) n))) bg)))))


;;; Blinking
(defun spotlight--shine ()
  "Shine a spotlight at point."
  (let ((colors (spotlight--color-range)))
    (save-excursion
      (while colors
        (if (looking-at "$")
            (progn
              ;; (spotlight--after-string)
              (setq colors nil))
          (spotlight--colored-overlay (pop colors))
          (forward-char 1))))))

(defun spotlight--dec ()
  "Decrease the spotlight brightness by one."
  (let ((o (spotlight--ov-at-point)))
    (if (not o)
        (spotlight--vanish)
      (delete-overlay o)
      (save-excursion
        (while (progn (forward-char 1)
                      (setq o (spotlight--ov-at-point)))
          (move-overlay o (1- (point)) (point)))))))

(defun spotlight-blink ()
  "Blink the spotlight at the position of the cursor."
  (interactive)
  (spotlight--vanish)
  (spotlight--shine)
  (setq spotlight--timer
        (run-at-time spotlight-blink-delay
                     (/ spotlight-blink-duration 1.0 (length spotlight--colors))
                     #'spotlight--dec)))


;;; Movement detection
(defvar spotlight--previous-place nil)
(defvar spotlight--previous-mark-head nil)

(defun spotlight--maybe-push-mark ()
  "Push mark if it seems to be safe."
  (when (and spotlight-push-mark
             (not mark-active))
    (let ((head (car mark-ring)))
      (when (and (eq spotlight--previous-mark-head head)
                 (not (equal head spotlight--previous-place)))
        (push-mark spotlight--previous-place)))))

(defun spotlight--post-command ()
  "Blink if point moved very far."
  (cond
   ((not (markerp spotlight--previous-place))
    (spotlight--vanish))
   ;; Blink because we changed buffer.
   ((not (equal (marker-buffer spotlight--previous-place)
                (current-buffer)))
    (when spotlight-blink-when-buffer-changes
      (unless (window-minibuffer-p)
        (spotlight-blink))))
   ;; Blink for distance movement
   ((and (> (abs (- (point) spotlight--previous-place))
            spotlight-minimum-distance)
         (> (count-screen-lines (min (point) spotlight--previous-place)
                                (max (point) spotlight--previous-place))
            spotlight-minimum-distance))
    (spotlight--maybe-push-mark)
    (spotlight-blink))
   ;; Even if we don't blink, vanish any previous spotlight.
   (t (spotlight--vanish)))
  (unless (window-minibuffer-p)
    (setq spotlight--previous-mark-head (car mark-ring))
    (setq spotlight--previous-place (point-marker))))


;;; Minor-mode
(defcustom spotlight-lighter (cond
                     ((char-displayable-p ?💡) "💡")
                     ((char-displayable-p ?Λ) "Λ")
                     (t "*"))
  "Lighter string used on the mode-line."
  :type 'string)

;;;###autoload
(define-minor-mode spotlight-mode
  nil nil spotlight-lighter nil
  :global t
  (if spotlight-mode
      (add-hook 'post-command-hook #'spotlight--post-command)
    (remove-hook 'post-command-hook #'spotlight--post-command)))

(provide 'spotlight)
;;; spotlight.el ends here
