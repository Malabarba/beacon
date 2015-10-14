;;; beacon.el --- Highlight the cursor whenever it moves long distances  -*- lexical-binding: t; -*-

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
;;     (beacon-mode 1)

;;; Code:

(require 'cl-lib)

(defgroup beacon nil
  "Customization group for beacon."
  :group 'emacs
  :prefix "beacon-")

(defvar beacon--timer nil)

(defcustom beacon-push-mark nil
  "Should the mark be pushed before long movements?"
  :type 'boolean)

(defcustom beacon-blink-when-point-moves nil
  "Should the beacon blink when changing buffer?
If nil, don't blink due to plain movement.
If non-nil, this should be an integer, which is the minimum
movement distance (in lines) that triggers a beacon blink."
  :type '(choice integer (const nil)))

(defcustom beacon-blink-when-buffer-changes t
  "Should the beacon blink when changing buffer?"
  :type 'boolean)

(defcustom beacon-blink-when-window-scrolls t
  "Should the beacon blink when the window scrolls?"
  :type 'boolean)

(defcustom beacon-blink-duration 0.3
  "Time, in seconds, that the blink should last."
  :type 'number)

(defcustom beacon-blink-delay 0.3
  "Time, in seconds, before starting to fade the beacon."
  :type 'number)

(defcustom beacon-size 30
  "Size of the beacon in characters."
  :type 'number)

(defcustom beacon-brightness 0.4
  "Brightness as a float between 0 and 1."
  :type 'number)


;;; Overlays
(defvar beacon--ovs nil)

(defun beacon--colored-overlay (color)
  "Put an overlay at point with background COLOR."
  (let ((ov (make-overlay (point) (1+ (point)))))
    (overlay-put ov 'face (list :background color))
    (overlay-put ov 'beacon t)
    (push ov beacon--ovs)))

(defun beacon--ov-at-point ()
  (car (cl-member-if (lambda (o) (overlay-get o 'beacon))
                     (overlays-at (point)))))

(defun beacon--vanish ()
  "Turn off the beacon."
  (when (timerp beacon--timer)
    (cancel-timer beacon--timer))
  (mapc #'delete-overlay beacon--ovs)
  (setq beacon--ovs nil))


;;; Colors
(defun beacon--int-range (a b)
  "Return a list of integers between A inclusive and B exclusive.
Only returns `beacon-size' elements."
  (let ((d (/ (- b a) beacon-size))
        (out (list a)))
    (dotimes (_ (1- beacon-size))
      (push (+ (car out) d) out))
    (nreverse out)))

(defun beacon--color-range ()
  "Return a list of background colors for the beacon."
  (let ((bg (color-values (face-attribute 'default :background))))
    (apply #'cl-mapcar (lambda (r g b) (format "#%04x%04x%04x" r g b))
           (if (< (color-distance "black" bg)
                  (color-distance "white" bg))
               (mapcar (lambda (n) (butlast (beacon--int-range (* beacon-brightness 65535) n))) bg)
             (mapcar (lambda (n) (cdr (beacon--int-range (* (- 1 beacon-brightness) 65535) n))) bg)))))


;;; Blinking
(defun beacon--shine ()
  "Shine a beacon at point."
  (let ((colors (beacon--color-range)))
    (save-excursion
      (while colors
        (if (looking-at "$")
            (progn
              ;; (beacon--after-string)
              (setq colors nil))
          (beacon--colored-overlay (pop colors))
          (forward-char 1))))))

(defun beacon--dec ()
  "Decrease the beacon brightness by one."
  (let ((o (beacon--ov-at-point)))
    (if (not o)
        (beacon--vanish)
      (delete-overlay o)
      (save-excursion
        (while (progn (forward-char 1)
                      (setq o (beacon--ov-at-point)))
          (move-overlay o (1- (point)) (point)))))))

(defun beacon-blink ()
  "Blink the beacon at the position of the cursor."
  (interactive)
  (beacon--vanish)
  (beacon--shine)
  (setq beacon--timer
        (run-at-time beacon-blink-delay
                     (/ beacon-blink-duration 1.0 beacon-size)
                     #'beacon--dec)))


;;; Movement detection
(defvar beacon--previous-place nil)
(defvar beacon--previous-window-start nil)
(defvar beacon--previous-mark-head nil)

(defun beacon--maybe-push-mark ()
  "Push mark if it seems to be safe."
  (when (and beacon-push-mark
             (not mark-active))
    (let ((head (car mark-ring)))
      (when (and (eq beacon--previous-mark-head head)
                 (not (equal head beacon--previous-place)))
        (push-mark beacon--previous-place)))))

(defun beacon--post-command ()
  "Blink if point moved very far."
  (cond
   ((not (markerp beacon--previous-place))
    (beacon--vanish))
   ;; Blink because we changed buffer.
   ((not (equal (marker-buffer beacon--previous-place)
                (current-buffer)))
    (when beacon-blink-when-buffer-changes
      (unless (window-minibuffer-p)
        (beacon-blink))))
   ;; Blink for scrolling.
   ((and beacon-blink-when-window-scrolls
         (progn (redisplay)
                (not (equal beacon--previous-window-start (window-start)))))
    (beacon--maybe-push-mark)
    (beacon-blink))
   ;; Blink for movement
   ((and beacon-blink-when-point-moves
         (> (abs (- (point) beacon--previous-place))
            beacon-blink-when-point-moves)
         (> (count-screen-lines (min (point) beacon--previous-place)
                                (max (point) beacon--previous-place))))
    (beacon--maybe-push-mark)
    (beacon-blink))
   ;; Even if we don't blink, vanish any previous beacon.
   (t (beacon--vanish)))
  (unless (window-minibuffer-p)
    (setq beacon--previous-window-start (window-start))
    (setq beacon--previous-mark-head (car mark-ring))
    (setq beacon--previous-place (point-marker))))


;;; Minor-mode
(defcustom beacon-lighter (cond
                     ((char-displayable-p ?💡) "💡")
                     ((char-displayable-p ?Λ) "Λ")
                     (t "*"))
  "Lighter string used on the mode-line."
  :type 'string)

;;;###autoload
(define-minor-mode beacon-mode
  nil nil beacon-lighter nil
  :global t
  (if beacon-mode
      (add-hook 'post-command-hook #'beacon--post-command)
    (remove-hook 'post-command-hook #'beacon--post-command)))

(provide 'beacon)
;;; beacon.el ends here
