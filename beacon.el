;;; beacon.el --- Highlight the cursor whenever it moves long distances  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/beacon
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
  "Should the mark be pushed before long movements?
If nil, `beacon' will not push the mark.
Otherwise this should be a number, and `beacon' will push the
mark whenever point moves more than that many lines."
  :type '(choice integer (const nil)))

(defcustom beacon-blink-when-point-moves nil
  "Should the beacon blink when moving a long distance?
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

(defcustom beacon-size 40
  "Size of the beacon in characters."
  :type 'number)

(defcustom beacon-color 0.5
  "Color of the beacon.
This can be a string or a number.

If it is a number, the color is taken to be white or
black (depending on the current theme's background) and this
number is a float between 0 and 1 specifing the brightness.

If it is a string, it is a color name or specification,
e.g. \"#666600\"."
  :type '(choice number color))


;;; Overlays
(defvar beacon--ovs nil)

(defun beacon--colored-overlay (color)
  "Put an overlay at point with background COLOR."
  (let ((ov (make-overlay (point) (1+ (point)))))
    (overlay-put ov 'face (list :background color))
    (overlay-put ov 'beacon t)
    (push ov beacon--ovs)))

(defun beacon--ov-put-after-string (overlay colors)
  "Add an after-string property to OVERLAY.
The property's value is a string of spaces with background
COLORS applied to each one."
  (overlay-put overlay 'beacon-colors colors)
  (overlay-put overlay 'after-string
               (mapconcat (lambda (c) (propertize " " 'face (list :background c)))
                          colors
                          "")))

(defun beacon--after-string-overlay (colors)
  "Put an overlay at point with an after-string property.
The property's value is a string of spaces with background
COLORS applied to each one."
  (let ((ov (make-overlay (point) (point))))
    (beacon--ov-put-after-string ov colors)
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
  (let* ((bg (color-values (face-attribute 'default :background)))
         (fg (cond
              ((stringp beacon-color) (color-values beacon-color))
              ((< (color-distance "black" bg)
                  (color-distance "white" bg))
               (make-list 3 (* beacon-color 65535)))
              (t (make-list 3 (* (- 1 beacon-color) 65535))))))
    (apply #'cl-mapcar (lambda (r g b) (format "#%04x%04x%04x" r g b))
           (mapcar (lambda (n) (butlast (beacon--int-range (elt fg n) (elt bg n))))
                   [0 1 2]))))


;;; Blinking
(defun beacon--shine ()
  "Shine a beacon at point."
  (let ((colors (beacon--color-range)))
    (save-excursion
      (while colors
        (if (looking-at "$")
            (progn
              (beacon--after-string-overlay colors)
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
          (let ((colors (overlay-get o 'beacon-colors)))
            (if (not colors)
                (move-overlay o (1- (point)) (point))
              (forward-char -1)
              (beacon--colored-overlay (pop colors))
              (beacon--ov-put-after-string o colors))))))))

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

(defun beacon--movement-> (delta)
  "Return non-nil if latest point movement is > DELTA.
If DELTA is nil, return nil."
  (and delta
       (markerp beacon--previous-place)
       (equal (marker-buffer beacon--previous-place)
              (current-buffer))
       (> (abs (- (point) beacon--previous-place))
          delta)
       (> (count-screen-lines (min (point) beacon--previous-place)
                              (max (point) beacon--previous-place))
          delta)))

(defun beacon--maybe-push-mark ()
  "Push mark if it seems to be safe."
  (when (and (not mark-active)
             (beacon--movement-> beacon-push-mark))
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
    (beacon-blink))
   ;; Blink for movement
   ((beacon--movement-> beacon-blink-when-point-moves)
    (beacon-blink))
   ;; Even if we don't blink, vanish any previous beacon.
   (t (beacon--vanish)))
  (beacon--maybe-push-mark)
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
