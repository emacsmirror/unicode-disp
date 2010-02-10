;;; unicode-disp.el --- display-table fallbacks for some unicode chars

;; Copyright 2008, 2009 Kevin Ryde
;;
;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: i18n
;; URL: http://user42.tuxfamily.org/unicode-disp/index.html
;;
;; unicode-disp.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; unicode-disp.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; M-x unicode-disp changes the display table to show selected unicode chars
;; as ascii equivalents or near equivalents.  The chars affected are a
;; personal selection of the worst bits of unicode encountered, just with
;; the aim of making them displayable on an ascii or latin1 tty.  If nothing
;; else it might give you ideas for display table mangling of your own.

;;; Emacsen:

;; Designed for Emacs 21 and up, does nothing in XEmacs 21.

;;; Install:

;; Put unicode-disp.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (autoload 'unicode-disp "unicode-disp" nil t)
;;     (add-hook 'term-setup-hook 'unicode-disp)
;;
;; There's an autoload cookie for the function, if you know how to use
;; `update-file-autoloads' and friends, after which just add or customize
;; the hook (or use M-x unicode-disp when desired).

;;; History:

;; Version 1 - the first version

;;; Code:

(defun unicode-disp-char-displayable-p (char)
  "Return non-nil if CHAR can be shown on the current display."
  (if (eval-when-compile (fboundp 'char-displayable-p))
      ;; emacs22
      (char-displayable-p char)
    ;; emacs21, assume everything displayable on X or on a utf8 tty
    (or window-system
        (eq 'utf-8 (terminal-coding-system)))))

(defun unicode-disp-attr-displayable-p (attr &optional display)
  "Return non-nil if ATTR can be shown on DISPLAY.
ATTR is a keyword like :overline.
DISPLAY defaults to the current display."
  (if (eval-when-compile (fboundp 'display-supports-face-attributes-p))
      ;; emacs22
      (display-supports-face-attributes-p (list attr t) display)
    ;; emacs21
    (assq attr face-attribute-name-alist)))

(defun unicode-disp-overline-face (&optional display)
  "Return an overline face for DISPLAY, or nil.
If the display can't show an overline face the return is nil.
DISPLAY defaults to the current frame.  "
  (when (unicode-disp-attr-displayable-p :overline display)
    (unless (facep 'unicode-disp-overline)
      (make-face 'unicode-disp-overline)
      (set-face-attribute 'unicode-disp-overline nil :overline t))
    'unicode-disp-overline))

;;;###autoload
(defun unicode-disp ()
  "Setup some display table fallbacks for unicode chars.
`standard-display-table' is modified to show a few unicode chars
as ascii near-equivalents if they're not otherwise displayable.
For example if like U+2010 HYPHEN isn't displayable then it's
setup as a plain ascii \"-\".

This only affects the screen display, the characters in the
buffers remain unchanged."

  (interactive)
  (when (eval-when-compile (boundp 'standard-display-table)) ;; not in xemacs
    (let (table)

      (dolist (elem '((#x2010 "-")  ;; HYPHEN
                      (#x2018 "`")  ;; LEFT SINGLE QUOTATION MARK
                      (#x2019 "'")  ;; RIGHT SINGLE QUOTATION MARK
                      (#x201C "\"") ;; LEFT DOUBLE QUOTATION MARK
                      (#x201D "\"") ;; RIGHT DOUBLE QUOTATION MARK
                      (#x2192 "->") ;; RIGHTWARDS ARROW
                      (#x2212 "-")  ;; MINUS SIGN
                      (#x2502 "|")  ;; BOX DRAWINGS LIGHT VERTICAL
                      (#x203E " "   ;; OVERLINE as face
                              unicode-disp-overline-face)))

        (let* ((char (decode-char 'ucs (nth 0 elem)))
               (str  (nth 1 elem))
               (face (nth 2 elem)))

          ;; only if char not already displayable, and if any face func
          ;; specified returns a face
          (when (and (not (unicode-disp-char-displayable-p char))
                     (or (not face)
                         (setq face (funcall face))))

            ;; standard-display-table starts life nil, initialize it
            ;; like disp-table.el does, when needed
            (unless table
              (setq table (or standard-display-table
                              (make-display-table))))

            (aset table char
                  (vconcat (mapcar
                            (lambda (c)
                              (if (eval-when-compile
                                    (fboundp 'make-glyph-code))
                                  (make-glyph-code c face) ;; emacs22
                                ;; emacs21
                                (logior c (* 524288
                                             (if face (face-id face) 0)))))
                            str))))))

      (setq standard-display-table table))))

;;;###autoload
(custom-add-option 'term-setup-hook 'unicode-disp)

(provide 'unicode-disp)

;;; unicode-disp.el ends here
