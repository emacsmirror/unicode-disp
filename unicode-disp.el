;;; unicode-disp.el --- display-table fallbacks for some unicode chars

;; Copyright 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2017, 2019 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 13
;; Keywords: i18n, unicode, display
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

;; M-x unicode-disp changes the display table to show a few otherwise
;; undisplayable unicode chars as ASCII equivalents or near equivalents.
;;
;; The characters handled are a personal selection of the worst bits of
;; unicode encountered, with the aim of making them displayable on an ASCII
;; or Latin-1 tty.  If nothing else it might give you ideas for display
;; table mangling of your own.
;;
;; See latin1-disp.el for similar display table setups for otherwise
;; undisplayable characters from the iso-8859-N charsets and some cyrillic.
;;
;; Quite what to transform and how prominent it should be is a matter of
;; personal preference.  Displaying an arrow char as a sequence like "->"
;; can make text lines come out too long, or tables etc not align, sometimes
;; very badly.  A face like `escape-glyph' can make it clear you're looking
;; at non-ascii, except it becomes distracting if the screen is littered
;; with it.
;;
;; The variant unicode hyphens and quotes currently treated by
;; `unicode-disp' are on the whole pointless and are better displayed as
;; plain ascii "-" etc where necessary, with no special highlighting.

;;; Emacsen:

;; Designed for Emacs 21 and up, does nothing in XEmacs 21.

;;; Bugs:

;; unicode-disp.el might or might not work with other display table mangling
;; packages.  Some packages which change the display table have their own
;; ideas on things like saving an existing table etc.  Anything which just
;; makes desired changes should work, but if trying to be cleverer then
;; maybe not.

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
;; Version 2 - act on window and buffer display tables too
;; Version 3 - express dependency on 'advice rather than maybe reloading it
;; Version 4 - term-setup-hook isn't customizable
;; Version 5 - some more chars
;; Version 6 - mathematical <> bracket chars
;; Version 7 - en dash
;; Version 8 - quieten xemacs byte compile a little
;; Version 9 - bullet, <= and >=
;; Version 10 - oops, need unicode-disp--with-selected-frame in defadvice
;; Version 11 - new email
;; Version 12 - char-displayable-p on emacs25 tty
;; Version 13 - docstrings of what is meant for interal-use

;;; Code:

;; Explicit dependency on advice.el since `unicode-disp-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled, and that
;; macro is not autoloaded.
(require 'advice)

;; quieten xemacs byte compile (though `unicode-disp' does nothing in xemacs
;; as yet)
(defvar standard-display-table)
(defvar buffer-display-table)

;;-----------------------------------------------------------------------------
;; compatibility -- various

(defun unicode-disp--char-displayable-p (char)
  "An internal part of unicode-disp.el.
Return non-nil if CHAR can be shown on the current display.

This is `char-displayable-p' except there was a bug in Emacs 25.1
on the Linux console which meant it reckoned various unicode
chars displayable even when `terminal-coding-system' said
`iso-latin-1-unix'.  This affects all uses of `char-displayable-p'
but is rather fundamental here so as a workaround on a tty demand
also that CHAR is encodable in `terminal-coding-system'.

In Emacs 21 there is no `char-displayable-p' so there it's
assumed (slightly rashly) that everything is displayable on a
window system, and `terminal-coding-system' on a tty."

  (if (and (eval-when-compile (fboundp 'char-displayable-p)) ;; emacs22 up
           window-system)
      (char-displayable-p char) ;; window system

    ;; assumed everything is displayable on X and on a utf8 tty,
    ;; otherwise check for encodability in terminal-coding-system
    (or window-system
        (eq 'utf-8 (terminal-coding-system))
        (equal char ? )
        (not (equal " " (encode-coding-string (char-to-string char)
                                              (terminal-coding-system)))))))
(eval-when-compile
  (put 'unicode-disp--char-displayable-p 'side-effect-free t))

;; unicode-disp--make-glyph-code
(eval-and-compile
  (if (eval-when-compile (fboundp 'make-glyph-code))
      ;; emacs22 up
      (defalias 'unicode-disp--make-glyph-code 'make-glyph-code)
    ;; emacs21
    (eval-when-compile
      (put 'unicode-disp--make-glyph-code 'side-effect-free t))
    (defun unicode-disp--make-glyph-code (c &optional face)
      "An internal part of unicode-disp.el.
Return a glyph code for CHAR displayed with FACE."
      (logior c (* 524288
                   (if face (face-id face) 0))))))

;;-----------------------------------------------------------------------------
;; compatibility -- emacs23 new stuff

;; unicode-disp--with-selected-frame
(eval-and-compile
  (defmacro unicode-disp--with-selected-frame (frame &rest body)
    "An internal part of unicode-disp.el.
This macro does not exist when running byte compiled.
Evaluate BODY with FRAME as the `selected-frame'."
    ;; (declare (debug t))  ;; emacs22,xemacs21, or 'cl

    (if (eval-when-compile (fboundp 'with-selected-frame))
        ;; emacs23 up, and xemacs21
        `(with-selected-frame ,frame ,@body)

      ;; emacs21,emacs22
      ;; the NO-ENTER parameter in emacs21 is gone in emacs22
      `(let ((unicode-disp--with-selected-frame--old (selected-frame)))
         (unwind-protect
             (progn
               (select-frame ,frame)
               ,@body)
           (select-frame unicode-disp--with-selected-frame--old))))))


;;-----------------------------------------------------------------------------

(defun unicode-disp-attr-displayable-p (attr &optional display)
  "An internal part of unicode-disp.el.
Return non-nil if ATTR can be shown on DISPLAY.
ATTR is a keyword like :overline.
DISPLAY is a display name, a frame, or nil for the selected frame."
  (cond ((eval-when-compile (fboundp 'display-supports-face-attributes-p))
         ;; emacs22 up
         (display-supports-face-attributes-p (list attr t) display))
        ((eval-when-compile (boundp 'face-attribute-name-alist))
         ;; emacs21
         (assq attr face-attribute-name-alist))
        ((progn
           (eval-and-compile (require 'cus-face))
           (eval-when-compile (boundp 'custom-face-attributes)))
         ;; xemacs21, emacs20
         ;; suppose this is all possible attributes, or close enough
         (assq attr custom-face-attributes))))

(defun unicode-disp-overline-face (&optional display)
  "An internal part of unicode-disp.el.
Return an overline face (a symbol) for DISPLAY, or nil.
If the display can't show an overline face then return nil.
DISPLAY is a display name, a frame, or nil for the selected frame."
  (when (unicode-disp-attr-displayable-p :overline display)
    (unless (facep 'unicode-disp-overline)
      (make-face 'unicode-disp-overline)
      (set-face-attribute 'unicode-disp-overline nil :overline t))
    'unicode-disp-overline))

;; is `facep' side-effect-free ?
(defun unicode-disp-escape-face ()
  "An internal part of unicode-disp.el.
Return 'escape-glyph if that face exists, otherwise 'default.
DISPLAY is a display name, a frame, or nil for the selected frame."
  (if (facep 'escape-glyph) ;; not in emacs21,xemacs21
      'escape-glyph
    'default))

(defconst unicode-disp-default-character-list
  '(
    (#x2010 "-")  ;; HYPHEN
    (#x2212 "-")  ;; MINUS SIGN
    (#x2013 "-")  ;; EN DASH
    (#x2014 "-")  ;; EM DASH

    (#x2500 "-"   ;; BOX DRAWINGS LIGHT HORIZONTAL
            unicode-disp-escape-face)
    (#x2502 "|")  ;; BOX DRAWINGS LIGHT VERTICAL
    ;;
    (#x250C "+"   ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
            unicode-disp-escape-face)
    (#x2514 "+"   ;; BOX DRAWINGS LIGHT UP AND RIGHT
            unicode-disp-escape-face)

    (#x2018 "`")  ;; LEFT SINGLE QUOTATION MARK
    (#x2019 "'")  ;; RIGHT SINGLE QUOTATION MARK
    (#x201C "\"") ;; LEFT DOUBLE QUOTATION MARK
    (#x201D "\"") ;; RIGHT DOUBLE QUOTATION MARK

    (#x2190 "<-"  ;; LEFTWARDS ARROW
            unicode-disp-escape-face)
    (#x2192 "->"  ;; RIGHTWARDS ARROW
            unicode-disp-escape-face)
    (#x21D0 "<="  ;; LEFTWARDS ARROW
            unicode-disp-escape-face)
    (#x21D2 "=>"  ;; RIGHTWARDS DOUBLE ARROW
            unicode-disp-escape-face)
    (#x221E "inf" ;; INFINITY
            unicode-disp-escape-face)

    (#x25C0 "<"   ;; BLACK LEFT-POINTING TRIANGLE
            unicode-disp-escape-face)
    (#x25B6 ">"   ;; BLACK RIGHT-POINTING TRIANGLE
            unicode-disp-escape-face)

    (#x27E8 "<")  ;; MATHEMATICAL LEFT ANGLE BRACKET
    (#x27E9 ">")  ;; MATHEMATICAL RIGHT ANGLE BRACKET

    (#x2022 "*")  ;; BULLET
    (#x2264 "<="  ;; LESS-THAN OR EQUAL TO
            unicode-disp-escape-face)
    (#x2265 ">="  ;; GREATER-THAN OR EQUAL TO
            unicode-disp-escape-face)

    (#x203E " "   ;; OVERLINE as face
            unicode-disp-overline-face)))

(defun unicode-disp-table (table)
  "An internal part of unicode-disp.el.
Apply unicode display to TABLE.
TABLE is a display table, or nil to act on as-yet uninitialized
`standard-display-table'.  The `selected-frame' is used to check
which characters are displayable."

  (dolist (elem unicode-disp-default-character-list)
    (let* ((char (decode-char 'ucs (nth 0 elem)))
           (str  (nth 1 elem))
           (face (nth 2 elem)))
      (when (and
             ;; table doesn't already have an entry
             (not (and table
                       (aref table char)))
             ;; char not already displayable
             (not (unicode-disp--char-displayable-p char))
             ;; face func returns a face
             (or (not face)
                 (setq face (funcall face))))

        ;; TABLE nil means `standard-display-table', to be initialized by
        ;; loading disp-table.el
        (unless table
          (require 'disp-table)
          (setq table standard-display-table))

        (aset table char
              (vconcat (mapcar (lambda (c)
                                 (unicode-disp--make-glyph-code c face))
                               str)))))))

;; `window-display-table' noticed through `set-window-display-table'
;;
(defadvice set-window-display-table (after unicode-disp activate)
  "Apply `unicode-disp' character fallbacks to `window-display-table'."
  ;; in emacs21 `unload-feature' doesn't call `unicode-disp-unload-function'
  ;; so this advice is left behind, allow for that by checking
  ;; 'unicode-disp-table' still exists
  (when (fboundp 'unicode-disp-table)
    (let ((table (ad-get-arg 1))) ;; args: WINDOW TABLE
      (and table
           (unicode-disp--with-selected-frame (window-frame window)
                                              (unicode-disp-table table))))))

(defun unicode-disp-unload-function ()
  "Remove advice on `set-window-display-table'.
This is called by `unload-feature'."
  (when (ad-find-advice 'set-window-display-table 'after 'unicode-disp)
    (ad-remove-advice   'set-window-display-table 'after 'unicode-disp)
    (ad-activate        'set-window-display-table))
  nil) ;; and do normal unload-feature actions too

;; `buffer-display-table' noticed under `window-configuration-change-hook'
;; this means only displayed buffers are considered, which may help if
;; there's lots of buffers, but basically there's no good way to notice a
;; plain setq to buffer-display-table anyway
;;
(defun unicode-disp-winconf ()
  "An internal part of unicode-disp.el.
Apply `unicode-disp' character fallbacks to `buffer-display-table'.
This function is used in `window-configuration-change-hook' to
check any buffer display tables in the displayed buffers."
  ;; in emacs21 `unload-feature' doesn't remove `unicode-disp-winconf' from
  ;; `window-configuration-change-hook', allow for that by checking
  ;; 'unicode-disp-table' still exists
  (when (fboundp 'unicode-disp-table) ;; in case `unload-feature'
    (dolist (window (window-list nil t))
      (when (window-live-p window) ;; dead windows don't have buffers
        (with-current-buffer (window-buffer window)
          (if buffer-display-table
              (unicode-disp-table buffer-display-table)))))))

;;;###autoload
(defun unicode-disp ()
  "Setup some display table fallbacks for unicode chars.
The display tables are modified to show a few unicode chars as
ascii near-equivalents if not otherwise displayable.  For example
if U+2010 HYPHEN isn't displayable then it's set to plain ascii
\"-\".

This only affects the screen display.  The characters in the
buffers are unchanged.

`standard-display-table' and current and future
`window-display-table' and `buffer-display-table' are acted on.
A new setting for a `buffer-display-table' is only noticed on the
next window configuration change, which is not really right but
usually close enough.

----
The unicode-disp.el home page is
URL `http://user42.tuxfamily.org/unicode-disp/index.html'"

  (interactive)
  (when (eval-when-compile (boundp 'standard-display-table)) ;; not in xemacs
    ;; if different frames have different char-displayable-p then might want
    ;; to mangle the standard table for each to make a lowest denominator,
    ;; but for now just do it once (can be repeated by M-x unicode-disp)
    (unicode-disp-table standard-display-table)

    (dolist (frame (frame-list))
      (unicode-disp--with-selected-frame frame
        ;; initial `window-display-table's
        (dolist (window (window-list frame t))
          (let ((table (window-display-table window)))
            (if table
                (unicode-disp-table table))))
        ;; initial `buffer-display-table's
        (unicode-disp-winconf)))

    ;; future `buffer-display-table's
    (add-hook 'window-configuration-change-hook 'unicode-disp-winconf)))

;; Might have put `unicode-disp' as a customize option on `term-setup-hook'
;; or similar.  But as of Emacs 23 `term-setup-hook' is just a defvar not
;; defcustom.
;;
;; ;;;###autoload
;; (custom-add-option 'term-setup-hook 'unicode-disp)

;; LocalWords: latin overline fallbacks unicode tty disp ascii undisplayable
;; LocalWords: charsets cyrillic iso el

(provide 'unicode-disp)

;;; unicode-disp.el ends here
