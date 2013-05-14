;;; -*- lexical-binding: t -*-
;;; jsinfo.el --- Minor mode to help editing JavaScript programs

;; Copyright (C) 2013  Mihai Bazon

;; Author: Mihai Bazon <mihai.bazon@gmail.com>
;; Keywords: languages, convenience

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

;;

;;; Code:

(require 'cl)
(require 'json)

(defvar *jsinfo-base-directory*
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name)))

(defvar *jsinfo-query-script*
  (expand-file-name "jsinfo.js" *jsinfo-base-directory*))

(add-to-list 'load-path *jsinfo-base-directory*)

(defvar *jsinfo-current-places* nil)
(defvar *jsinfo-current-origin* nil)
(defvar *jsinfo-current-info* nil)
(defvar *jsinfo-extend-region-undo* nil)

(make-variable-buffer-local '*jsinfo-current-info*)
(make-variable-buffer-local '*jsinfo-current-origin*)
(make-variable-buffer-local '*jsinfo-current-places*)
(make-variable-buffer-local '*jsinfo-extend-region-undo*)

(defun jsinfo-places-or-die ()
  (unless *jsinfo-current-places*
    (error "No places highlighted"))
  *jsinfo-current-places*)

(defun %jsinfo-prop (obj names)
  (if names
      (%jsinfo-prop (cdr (assq (car names) obj)) (cdr names))
    obj))

(defmacro jsinfo-prop (&rest names)
  `(progn
     (unless *jsinfo-current-info*
       (error "No places highlighted"))
     (%jsinfo-prop *jsinfo-current-info* ',names)))

(defun jsinfo-query (pos)
  (let* ((output (generate-new-buffer "*jsinfo.js*"))
         (json (save-restriction
                 (widen)
                 (call-process-region (point-min) (point-max)
                                      *jsinfo-query-script*
                                      nil (list output nil) nil
                                      (format "%d" pos)))))
    (unwind-protect
        (progn
          (setq *jsinfo-current-info*
                (with-current-buffer output
                  (beginning-of-buffer)
                  (let ((json-array-type 'list))
                    (json-read))))
          (setq *jsinfo-current-origin* (jsinfo-prop origin))
          (when (jsinfo-prop parse-error)
            (let ((line (jsinfo-prop parse-error line))
                  (col (jsinfo-prop parse-error col))
                  (pos (jsinfo-prop parse-error pos))
                  (message (jsinfo-prop parse-error message)))
              (push-mark (point))
              (goto-char pos)
              (message "Parse error [%d,%d]: %s" line col message))))
      (kill-buffer output))))

(defun jsinfo-forgetit ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'jsinfo-highlight-symbol t)
  (setq *jsinfo-current-info* nil
        *jsinfo-current-places* nil
        *jsinfo-current-origin* nil
        *jsinfo-extend-region-undo* nil)
  (%jsinfo-hl-mode 0))

(defun %jsinfo-extend-region-cleanup ()
  (jsinfo-forgetit)
  (remove-hook 'deactivate-mark-hook '%jsinfo-extend-region-cleanup t))

(defun jsinfo-extend-region-undo ()
  (interactive)
  (if *jsinfo-extend-region-undo*
      (let ((x (pop *jsinfo-extend-region-undo*)))
        (goto-char (car x))
        (push-mark (cdr x) t t))
    (message "No undo available")))

(defun %jsinfo-extend-region (prop begin end)
  (unless (use-region-p)
    (setq begin (point)
          end begin))
  (push (cons begin end) *jsinfo-extend-region-undo*)
  (unless *jsinfo-current-info*
    (jsinfo-query begin))
  (let ((path (%jsinfo-prop *jsinfo-current-info* prop)))
    (when path
      (add-hook 'deactivate-mark-hook '%jsinfo-extend-region-cleanup t t)
      (catch 'done
        (dolist (i (reverse path))
          (let ((x (cdr (assq 'begin i)))
                (y (cdr (assq 'end i))))
            (when (or (< x begin)
                      (> y end))
              (goto-char x)
              (push-mark y t t)
              (message "Node: %s" (cdr (assq 'type i)) x y)
              (throw 'done nil))))))))

(defun jsinfo-extend-region-node (begin end)
  (interactive "r")
  (%jsinfo-extend-region '(path) begin end))

(defun jsinfo-extend-region-statement (begin end)
  (interactive "r")
  (%jsinfo-extend-region '(stat) begin end))

(defun jsinfo-rename-symbol (pos new-name)
  (interactive "d\nsNew name: ")
  (let ((places (sort (copy-list (append (jsinfo-prop references)
                                         (jsinfo-prop definition)))
                      (lambda (a b)
                        (< (cdr (assq 'begin b))
                           (cdr (assq 'begin a)))))))
    (save-excursion
      (dolist (p places)
        (let ((begin (cdr (assq 'begin p)))
              (end (cdr (assq 'end p))))
          (message "Changing %d-%d" begin end)
          (delete-region begin end)
          (goto-char begin)
          (insert new-name)))
      (message "%d occurrences renamed to %s" (length places) new-name))
    (jsinfo-forgetit)))

(defun jsinfo-goto-next-symbol ()
  (interactive)
  (catch 'done
    (dolist (i (jsinfo-places-or-die))
      (let ((x (cdr (assq 'begin i))))
        (when (> x (point))
          (goto-char x)
          (throw 'done nil))))))

(defun jsinfo-goto-prev-symbol ()
  (interactive)
  (catch 'done
    (dolist (i (reverse (jsinfo-places-or-die)))
      (when (< (cdr (assq 'end i)) (point))
        (goto-char (cdr (assq 'begin i)))
        (throw 'done nil)))))

(defun jsinfo-goto-definition (pos)
  (interactive "d")
  (unless *jsinfo-current-origin*
    (jsinfo-highlight-symbol pos))
  (let* ((defs (jsinfo-prop definition))
         (def (car defs))
         (begin (cdr (assq 'begin def)))
         (end (cdr (assq 'end def))))
    (if begin
        (goto-char begin)
      (message "Definition not found"))))

(defun jsinfo-highlight-symbol (pos)
  (interactive "d")
  (jsinfo-forgetit)
  (jsinfo-query pos)
  (setq *jsinfo-current-places*
        (sort (copy-list (append (jsinfo-prop references)
                                 (jsinfo-prop definition)))
              (lambda (a b)
                (< (cdr (assq 'begin a))
                   (cdr (assq 'begin b))))))
  (let* ((places *jsinfo-current-places*))
    (when places
      (loop for ref in places
            for beg = (cdr (assq 'begin ref))
            for end = (cdr (assq 'end ref))
            do (let ((ovl (make-overlay beg end)))
                 (overlay-put ovl 'face 'highlight)
                 (overlay-put ovl 'evaporate t)
                 (overlay-put ovl 'jsinfo-highlight-symbol t)))
      (message "%d occurrences found" (length places))
      (%jsinfo-hl-mode 1))))

(define-minor-mode jsinfo-mode
  "Enables some JS editing goodies via an external tool based on UglifyJS"
  ;; initially disabled
  nil
  ;; modeline
  " JSinfo"
  ;; keymap
  `(
    (,(kbd "M-?") . jsinfo-highlight-symbol)
    (,(kbd "M-.") . jsinfo-goto-definition)
    (,(kbd "C-c <left>") . jsinfo-extend-region-node)
    (,(kbd "C-c C-<left>") . jsinfo-extend-region-node)
    (,(kbd "C-c <up>") . jsinfo-extend-region-statement)
    (,(kbd "C-c C-<up>") . jsinfo-extend-region-statement)
    (,(kbd "C-c <down>") . jsinfo-extend-region-undo)
    (,(kbd "C-c C-<down>") . jsinfo-extend-region-undo)
    ))

(defun %jsinfo-hl-mode-onchange (begin end)
  (jsinfo-forgetit))

(define-minor-mode %jsinfo-hl-mode
  "Internal mode used by `jsinfo-mode'"
  nil
  nil
  `(
    (,(kbd "C-<down>") . jsinfo-goto-next-symbol)
    (,(kbd "C-<right>") . jsinfo-goto-next-symbol)
    (,(kbd "C-<up>") . jsinfo-goto-prev-symbol)
    (,(kbd "C-<left>") . jsinfo-goto-prev-symbol)
    (,(kbd "C-<return>") . jsinfo-rename-symbol)
    (,(kbd "ESC") . jsinfo-forgetit)
    (,(kbd "C-g") . jsinfo-forgetit)
    )

  (cond
   (%jsinfo-hl-mode                     ; activated
    (add-hook 'before-change-functions '%jsinfo-hl-mode-onchange t t))

   (t                                   ; disabled
    (remove-hook 'before-change-functions '%jsinfo-hl-mode-onchange t))))

(add-hook 'js-mode-hook 'jsinfo-mode)

(provide 'jsinfo)
;;; jsinfo.el ends here
