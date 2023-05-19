;;; eff-mode.el --- Show symbols in Executable File Formats -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel
;; Copyright (C) 2020-2023 Michael Krasnyk

;; Author: Oleh Krehel <ohwoeowho@gmail.com>, Michael Krasnyk <michael.krasnyk@gmail.com>
;; URL: https://github.com/oxidase/eff-mode
;; Package-Requires: ((emacs "28"))
;; Version: 1.0
;; Keywords: elf readelf convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Toggle `eff-mode' to show the symbols that the binary uses instead
;; of the actual binary contents.
;;
;; References:
;;    https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#Specifications

;;; Code:

; (eval-when-compile (require 'subr-x))


;; Customizable variables
(defgroup eff-mode nil "EFF mode customizable variables.")

(defcustom eff-mode-md5sum "md5sum"
  "MD5 sum executable name or path."
  :type 'string
  :group 'eff-mode)

(defcustom eff-mode-readelf "readelf"
   "The readelf executable name or path."
  :type 'string
  :group 'eff-mode)

(defcustom eff-mode-strings "strings"
  "The strings executable name or path."
  :type 'string
  :group 'eff-mode)

(defcustom eff-mode-buffer-initial-type 'dynamic
  "The initial state of an ELF buffer."
  :type 'symbol
  :group 'eff-mode)

(defcustom eff-mode-use-local-toolchain t
  "Use a toolchain on local or remote machine.

  TODO: Using toolchains remotely is not implemented, required commit
  https://github.com/emacs-mirror/emacs/commit/83b1db043b44a8efb091ced873eab686e671c5ac"
  :type 'boolean
  :group 'eff-mode)

(defcustom eff-mode-gdb-alist `(("00b7" . "aarch64-linux-gnu-gdb")
                                ("0028" . "arm-none-eabi-gdb"))
  "The gdb binary used by used by the corresponding platform.

Each element has the form (E_MACHINE . GDB).
 E_MACHINE is a elf file header e_machine value, as a string in lower case.
 GDB pecifies the gdb command for corresponding platform."
  :type '(alist :key-type (string :tag "e_machine id")
          :value-type (string :tag "gdb executable"))
  :group 'eff-mode)


;; Faces
(defface eff-mode-disassemble-hex
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the disassembled hex values.")

(defface eff-mode-disassemble-opcode
  '((((class color) (background light)) :foreground "blue"))
  "Face for the disassembled opcode values.")


;; Local variables
(defvar-local eff-mode-buffer-type eff-mode-buffer-initial-type)

(defvar-local eff-mode-gdb-executable "gdb")

(defvar-local eff-mode-disassemble-command
  "%s -n -q -batch -ex 'file %s' -ex 'disassemble/rs %s'")

(defvar-local eff-mode-binary-command
  "dd status=none bs=1 skip=%s count=%s if=%s")

(defvar-local asm-comment-char ?#)


;; Constants
(defconst eff-mode-buffer-types
  `((arch-specific   . ((key . "A") (command . ,(append (split-string eff-mode-readelf) '("-W" "--arch-specific")))))
    (archive-index   . ((key . "c") (command . ,(append (split-string eff-mode-readelf) '("-W" "--archive-index")))))
    (dynamic         . ((key . "d") (command . ,(append (split-string eff-mode-readelf) '("-W" "--dynamic")))))
    (headers         . ((key . "e") (command . ,(append (split-string eff-mode-readelf) '("-W" "--headers")))))
    (section-groups  . ((key . "G") (command . ,(append (split-string eff-mode-readelf) '("-W" "--section-groups")))))
    (header          . ((key . "h") (command . ,(append (split-string eff-mode-readelf) '("-W" "--file-header")))))
    (histogram       . ((key . "I") (command . ,(append (split-string eff-mode-readelf) '("-W" "--histogram")))))
    (program-headers . ((key . "l") (command . ,(append (split-string eff-mode-readelf) '("-W" "--program-headers")))))
    (md5sum          . ((key . "m") (command . ,(split-string eff-mode-md5sum))))
    (notes           . ((key . "n") (command . ,(append (split-string eff-mode-readelf) '("-W" "--notes")))))
    (relocs          . ((key . "r") (command . ,(append (split-string eff-mode-readelf) '("-W" "--relocs")))))
    (section-headers . ((key . "S") (command . ,(append (split-string eff-mode-readelf) '("-W" "--section-headers")))))
    (symbols         . ((key . "s") (command . ,(append (split-string eff-mode-readelf) '("-W" "--symbols")))))
    (unwind          . ((key . "u") (command . ,(append (split-string eff-mode-readelf) '("-W" "--unwind")))))
    (version-info    . ((key . "V") (command . ,(append (split-string eff-mode-readelf) '("-W" "--version-info")))))
    (dyn-syms        . ((key . "x") (command . ,(append (split-string eff-mode-readelf) '("-W" "--dyn-syms")))))
    (strings         . ((key . "z") (command . ,(split-string eff-mode-strings))))
))


;;
(defun eff-mode-buffer-file-name ()
  "Get local file name of a buffer."
  (cond
   ((and eff-mode-use-local-toolchain (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p (buffer-file-name)))
    (tramp-handle-file-local-copy (buffer-file-name)))
   (t (buffer-file-name))))

(defun elf-add-func-refs ()
  "Add references to disassemble commands for FUNC entries."
  (goto-char (point-min))
  (while (re-search-forward "FUNC[[:space:]]+\\([[:alnum:]]+[[:space:]]+\\)\\{3\\}\\(\\sw+\\)" nil t)
    (let* ((name (match-string 2))
           (ol (make-button
                (match-beginning 2) (match-end 2)
                'help-echo (format "disassemble %s" name)
                'action #'eff-mode-disassemble
                'mouse-action #'eff-mode-disassemble)))
      (overlay-put ol 'symbol name))))

(defun elf-add-sections-refs ()
  "Add references to hexl commands for header sections."
  (goto-char (point-min))
  (while (re-search-forward "^ *\\[ *[[:digit:]]+] +\\([^ ]+\\) +[^ ]+ +[^ ]+ +\\([^ ]+\\) +\\([^ ]+\\)" nil t)
    (when (string-prefix-p "." (match-string 1))
      (let* ((name (match-string 1))
             (offset (string-to-number (match-string 2) 16))
             (size (string-to-number (match-string 3) 16))
             (ol (make-button
                  (match-beginning 1) (match-end 1)
                  'help-echo (format "hexl %s" (match-string 1))
                  'action #'eff-mode-binary
                  'mouse-action #'eff-mode-binary)))
        (overlay-put ol 'section name)
        (overlay-put ol 'offset offset)
        (overlay-put ol 'size size)))))

(defun elf-revert-buffer ()
  "Revert buffer with ELF-specific modes."
  (interactive)
  (when (eq 'eff-mode major-mode)
    (let* ((state (cdr (assoc eff-mode-buffer-type eff-mode-buffer-types)))
           (command (cdr (assoc 'command state)))
           (stdout (current-buffer))
           (inhibit-read-only t)
           (stderr (generate-new-buffer "*readelf stderr*"))
           (file-name (eff-mode-buffer-file-name))
           (default-directory (file-name-directory file-name)))
      (setf (buffer-string) "")
      (let* ((get_e_machine_command
              (format "hexdump -e '1/2 \"%s\"' -s 0x12 -n 2 %s" "%04x" (file-name-nondirectory file-name)))
             (e_machine (string-trim (shell-command-to-string get_e_machine_command)))
             (gdb (assoc e_machine eff-mode-gdb-alist)))
        (if gdb (setq-local eff-mode-gdb-executable (cdr gdb))))

      (make-process ;; TODO: switch to tramp-handle-make-process
       :name "readelf"
       :buffer nil
       :stderr stderr
       :noquery t
       :command (append command `(,(file-name-nondirectory file-name)))
       :filter
       (lambda (_proc msg)
         (when (buffer-live-p stdout)
           (with-current-buffer stdout
             (setq-local inhibit-read-only t)
             (goto-char (point-max))
             (insert msg))))
       :sentinel
       (lambda (_proc event)
         (with-current-buffer stdout
           (when (string= event "finished\n")
             (cond
               ((and (not (string= "a" (file-name-extension file-name))) ;; TODO: add offsets for *.o files shown by "ar tO"
                 (or (eq eff-mode-buffer-type 'headers)
                     (eq eff-mode-buffer-type 'section-headers)))
                (elf-add-sections-refs))
               ((or (eq eff-mode-buffer-type 'dyn-syms)
                    (eq eff-mode-buffer-type 'symbols))
                (elf-add-func-refs))))
           (goto-char (point-min))
           (set-buffer-modified-p nil)
           (read-only-mode))
         (with-current-buffer stderr
           (let ((err (string-trim (buffer-string))))
             (unless (string= "" err)
               (message "eff-mode: %s\n%s" event err))))
         (kill-buffer stderr))))))

(defun eff-mode-disassemble (overlay)
  "Mode for disassembled code for a symbols OVERLAY."
  (let* ((symbol (overlay-get overlay 'symbol))
         (buffer-name (format "%s(%s)" (buffer-name) symbol))
         (file-name (eff-mode-buffer-file-name))
         (default-directory (file-name-directory file-name))
         (command (format eff-mode-disassemble-command eff-mode-gdb-executable (file-name-nondirectory file-name) symbol)))
    (with-current-buffer (pop-to-buffer buffer-name)
      (shell-command command (current-buffer))
      (flush-lines "^[[:space:]]*$" (point-min) (point-max))
      (set-buffer-modified-p nil)
      (asm-mode)
      (while (re-search-forward "\\(<\\+[[:digit:]]+>:\\)\t\\([a-f0-9 ]+\\)\t\\([a-zA-Z0-9]+\\)" nil t)
        (let* ((offset (concat (match-string 1) (make-string (+ 1 (max 0 (- 8 (length (match-string 1))))) ? )))
               (hex (concat (match-string 2) (make-string (+ 1 (max 0 (- 32 (length (match-string 2))))) ? )))
               (opcode (match-string 3))
               (beg1 (match-beginning 1))
               (beg2 (+ beg1 (length offset)))
               (beg3 (+ beg2 (length hex)))
               (end3 (+ beg3 (length opcode))))
          (replace-match (concat offset hex opcode))
          (put-text-property beg2 beg3 'font-lock-face 'eff-mode-disassemble-hex)
          (put-text-property beg3 end3 'font-lock-face 'eff-mode-disassemble-opcode)))
      (setq-local asm-comment-char ?#)
      (read-only-mode))))

(defun eff-mode-binary (overlay)
  "Mode for hex dump for a sections OVERLAY."
  (let* ((section (overlay-get overlay 'section))
         (offset (overlay-get overlay 'offset))
         (size (overlay-get overlay 'size))
         (buffer-name (format "%s(%s)" (buffer-name) section))
         (file-name (eff-mode-buffer-file-name))
         (default-directory (file-name-directory file-name))
         (command (format eff-mode-binary-command offset size (file-name-nondirectory file-name))))
    (with-current-buffer (pop-to-buffer buffer-name)
      (setq buffer-file-coding-system nil)
      (shell-command command (current-buffer))
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil)
      (hexl-mode)
      (read-only-mode))))

(defconst eff-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st))

(setq eff-mode-map (make-keymap))
(suppress-keymap eff-mode-map)

(defmacro eff-mode-create-key-binding (arg)
  "Create a mode key function for ARG state."
  `(defun ,(intern (concat "eff-mode-" arg)) ()
     "Macro-generated key-binding function that changes buffer type"
     (interactive)
     (setq eff-mode-buffer-type (intern ,arg))
     (elf-revert-buffer)))

(mapcar
 (lambda (x)
   (let* ((key (cdr (assoc 'key x)))
          (state-name (car x)))
     (eval (macroexpand-1 `(eff-mode-create-key-binding ,(symbol-name state-name))))
     (define-key eff-mode-map key (intern (concat "eff-mode-" (symbol-name state-name))))
     ))
 eff-mode-buffer-types)


;;;###autoload
(define-derived-mode eff-mode special-mode "Elf"
  :syntax-table eff-mode-syntax-table
  (buffer-disable-undo)
  (elf-revert-buffer))

;;;###autoload
(add-to-list 'magic-mode-alist '("^\177ELF" . eff-mode))

(add-to-list 'magic-mode-alist '("^!<arch>\012/       " . eff-mode))

(provide 'eff-mode)

;;; eff-mode.el ends here
