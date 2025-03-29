;;; llm-tool-collection.el --- Crowdsourced tools for LLMs -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/skissue/llm-tool-collection
;; Keywords: tools, convenience


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Crowdsourced collection of tools for LLMs in Emacs

;;; Code:

(require 'seq)
(require 'cl-lib)

(defvar llm-tool-collection--all-tools nil
  "A list of all tool definition symbols.")

(eval-and-compile
  (defun llm-tool-collection--name-to-symbol (name)
    "Convert tool NAME into a namespaced symbol by prepending `llm-tc/'."
    (intern (concat "llm-tc/" (symbol-name name))))

  (defun llm-tool-collection--make-llm-name (name)
    "Replace dashes with underscores to make tool NAME LLM-friendly."
    (string-replace "-" "_" (symbol-name name))))

(defvar llm-tool-collection-post-define-functions nil
  "Functions called after defining a new LLM tool.
Each function is called with one argument, the tool's plist definition.")

(defmacro llm-tool-collection-deftool (name specs args description &rest body)
  "Declare a generic LLM tool named NAME.

SPECS should be a plist specifying the standard attributes of an LLM
tool:

- :name.  The LLM-friendly name for the tool.  If not set, the NAME
  argument (with dashes replaced with underscores) will be used by
  default.

- :category.  Required. A string categorizing the tool, such as
  \"filesystem\", \"buffers\", \"system\".

- :tags.  A list of symbols for tagging the tool to enable more precise
  filtering.  These can be arbitrary symbols, such as `buffers',
  `introspection', `programming', `editing'.

SPECS may also contain other extra keywords used by specific clients.
Conformant clients should ignore all unsupported keywords.  Recommended
examples:

- :confirm.  Boolean flag to indicate whether user confirmation should be
  requested before executing the tool (used by `gptel').

- :include.  Boolean flag to indicate whether the tool result should be
  included as part of the LLM output (used by `gptel').

ARGS is a list where each element is of the form

  (ARGNAME \"DESCRIPTION\" :type TYPE [...]).

Arguments after the special symbol `&optional' are marked as optional.
TYPE and further properties [...] can include:

- :type.  Required.  One of the symbols string, number, integer, boolean,
  array, object, or null.

- :enum.  For enumerated types, a vector of strings representing allowed
  values.  Note that :type is still required even with enums.

- :items.  Required if :type is array.  Must be a plist including at least
  the item's :type.

- :properties.  Required if :type is object.  Must be a plist that can be
  serialized into a JSON object specification via `json-serialize', with
  the exception that :type specifications in this plist must be symbols.

- :required.  For object types, a vector of strings listing required
  object keys.

For example, a weather tool might have ARGS defined as:

  ((location \"The city and state, e.g. San Francisco, CA\" :type string)
   &optional
   (unit \"The unit of temperature, either \\='celsius\\=' or \\='fahrenheit\\='\"
         :type string
         :enum [\"celsius\" \"fahrenheit\"]))

This would translate to a tool specification, in the sense described at
the URL
`https://github.com/ahyatt/llm/discussions/124#discussioncomment-11877109',
with args:

  ((:name \"location\"
    :type string
    :description \"The city and state, e.g. San Francisco, CA\")
   (:name \"unit\"
    :type string
    :enum [\"celsius\" \"fahrenheit\"]
    :description \"The unit of temperature, either \\='celsius\\=' or \\='fahrenheit\\='\"
    :optional t))

DESCRIPTION is the tool's documentation string.

BODY contains the function body.

This macro defines a constant with the tool's specs and a function whose
docstring is DESCRIPTION with the tool's body under `llm-tc/NAME'. After
the tool is defined, it is additionally made available via
`llm-tool-collection-get-all' and `llm-tool-collection-get-category',
and all functions in `llm-tool-collection-post-define-functions' are
called with the tool's spec as their argument."
  (declare (indent defun)
           (debug (&define symbolp sexp sexp stringp def-body)))
  (let* ((optional nil)
         (arg-syms '())
         (arg-specs '()))
    (dolist (arg args)
      (if (eq arg '&optional)
          (progn
            (setq optional t)
            (push arg arg-syms))
        (let ((argname (car arg))
              (argdesc (cadr arg))
              (argrest (cddr arg)))
          (push argname arg-syms)
          (push `(:name ,(llm-tool-collection--make-llm-name argname)
                        :description ,argdesc
                        ,@(when optional '(:optional t))
                        ,@argrest)
                arg-specs))))
    (setq arg-syms (reverse arg-syms)
          arg-specs (reverse arg-specs))
    (let* ((sym (llm-tool-collection--name-to-symbol name))
           (name-spec (unless (plist-get specs :name)
                        `(:name ,(llm-tool-collection--make-llm-name name)))))
      `(progn
         (defconst ,sym
           '(,@name-spec
             :description ,description
             ,@specs
             :args ,arg-specs
             :function ,sym))
         (defun ,sym ,arg-syms
           ,(concat description "\n\n"
                    "Definition generated by `llm-tool-collection'.")
           ,@body)
         (cl-pushnew ',sym llm-tool-collection--all-tools)
         (run-hook-with-args
          'llm-tool-collection-post-define-functions (symbol-value ',sym))))))

;;;###autoload
(defun llm-tool-collection-get-category (category)
  "Return a list of all tool definitions in the collection part of CATEGORY.

Mapping over this list with `gptel-make-tool', `llm-make-tool', or
similar will add all tools to the respective client:

 (mapcar (apply-partially #\\='apply #\\='gptel-make-tool)
         (llm-tool-collection-get-category \"filesystem\"))"
  (seq-filter (lambda (tool) (string= (plist-get tool :category) category))
              (llm-tool-collection-get-all)))

;;;###autoload
(defun llm-tool-collection-get-tag (tag)
  "Return a list of all tool definitions in the collection tagged with TAG.

Mapping over this list with `gptel-make-tool', `llm-make-tool', or
similar will add all tools to the respective client:

 (mapcar (apply-partially #\\='apply #\\='gptel-make-tool)
         (llm-tool-collection-get-tag \\='buffer))"
  (seq-filter (lambda (tool) (memq tag (plist-get tool :tags)))
              (llm-tool-collection-get-all)))

;;;###autoload
(defun llm-tool-collection-get-all ()
  "Return a list of all tool definitions in the collection.

Mapping over this list with `gptel-make-tool', `llm-make-tool', or
similar will add all tools to the respective client:

 (mapcar (apply-partially #\\='apply #\\='gptel-make-tool)
         (llm-tool-collection-get-all))"
  (mapcar #'symbol-value llm-tool-collection--all-tools))

;;; Imenu

;;;###autoload
(cl-pushnew (list "LLM Tools"
                  (concat "^\\s-*("
                          (regexp-opt '("llm-tool-collection-deftool") t)
                          "\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                  2)
            lisp-imenu-generic-expression :test #'equal)

;;; Font-Lock

;;;###autoload
(defconst llm-tool-collection-font-lock-keywords
  '(("(\\(llm-tool-collection-deftool\\)\\_>[ \t'(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face nil t))))

;;;###autoload
(font-lock-add-keywords 'emacs-lisp-mode llm-tool-collection-font-lock-keywords)

;;; Tools

(llm-tool-collection-deftool read-file
  (:category "filesystem" :tags (filesystem editing) :confirm t :include t)
  ((path "Path to the file to read. Supports relative paths and '~'."
         :type string))
  "Read the contents of a file and return its content as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name path))
    (buffer-string)))

(llm-tool-collection-deftool list-directory
  (:category "filesystem" :tags (filesystem) :confirm t :include t)
  ((path "Path to the directory. Supports relative paths and '~'."
         :type string))
  "List the contents of a specified directory."
  (let ((expanded-path (expand-file-name path)))
    (if (file-directory-p expanded-path)
        (string-join `(,(format "Contents of %s:" path)
                       ,@(directory-files expanded-path))
                     "\n")
      (error "%s is not a directory" expanded-path))))

(llm-tool-collection-deftool create-file
  (:category "filesystem" :tags (filesystem editing) :confirm t)
  ((path "Path to the new file. Supports relative paths and '~'." :type string)
   (content "Content to write to the file." :type string))
  "Create a new file with the specified content if it does not already exist."
  (let ((expanded-path (expand-file-name path)))
    (if (file-exists-p expanded-path)
        (error "File already exists: %s" expanded-path)
      (with-temp-file expanded-path
        (insert content))
      (format "File created successfully: %s" path))))

(llm-tool-collection-deftool create-directory
  (:category "filesystem" :tags (filesystem) :confirm t)
  ((path "Path to the new directory. Supports relative paths and '~'."
         :type string))
  "Create a new directory at the specified path if it does not already
exist."
  (let ((expanded-path (expand-file-name path)))
    (if (file-exists-p expanded-path)
        (error "Directory already exists: %s" expanded-path)
      (make-directory expanded-path t)
      (format "Directory created successfully: %s" path))))

(llm-tool-collection-deftool view-buffer
  (:category "buffers" :tags (buffers editing))
  ((buffer-name "Name of the buffer to view." :type string)
   &optional
   (offset "Line number to start reading from (0-based)." :type integer)
   (limit "Maximum number of lines to return." :type integer))
  "View contents of BUFFER-NAME with optional OFFSET and LIMIT.
OFFSET specifies the starting line (0-based).
LIMIT specifies the maximum number of lines to return."
  (with-current-buffer buffer-name
    (let* ((lines (split-string (buffer-string) "\n"))
           (total-lines (length lines))
           (start (min (or offset 0) total-lines))
           (end (min (+ start (or limit total-lines)) total-lines))
           (selected-lines (seq-subseq lines start end)))
      (string-join selected-lines "\n"))))

(llm-tool-collection-deftool edit-buffer
  (:category "buffers" :tags (buffers editing))
  ((buffer-name "Name of the buffer to modify" :type string)
   (old-string "Text to replace (must match exactly)" :type string)
   (new-string "Text to replace old_string with" :type string))
  "Edits Emacs buffers"
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (if (= count 0)
              (format
               "Error: Could not find text to replace in buffer %s" buffer-name)
            (if (> count 1)
                (format
                 "Error: Found %d matches for the text to replace in buffer %s"
                 count buffer-name)
              (goto-char (point-min))
              (search-forward old-string)
              (replace-match new-string t t)
              (format "Successfully edited buffer %s" buffer-name))))))))

(provide 'llm-tool-collection)

;;; llm-tool-collection.el ends here
