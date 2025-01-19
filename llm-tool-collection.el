;;; llm-tool-collection.el --- Crowdsourced tools for LLMs -*- lexical-binding: t -*-

;; Author: Ad
;; Maintainer: Ad
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

(defvar llm-tool-collection--all-tools nil
  "A list of all tool definition symbols.")

(defun llm-tool-collection--name-to-symbol (name)
  "Convert tool NAME into a namespaced symbol."
  (intern (concat "llm-tool-collection-tool-"
                  (string-replace "_" "-" name))))

(defmacro llm-tool-collection-deftool (name &rest specs)
  "Declare a generic LLM tool named NAME.
The remaining arguments SPECS should be a plist specifying the standard
attributes of an LLM tool."
  (declare (indent defun))
  (let ((sym (llm-tool-collection--name-to-symbol name)))
    `(progn
       (defconst ,sym
         '(:name ,name
                 ,@specs))
       (push ',sym llm-tool-collection--all-tools))))

(defun llm-tool-collection-get (name)
  "Return the specification for tool NAME.
NAME should be the string name given in the tool specification."
  (let ((sym (llm-tool-collection--name-to-symbol name)))
    (symbol-value sym)))

(defun llm-tool-collection-get-all ()
  "Return a list of all tool definitions in the collection.

Mapping over this list with `gptel-make-tool', `llm-make-tool', or
similar will add all tools to the respective client:

 (mapcar (apply-partially #'apply #'gptel-make-tool)
         (llm-tool-collection-get-all))"
  (mapcar #'symbol-value llm-tool-collection--all-tools))

(llm-tool-collection-deftool "read_file"
  :description "Read the contents of a file"
  :args (list '(:name "path"
                      :type "string"
                      :description "Path to the file to read. Supports relative paths and ~."))
  :function (lambda (path)
              (with-temp-buffer
                (insert-file-contents (expand-file-name path))
                (buffer-string))))

(provide 'llm-tool-collection)

;;; llm-tool-collection.el ends here
