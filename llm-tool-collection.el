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

(defmacro llm-tool-collection-deftool (name &rest specs)
  "Declare a generic LLM tool named NAME.
The remaining arguments SPECS should be a plist specifying the standard
attributes of an LLM tool."
  (declare (indent defun))
  `(defconst ,(intern (concat "llm-tool-collection-tool-"
                              (string-replace "_" "-" name)))
     (:name ,name
            ,@specs)))

(llm-tool-collection-deftool "read_file"
  :description "Read the contents of a file"
  :args (list '(:name "path"
                      :type "string"
                      :description "Path to the file to read. Supports relative paths and ~."))
  :category "filesystem"
  :include t
  :confirm t
  :function (lambda (path)
              (with-temp-buffer
                (insert-file-contents (expand-file-name path))
                (buffer-string))))

(provide 'llm-tool-collection)

;;; llm-tool-collection.el ends here
