;;; early-init.el --- Early Init File  -*- lexical-binding: t; -*-

;; Copyright 2024 ZharMeny

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; early-init.el was introduced in Emacs 27.1.  This file is loaded
;; before the package system and GUI is initialized and before site
;; files are loaded, so in it you can customize variables that affect
;; the package initialization process.

;;; Code:

(setq package-enable-at-startup nil)

;;; early-init.el ends here
