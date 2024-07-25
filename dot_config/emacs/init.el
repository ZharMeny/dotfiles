;;; init.el --- ZharMeny's init.el  -*- lexical-binding: t; -*-

;; Copyright 2024 ZharMeny

;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/ZharMeny/dotfiles

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

;; The init file is where all the magic happens: it is loaded when Emacs
;; is started.  Emacs looks for the init file using the filenames
;; ~/.emacs.el, ~/.emacs, or ~/.emacs.d/init.el, in that order.
;; Additionally, Emacs can look in an XDG-compatible location for
;; init.el, the default is the directory ~/.config/emacs.

;;; Code:

;;;; Core packages

(use-package cc-vars
  :custom
  (c-default-style
   '((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux"))))

(use-package display-line-numbers
  :hook (conf-mode prog-mode text-mode))

(use-package elec-pair
  :custom (electric-pair-mode t))

(use-package emacs
  :no-require
  :custom
  (enable-recursive-minibuffers t)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (truncate-lines t)
  (visible-bell t))

(use-package faces
  :config
  (set-face-attribute 'default nil :height 120)
  (set-face-font 'default "Iosevka")
  (set-face-font 'variable-pitch "Iosevka Aile"))

(use-package flymake
  :config
  (remove-hook
   'flymake-diagnostic-functions
   'flymake-proc-legacy-flymake)
  :hook prog-mode)

(use-package holidays
  :custom
  (holiday-general-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-christian-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil)
  (holiday-other-holidays
   '((holiday-fixed 4 15 "Steal Something from Work Day")
     (holiday-fixed 5 1 "International Workers' Day")
     (holiday-fixed 6 1 "Pride Month")
     (holiday-fixed 6 28 "Stonewall Riots Anniversary")
     ;; Discordian holidays
     (holiday-fixed 1 1 "Nude Year's Day")
     (holiday-fixed 1 5 "Mungday")
     (holiday-fixed 1 10 "demrofeR, yaD sdrawkcaB")
     (holiday-fixed 1 18 "Pat Pineapple Day")
     (holiday-fixed 1 21 "Hug Day")
     (holiday-fixed 2 18 "The Mary Day")
     (holiday-fixed 2 19 "Chaoflux")
     (holiday-fixed 2 20 "Pet Loving Day")
     (holiday-fixed 2 29 "St. Tib's Day")
     (holiday-fixed 3 10 "Head Chicken/Chicken Head Day")
     (holiday-fixed 3 19 "Mojoday")
     (holiday-fixed 3 25 "Love Your Neighbour Day")
     (holiday-fixed 4 1 "April Fool's Day")
     (holiday-fixed 4 2 "St. John the Blasphemist Day")
     (holiday-fixed 4 6 "Jake Day")
     (holiday-fixed 5 3 "Discoflux")
     (holiday-fixed 5 23 "Jake Day Jr.")
     (holiday-fixed 5 25 "Towel Day")
     (holiday-fixed 5 31 "Syaday")
     (holiday-fixed 5 37 "537 Day")
     (holiday-fixed 6 10 "Mad Hatter Day")
     (holiday-fixed 6 21 "Imaginary Friend Day")
     (holiday-fixed 7 2 "Mid Year's Day")
     (holiday-fixed 7 15 "Confuflux")
     (holiday-fixed 8 10 "Multiversal Underwear Day")
     (holiday-fixed 8 12 "Zaraday")
     (holiday-fixed 8 25 "Festival of Hanky-Panky Spankies")
     (holiday-fixed 9 9 "Cat Dancing Day")
     (holiday-fixed 9 13 "Mass of Planet Eris")
     (holiday-fixed 9 26 "Bureflux")
     (holiday-fixed 10 3 "Shamlicht Kids Club Day")
     (holiday-fixed 10 5 "Gonculator Day")
     (holiday-fixed 10 6 "Mad Hatter Day")
     (holiday-fixed 10 12 "Habeas Corpus Remembrance Day")
     (holiday-fixed 10 24 "Maladay")
     (holiday-fixed 11 16 "Ek-sen-triks CluborGuild Day")
     (holiday-fixed 11 24 "Spanking Fest")
     (holiday-fixed 11 25 "537 Day")
     (holiday-fixed 12 4 "Hug Day II")
     (holiday-fixed 12 8 "Afflux")
     (holiday-fixed 12 25 "Santa Claus Day")
     (holiday-fixed 12 30 "New Year’s Eve Eve"))))

(use-package icomplete
  :custom (icomplete-mode t))

(use-package lisp-mode
  :preface (defun zharmeny/force-soft-tabs () (indent-tabs-mode -1))
  :hook ((lisp-data-mode scheme-mode) . zharmeny/force-soft-tabs))

(use-package mb-depth
  :custom (minibuffer-depth-indicate-mode t))

(use-package minibuffer
  :custom (completion-styles '(basic partial-completion flex)))

(use-package mule-cmds
  :no-require
  :config (set-language-environment "UTF-8"))

(use-package novice
  :custom (disabled-command-function nil))

(use-package nsm
  :custom (network-security-level 'paranoid))

(use-package org-src
  :custom (org-src-preserve-indentation t))

(use-package rust-ts-mode)

(use-package scroll-bar
  :custom (scroll-bar-mode nil))

(use-package server
  :hook (after-init . server-start))

(use-package shr
  :custom (shr-use-fonts nil))

(use-package simple
  :hook (text-mode . auto-fill-mode)
  :custom
  (column-number-mode t)
  (size-indication-mode t))

(use-package so-long
  :custom (global-so-long-mode t))

(use-package startup
  :no-require
  :custom (inhibit-startup-screen t))

(use-package toml-ts-mode)

(use-package treesit
  :custom (treesit-font-lock-level 4))

(use-package url-vars
  :custom (url-privacy-level 'paranoid))

(use-package xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package yaml-ts-mode)

;;;; Elpaca

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;;; External packages

(use-package apheleia
  :custom
  (apheleia-global-mode t)
  (apheleia-mode-lighter "")
  :ensure (:ref "61766b50b24fa16be519d77795dc63522e04dce8"))

(use-package apheleia-formatters
  :config (add-to-list 'apheleia-mode-alist '(scheme-mode . lisp-indent))
  :after apheleia
  :defines apheleia-mode-alist)

(use-package consult
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c m" . consult-man)
   ("M-g k" . consult-global-mark)
   ("M-g m" . consult-mark)
   ("M-g o" . consult-outline)
   ("M-s G" . consult-git-grep)
   ("M-s L" . consult-line-multi)
   ("M-s c" . consult-locate)
   ("M-s d" . consult-fd)
   ("M-s e" . consult-isearch-history)
   ("M-s g" . consult-grep)
   ("M-s k" . consult-keep-lines)
   ("M-s l" . consult-line)
   ("M-s r" . consult-ripgrep)
   ("M-s u" . consult-focus-lines)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   :map isearch-mode-map
   ("M-s L" . consult-line-multi)
   ("M-s l" . consult-line)
   ([remap isearch-edit-string] . consult-isearch-history)
   :map minibuffer-local-map
   ([remap next-matching-history-element] . consult-history))
  :ensure (:ref "4889458dccf842ab6223099f8a73ff8b147e9459"))

(use-package consult-compile
  :bind ("M-g e" . consult-compile-error))

(use-package consult-denote
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep)
  (consult-denote-mode t)
  :ensure (:ref "decdaa3935aa79b23f8ceab5768b248ee15e65fd"))

(use-package consult-flymake
  :bind ("M-g f" . consult-flymake))

(use-package consult-imenu
  :bind
  ("M-g I" . consult-imenu-multi)
  ([remap imenu] . consult-imenu))

(use-package consult-info
  :bind
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info))

(use-package consult-kmacro
  :bind ("C-c k" . consult-kmacro))

(use-package consult-org
  :bind
  (:map org-mode-map ([remap consult-outline] . consult-org-heading)))

(use-package consult-register
  :bind
  ("C-M-#" . consult-register)
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store))

(use-package cue-ts-mode
  :ensure (cue-ts-mode :host github :repo "ZharMeny/cue-ts-mode"))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n I" . denote-add-links)
   ("C-c n N" . denote-type)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n b" . denote-backlinks)
   ("C-c n c" . denote-region)
   ("C-c n d" . denote-date)
   ("C-c n f b" . denote-find-backlink)
   ("C-c n f f" . denote-find-link)
   ("C-c n i" . denote-link)
   ("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n s" . denote-subdirectory)
   ("C-c n t" . denote-template)
   ("C-c n z" . denote-signature)
   :map dired-mode-map
   ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter)
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-r" . denote-dired-rename-files))
  :ensure (:ref "02d296d8e8278d5049e7afefb4f973c2b1b89036"))

(use-package editorconfig
  :custom
  (editorconfig-mode t)
  (editorconfig-mode-lighter "")
  ;; TEMP: until Emacs 30
  :ensure (:ref "7c7b1f81f8fac654791fa2b04da55edced2cef33"))

(use-package elfeed
  :custom
  (elfeed-feeds
   '("https://blog.gtk.org/feed/"
     "https://blog.rust-lang.org/feed.xml"
     "https://blog.rust-lang.org/inside-rust/feed.xml"
     "https://blogs.gnome.org/alatiera/feed/"
     "https://blogs.gnome.org/alicem/feed/"
     "https://blogs.gnome.org/chergert/feed/"
     "https://blogs.gnome.org/haeckerfelix/feed/"
     "https://blogs.gnome.org/jsparber/feed/"
     "https://blogs.gnome.org/shell-dev/feed/"
     "https://blogs.gnome.org/sophieh/feed/"
     "https://protesilaos.com/master.xml"
     "https://solar.lowtechmagazine.com/posts/index.xml"
     "https://thephd.dev/feed.xml"
     "https://thisweek.gnome.org/index.xml"
     "https://wingolog.org/feed/atom"
     "https://xeiaso.net/blog.rss"
     "https://xkcd.com/atom.xml"))
  :ensure (:ref "5c05a1eab37bc113ecb158a4d57fe05352fa2c6a"))

(use-package elpher
  :ensure (:ref "0bd12913940a047724d830725bf8649e4f8df499"))

(use-package embark
  :bind ("C-;" . embark-act)
  :ensure (:ref "19a13e344e04bbf861eaa74491b23da52b398672"))

(use-package embark-consult
  ;; embark monorepo
  :ensure)

(use-package emms
  :ensure (:ref "b5567be2176dcbdf42aa2d0ccad32a44f245dd09"))

(use-package emms-setup
  :hook
  (elpaca-after-init . emms-all)
  (elpaca-after-init . emms-default-players))

(use-package geiser
  :ensure (:ref "a81969a5271f155d2d1e389ccbe47e1c7ec36ae7"))

(use-package geiser-guile
  :ensure (:ref "ebdd1923b0780778706ea6b16aa2b0ce3e7dc33d"))

(use-package git-modes
  :ensure (:ref "d96fa7a3c7d754812675b37247c6a77e459eec53"))

(use-package haskell-mode
  :ensure (:ref "727f72a2a4b8e4fd0a7b62129668baea55a2c3e0"))

(use-package magit
  :ensure (:ref "c575c2e09a1cf0cc245400c208dc0ce4ae424d47"))

(use-package marginalia
  :custom (marginalia-mode t)
  :ensure (:ref "77b8e568d8e14090ee40d9549fd727a62eb82f7d"))

(use-package modus-themes
  :init (load-theme 'modus-vivendi :no-confirm)
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  ;; TEMP: until Emacs 30
  :ensure (:ref "8b210e065e04dd4495ae89a179636205cee7ce9d"))

(use-package package-lint
  :ensure (:ref "972dd8403ac8d2d43f298ef89a6b118e49c7355f"))

(use-package transient
  :custom (transient-history-limit 0)
  ;; TEMP: until Emacs 30
  :ensure (:ref "2a680c21e2be0b055e0e801d43c92792e5305acc"))

(use-package wgrep
  :ensure (:ref "208b9d01cfffa71037527e3a324684b3ce45ddc4"))

;;;;; Dependencies

;; TEMP: until Emacs 30
(use-package compat
  :ensure (:ref "09dce8a193c5a70277512263782b82fa1cba84c0"))

(use-package dash
  :ensure (:ref "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))

(use-package git-commit
  ;; magit monorepo
  :ensure)

(use-package magit-section
  ;; magit monorepo
  :ensure)

(use-package with-editor
  :ensure (:ref "97dd5dd4b69a040506f4fc104dc0b855e84c1c0f"))

;;; init.el ends here
