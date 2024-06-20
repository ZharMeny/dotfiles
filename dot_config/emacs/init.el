;;; init.el --- ZharMeny's init.el  -*- lexical-binding: t; -*-

;; Copyright 2024 ZharMeny

;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/ZharMeny/dotfiles

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The init file is where all the magic happens: it is loaded when
;; Emacs is started.  Emacs looks for the init file using the
;; filenames ~/.emacs.el, ~/.emacs, or ~/.emacs.d/init.el, in that
;; order.  Additionally, Emacs can look in an XDG-compatible location
;; for init.el, the default is the directory ~/.config/emacs.

;;; Code:

;;;; Core packages

(set-language-environment "UTF-8")

(use-package emacs
  :no-require
  :custom
  (enable-recursive-minibuffers t)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (truncate-lines t)
  (visible-bell t))

(use-package display-line-numbers
  :hook (conf-mode prog-mode text-mode))

(use-package elec-pair
  :custom (electric-pair-mode t))

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

(use-package mb-depth
  :custom (minibuffer-depth-indicate-mode t))

(use-package minibuffer
  :custom
  (completion-styles
   '(basic substring partial-completion initials flex)))

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

(use-package toml-ts-mode)

(use-package treesit
  :custom (treesit-font-lock-level 4))

(use-package url-vars
  :custom (url-privacy-level 'paranoid))

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

(elpaca '(elpaca-use-package)
  (elpaca-use-package-mode))

(elpaca-wait)

;;;; External packages

(use-package apheleia
  :custom (apheleia-global-mode t)
  :ensure (:ref "61766b50b24fa16be519d77795dc63522e04dce8"))

(use-package consult
  :ensure (:ref "fe4852280006e61be7f1374d021ee06155ce5a26"))

(use-package cue-ts-mode
  :ensure (cue-ts-mode :host github :repo "ZharMeny/cue-ts-mode"))

(use-package denote
  :bind ("C-c n c" . denote)
  :ensure (:ref "b8fd5afe8fec763438b32bd470d6c5d690c1f43a"))

(use-package elfeed
  :custom
  (elfeed-feeds
   '("https://blog.rust-lang.org/feed.xml"
     "https://blog.rust-lang.org/inside-rust/feed.xml"
     "https://blogs.gnome.org/alicem/feed/"
     "https://blogs.gnome.org/chergert/feed/"
     "https://irreal.org/blog/?feed=rss2"
     "https://protesilaos.com/master.xml"
     "https://quiltmc.org/feed.xml"
     "https://samsai.eu/index.xml"
     "https://servo.org/blog/feed.xml"
     "https://solar.lowtechmagazine.com/posts/index.xml"
     "https://thelambdalab.xyz/gitweb/index.cgi?p=elpher.git;a=atom"
     "https://thephd.dev/feed.xml"
     "https://thisweek.gnome.org/index.xml"
     "https://utopic.work/devlogs/feed.rss"
     "https://vkc.sh/feed/"
     "https://wingolog.org/feed/atom"
     "https://www.florkofcows.com/comic/atom/"
     "https://xeiaso.net/blog.rss"
     "https://xkcd.com/atom.xml"))
  :ensure (:ref "5c05a1eab37bc113ecb158a4d57fe05352fa2c6a"))

(use-package elpher
  :ensure (:ref "56bc74e224d9835c41b6e6b68c9705b60e6dbbe2"))

(use-package embark
  :bind ("C-;" . embark-act)
  :ensure (:ref "9c166c4b96a0b1e85401bcc6fb95ce021e7b5013"))

(use-package embark-consult
  :after embark
  :ensure)

(use-package emms
  :ensure (:ref "2c328f0a4d46c008d409bbe44994588816adf221"))

(use-package emms-score
  :hook (elpaca-after-init . emms-score-disable)
  :after emms)

(use-package emms-setup
  :hook
  (elpaca-after-init . emms-all)
  (elpaca-after-init . emms-default-players)
  :after emms)

(use-package git-modes
  :ensure (:ref "52ea2a1281ea9df9b8732fe2add0e6a0c9c2cd11"))

(use-package haskell-mode
  :ensure (:ref "727f72a2a4b8e4fd0a7b62129668baea55a2c3e0"))

(use-package magit
  :ensure (:ref "df58c94081370e2c1aa7ab9ba90462f6d690800d"))

(use-package marginalia
  :custom (marginalia-mode t)
  :ensure (:ref "da72da4622c7b38741e6968678028f7e0564816c"))

;; TEMP: until Emacs 30
(use-package modus-themes
  :config (load-theme 'modus-vivendi :no-confirm)
  :ensure (:ref "1090a80a76c77d215b948d68a707fbb7e2b8d407"))

(use-package package-lint
  :ensure (:ref "972dd8403ac8d2d43f298ef89a6b118e49c7355f"))

(use-package transient
  :custom (transient-history-limit 0)
  ;; TEMP: until Emacs 30
  :ensure (:ref "a42b2f6992b27e8da924d4ff86ef9a33b6804b84"))

(use-package wgrep
  :ensure (:ref "208b9d01cfffa71037527e3a324684b3ce45ddc4"))

;;;;; Dependencies

;; TEMP: until Emacs 30
(use-package compat
  :ensure (:ref "80dbd9bc5efee05a479663f8cfd0cc9e0a30dac5"))

(use-package dash
  :ensure (:ref "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))

(use-package git-commit
  :after magit
  :ensure)

(use-package magit-section
  :after magit
  :ensure)

(use-package with-editor
  :ensure (:ref "f6a3fc8f6735fbc804e02f9c54bc621746afd5b0"))

;;; init.el ends here
