;;; arch-packer.el --- Arch Linux package management frontend -*- lexical-binding: t -*-

;; Copyright (C) 2017  Fritz Stelzer <brotzeitmacher@gmail.com>

;; Author: Fritz Stelzer <brotzeitmacher@gmail.com>
;; URL: https://github.com/brotzeitmacher/arch-packer
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (s "1.11.0"))

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package can be either used with pacman or pacaur.  It provides
;; a package menu similiar to package.el.  Outdated packages are
;; displayed on top of the list and AUR packages are highlighted by
;; default.

;;; Code:

(require 'tabulated-list)
(require 's)
(require 'json)

(defgroup arch-packer nil
  "Manager for Arch Linux packages."
  :prefix "arch-packer-"
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables

(defcustom arch-packer-default-command "pacman"
  "Default package manager."
  :type '(choice (const :tag "pacman" "pacman")
                 (const :tag "pacaur" "pacaur"))
  :group 'arch-packer)

(defcustom arch-packer-column-width-package 18
  "Width of the Package column."
  :type 'integer
  :group 'arch-packer)

(defcustom arch-packer-column-width-version 20
  "Width of the Version and Latest columns."
  :type 'integer
  :group 'arch-packer)

(defcustom arch-packer-menu-latest-face "firebrick"
  "Face for latest version when newer than installed version."
  :type 'face
  :group 'arch-packer)

(defcustom arch-packer-menu-aur-face "#1793d0"
  "Face for AUR packages."
  :type 'face
  :group 'arch-packer)

(defcustom arch-packer-highlight-aur-packages t
  "Highlight AUR packages."
  :type 'boolean
  :group 'arch-packer)

(defcustom arch-packer-query-options t
  "Restrict or filter output to explicitly installed packages."
  :type 'boolean
  :group 'arch-packer)

(defcustom arch-packer-display-status-reporter t
  "Display progress-reporter."
  :type 'boolean
  :group 'arch-packer)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package menu mode

(defvar arch-packer-package-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "m") 'arch-packer-menu-mark-unmark)
    (define-key map (kbd "d") 'arch-packer-menu-mark-delete)
    (define-key map (kbd "U") 'arch-packer-menu-mark-all-upgrades)
    (define-key map (kbd "u") 'arch-packer-menu-mark-upgrade)
    (define-key map (kbd "r") 'arch-packer-list-packages)
    (define-key map (kbd "i") 'arch-packer-install-package)
    (define-key map (kbd "x") 'arch-packer-menu-execute)
    (define-key map (kbd "RET") 'arch-packer-menu-visit-homepage)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Local keymap for `arch-packer-package-menu-mode' buffers.")

(define-derived-mode arch-packer-package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of installed arch-packer packages."
  (setq buffer-read-only nil)
  (setq truncate-lines t)
  (setq tabulated-list-format
        `[("Package" ,arch-packer-column-width-package nil)
          ("Version" ,arch-packer-column-width-version nil)
          ("Latest" ,arch-packer-column-width-version nil)
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun pacman-menu-entry (pkg)
  "Return a package entry of PKG suitable for `tabulated-list-entries'."
  (let ((name (alist-get 'Name pkg))
        (version (alist-get 'Version pkg))
        (latest (alist-get 'Latest pkg))
        (description (alist-get 'Description pkg))
        (link (alist-get 'URL pkg)))
    (list name `[,(if (and arch-packer-highlight-aur-packages
                           (alist-get 'Validated pkg))
                      (progn
                        (put-text-property 0 (length name) 'link link name)
                        (put-text-property 0 (length name) 'AUR version name)
                        (propertize name 'font-lock-face `(:foreground ,arch-packer-menu-aur-face)))
                    (progn
                      (put-text-property 0 (length name) 'link link name)
                      name))
                 ,version
                 ,(if (string= version latest)
                      latest
                    (propertize latest 'font-lock-face `(:foreground ,arch-packer-menu-latest-face)))
                 ,description])))

(defun arch-packer-generate-menu (packages)
  "Re-populate the `tabulated-list-entries' with PACKAGES."
  (let ((buf (get-buffer arch-packer-process-buffer))
        pkg-list
        latest)
    (with-current-buffer buf
      (let ((_read-only-mode nil))
        (arch-packer-package-menu-mode)
        (erase-buffer)
        (goto-char (point-min))
        (setq tabulated-list-entries nil)
        (setq pkg-list
              (reverse (mapcar #'pacman-menu-entry packages)))
        (dolist (pkg pkg-list)
          (if (not (string= (elt (elt pkg 1) 1) (elt (elt pkg 1) 2)))
              (push pkg tabulated-list-entries)
            (push pkg latest)))
        (dolist (pkg (reverse latest))
          (push pkg tabulated-list-entries))
        (setq tabulated-list-entries (reverse tabulated-list-entries))
        (tabulated-list-print t)
        (read-only-mode t)))
    (display-buffer buf)))

(defun arch-packer-get-package-alist ()
  "Return alist containing various package information on installed packages."
  (let ((outdated (arch-packer-get-outdated))
        (packages (arch-packer-get-info))
        result)
    (dolist (pkg packages)
      (let ((pkg-raw (split-string pkg "\n"))
            (split-attr (lambda (attr) (nth 1 (split-string attr " : "))))
            pkg-alist)
        (dolist (attr pkg-raw)
          (cond
           ((string-match "^Name" attr)
            (push `(Name . ,(funcall split-attr attr)) pkg-alist))
           ((string-match "^Version " attr)
            (push `(Version . ,(funcall split-attr attr)) pkg-alist))
           ((string-match "^Description" attr)
            (push `(Description . ,(funcall split-attr attr)) pkg-alist))
           ((string-match "^URL" attr)
            (push `(URL . ,(funcall split-attr attr)) pkg-alist))
           ((string-match "^Validated By" attr)
            (when (string-match "None" (funcall split-attr attr))
              (push `(Validated . None) pkg-alist)))))
        (dolist (out outdated)
          (when (string= (cdr (assoc 'name out)) (cdr (assoc 'Name pkg-alist)))
            (let ((Latest `(Latest . ,(cdr (assoc 'latest out)))))
              (push Latest pkg-alist)
              (setq outdated (delete out outdated))
              (return))))
        (unless (cdr (assoc 'Latest pkg-alist))
          (push `(Latest . ,(cdr (assoc 'Version pkg-alist))) pkg-alist))
        (push pkg-alist result)))
    result))

;;;;;;;;;;;;;;;;;;;;
;;; Shell Process

(defvar arch-packer-process-name "arch-packer-process"
  "Process name for arch-packer processes.")

(defvar arch-packer-process-buffer "*Pacman-Packages*"
  "Buffer name for arch-packer process buffers.")

(defvar arch-packer-process-output nil
  "Holds output of last command executed by subprocess.")

(defun arch-packer-open-shell-process ()
  "Start shell process."
  (let ((buf arch-packer-process-buffer))
    (start-process arch-packer-process-name
                   buf
                   "/bin/bash")
    (let ((proc (get-buffer-process buf)))
      (set-process-filter proc 'arch-packer-process-filter)
      (accept-process-output proc 0.1))))

(defun arch-packer-process-filter (proc _output)
  "Filter for arch-packer-process PROC."
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (cond
       ((string-match "Pacman error\n" output)
        (arch-packer-disable-status-reporter)
        (message arch-packer-subprocess-output))
       ((string-match "Pacman finished\n" output)
        (arch-packer-generate-menu (arch-packer-get-package-alist))
        (arch-packer-disable-status-reporter)
        (message "Pacman finished"))
       ((string-match "\\[sudo\\] password for" output)
        (arch-packer-disable-status-reporter)
        (arch-packer-send-root)
        (arch-packer-enable-status-reporter)
        (arch-packer-wait-shell-subprocess)
        (arch-packer-get-exit-status))
       (t
        (unless (string-match "^\\[" output)
          (setq arch-packer-subprocess-output output)))))))

(defun arch-packer-call-shell-process (proc string)
  "Send arch-packer shell-process PROC the contents of STRING as input."
  (process-send-string proc (concat string "\n"))
  (arch-packer-enable-status-reporter))

(defun arch-packer-send-root ()
  "Prompt user for root password and send it to arch-packer-process."
  (let* ((map minibuffer-local-map))
    (define-key map "\C-g"
      (lambda ()
        (interactive)
        (arch-packer-disable-status-reporter)
        (ignore-errors (kill-process arch-packer-process-name))
        (abort-recursive-edit)))
    (let ((minibuffer-local-map map)
          (passwd (read-passwd "Password: ")))
      (arch-packer-call-shell-process arch-packer-process-name passwd)
      (clear-string passwd))))

(defun arch-packer-shell-process-live-p ()
  "Is arch-packer shell-process running."
  (process-live-p (get-buffer-process arch-packer-process-buffer)))

(defun arch-packer-wait-shell-subprocess ()
  "Wait until subprocess of arch-packer shell process finished."
  (while (process-running-child-p arch-packer-process-name)
    (sit-for 1)))

(defun arch-packer-get-exit-status ()
  "Get exit status of pacman subprocess."
  (arch-packer-wait-shell-subprocess)
  (arch-packer-call-shell-process arch-packer-process-name
                             "if [ `echo $?` -ne 0 ]; 
                              then echo \"Pacman error\n\"; 
                              else echo \"Pacman finished\n\" ;fi"))

(defun arch-packer-disable-status-reporter ()
  "Enable progress-reporter."
  (when arch-packer-display-status-reporter
   (remove-hook 'post-command-hook 'arch-packer-status-reporter)))

(defun arch-packer-enable-status-reporter ()
  "Disable progress-reporter."
  (when arch-packer-display-status-reporter
   (add-hook 'post-command-hook 'arch-packer-status-reporter)))

(defun arch-packer-status-reporter ()
  "Status indicator is shown in the echo area while arch-packer shell process alive."
  (unless (active-minibuffer-window)
    (let ((progress-reporter (make-progress-reporter "Pacman processing...")))
      (dotimes (i 1000)
        (when (process-running-child-p arch-packer-process-name)
          (progress-reporter-update progress-reporter i)
          (sit-for 0.1))))))

;;;;;;;;;;;;;
;;; Pacman

(defun arch-packer-shell-command ()
  "Prepend sudo when using pacman."
  (if (string= arch-packer-default-command "pacman")
      (concat "sudo " arch-packer-default-command)
    arch-packer-default-command))

(defun arch-packer-refresh-database ()
  "Download a fresh copy of the master package database."
  (let ((command (arch-packer-shell-command)))
    (arch-packer-call-shell-process arch-packer-process-name (concat command " -Sy"))))

(defun arch-packer-delete-package (packages)
  "Uninstall provided PACKAGES."
  (let ((command (arch-packer-shell-command)))
  (arch-packer-call-shell-process arch-packer-process-name (concat command
                                                                   " -Rsn --noconfirm "
                                                                   packages))
  (arch-packer-wait-shell-subprocess)
  (arch-packer-get-exit-status)))

(defun arch-packer-upgrade-package (packages)
  "Upgrade provided PACKAGES."
  (let ((command (arch-packer-shell-command)))
    (arch-packer-call-shell-process arch-packer-process-name (concat command
                                                                     " -S --noconfirm "
                                                                     packages))
    (arch-packer-wait-shell-subprocess)
    (arch-packer-get-exit-status)))

(defun arch-packer-get-info ()
  "Return list containing information on installed packages."
  (remove "" (split-string
              (shell-command-to-string
               (concat arch-packer-default-command
                       (format " -Q%s --info" (if arch-packer-query-options "e" ""))))
              "\n\n")))

(defun arch-packer-get-outdated ()
  "Return outdated packages."
  (let* ((default (string= arch-packer-default-command "pacman"))
         (name (--if-let default
                   0 2))
         (latest (--if-let default
                     3 5)))
    (mapcar #'(lambda (pkg)
                (let (pkg-attr)
                  (push `(name . ,(nth name (split-string pkg))) pkg-attr)
                  (push `(latest . ,(nth latest (split-string pkg))) pkg-attr)))
            (remove "" (split-string (shell-command-to-string (concat arch-packer-default-command
                                                                      " -Qu"))
                                     "\n")))))
  
;;;;;;;;;;;;;;;;;;
;;; Interaction

(defun arch-packer-menu-mark-upgrade ()
  "Mark upgradable package."
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (unless (string= (aref (tabulated-list-get-entry) 1) (aref (tabulated-list-get-entry) 2))
      (tabulated-list-put-tag "U" t))))

(defun arch-packer-menu-mark-delete ()
  "Mark package for deletion and move to the next line."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun arch-packer-menu-mark-unmark ()
  "Clear any marks on a package."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun arch-packer-menu-mark-all-upgrades ()
  "Mark all upgradable packages in the Package Menu."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (arch-packer-menu-mark-upgrade)
      (forward-line))))

(defun arch-packer-menu-visit-homepage ()
  "Browse provided URL."
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (browse-url (get-text-property (point) 'link))))

(defun arch-packer-menu-execute ()
  "Perform marked Package Menu actions."
  (interactive)
  (let (upgrade-list
        delete-list
        cmd
        pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (setq pkg-desc (tabulated-list-get-id))
        (cond ((eq cmd ?D)
               (push (substring-no-properties pkg-desc) delete-list))
              ((eq cmd ?U)
               (push (substring-no-properties pkg-desc) upgrade-list)))
        (forward-line)))
    (unless (or delete-list upgrade-list)
      (user-error "No operations specified"))
    (let* ((del (when delete-list
                  (concat (format "Delete %d package%s "
                                  (length delete-list)
                                  (if (> (length delete-list) 1)
                                      "s" ""))
                          (replace-regexp-in-string " " ", "
                                                    (format "%s" delete-list)))))
           (up (when upgrade-list
                 (concat (format "Upgrade %d package%s "
                                 (length upgrade-list)
                                 (if (> (length upgrade-list) 1)
                                     "s" ""))
                         (replace-regexp-in-string " " ", "
                                                   (format "%s" upgrade-list)))))
           (msg (if (and del up)
                    (concat del " and " up)
                  (or del up))))
      (when (yes-or-no-p (format "%s" msg))
        (when upgrade-list
          (arch-packer-upgrade-package (mapconcat 'identity upgrade-list " ")))
        (arch-packer-wait-shell-subprocess)
        (when delete-list
          (arch-packer-delete-package (mapconcat 'identity delete-list " ")))))))

;;;###autoload
(defun arch-packer-install-package ()
  "Prompt user for a string containing packages to be installed."
  (interactive)
  (unless (arch-packer-shell-process-live-p)
    (and
     (arch-packer-open-shell-process)
     (arch-packer-refresh-database)))
  (let ((pkg (read-from-minibuffer "Enter package name: ")))
    (arch-packer-upgrade-package pkg)))

;;;###autoload
(defun arch-packer-list-packages ()
  "Refresh package menu."
  (interactive)
  (if (arch-packer-shell-process-live-p)
      (arch-packer-get-exit-status)
    (progn (arch-packer-open-shell-process)
           (arch-packer-refresh-database))))

(provide 'arch-packer)
;;; arch-packer.el ends here
