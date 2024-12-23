;;; qutebrowser.el --- Qutebrowser integration with Emacs and EXWM     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lars Rustand.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Lars Rustand
;; URL: https://github.com/lrustand/qutebrowser.el
;; Version: 0
;; Package-Requires: ((emacs "29.1") (consult "0.34"))

;;; Commentary:

;; This package adds enhanced support for Qutebrowser under EXWM,
;; including integration with the Emacs bookmark system, buffer and
;; history sources for Consult, a minor mode for Qutebrowser EXWM
;; buffers, a minor mode providing theme synchronization between Emacs
;; and Qutebrowser, and facilities for sending arbitrary commands to
;; Qutebrowser from Emacs using IPC.

;;; Change Log:

;;; Code:

(require 'sqlite)
(require 'consult)
(require 'exwm)
(require 'json)
(require 'color)

(defgroup qutebrowser nil
  "EXWM enhancements for Qutebrowser."
  :group 'external)

(defcustom qutebrowser-theme-export-face-mappings
         '((completion.fg . default)
           (completion.odd.bg . default)
           (completion.even.bg . default)
           (completion.category.fg . font-lock-function-name-face)
           (completion.category.bg . default)
           (completion.category.border.top . mode-line)
           (completion.category.border.bottom . mode-line)
           (completion.item.selected.fg . highlight)
           (completion.item.selected.bg . highlight)
           (completion.item.selected.border.top . highlight)
           (completion.item.selected.border.bottom . highlight)
           (completion.match.fg . dired-directory)
           (completion.scrollbar.fg . scroll-bar)
           (completion.scrollbar.bg . scroll-bar)
           (contextmenu.disabled.bg . default)
           (contextmenu.disabled.fg . shadow)
           (contextmenu.menu.bg . default)
           (contextmenu.menu.fg . default)
           (contextmenu.selected.bg . highlight)
           (contextmenu.selected.fg . highlight)
           (downloads.bar.bg . mode-line)
           (downloads.start.fg . success)
           (downloads.start.bg . success)
           (downloads.stop.fg . error)
           (downloads.stop.bg . error)
           (downloads.error.fg . error)
           (hints.fg . avy-lead-face)
           (hints.bg . avy-lead-face)
           (hints.match.fg . avy-lead-face-0)
           (keyhint.fg . default)
           (keyhint.suffix.fg . font-lock-constant-face)
           (keyhint.bg . highlight)
           (messages.error.fg . error)
           (messages.error.bg . error)
           (messages.error.border . error)
           (messages.warning.fg . warning)
           (messages.warning.bg . warning)
           (messages.warning.border . warning)
           (messages.info.fg . success)
           (messages.info.bg . success)
           (messages.info.border . success)
           (prompts.fg . minibuffer-prompt)
           (prompts.bg . highlight)
           (prompts.border . minibuffer-prompt)
           (prompts.selected.fg . success)
           (prompts.selected.bg . success)
           (statusbar.normal.fg . mode-line)
           (statusbar.normal.bg . default)
           (statusbar.insert.fg . dired-header)
           (statusbar.insert.bg . dired-header)
           (statusbar.passthrough.fg . mode-line)
           (statusbar.passthrough.bg . mode-line)
           (statusbar.private.fg . mode-line)
           (statusbar.private.bg . mode-line)
           (statusbar.command.fg . mode-line)
           (statusbar.command.bg . mode-line)
           (statusbar.command.private.fg . mode-line)
           (statusbar.command.private.bg . mode-line)
           (statusbar.caret.fg . region)
           (statusbar.caret.bg . region)
           (statusbar.caret.selection.fg . region)
           (statusbar.caret.selection.bg . region)
           (statusbar.progress.bg . mode-line)
           (statusbar.url.fg . success)
           (statusbar.url.error.fg . error)
           (statusbar.url.hover.fg . link-visited)
           (statusbar.url.success.http.fg . success)
           (statusbar.url.success.https.fg . success)
           (statusbar.url.warn.fg . warning)
           (tabs.bar.bg . tab-bar)
           (tabs.indicator.start . success)
           (tabs.indicator.stop . mode-line)
           (tabs.indicator.error . error)
           (tabs.odd.fg . tab-bar)
           (tabs.odd.bg . tab-bar)
           (tabs.even.fg . tab-bar)
           (tabs.even.bg . tab-bar)
           (tabs.pinned.even.bg . tab-bar)
           (tabs.pinned.even.fg . tab-bar)
           (tabs.pinned.odd.bg . tab-bar)
           (tabs.pinned.odd.fg . tab-bar)
           (tabs.pinned.selected.even.fg . tab-line)
           (tabs.pinned.selected.even.bg . tab-line)
           (tabs.pinned.selected.odd.fg . tab-line)
           (tabs.pinned.selected.odd.bg . tab-line)
           (tabs.selected.odd.fg . tab-line)
           (tabs.selected.odd.bg . tab-line)
           (tabs.selected.even.fg . tab-line)
           (tabs.selected.even.bg . tab-line)
           (webpage.bg . default))
         "Mapping between Emacs faces and Qutebrowser color settings."
         :type '(alist :key-type symbol
                       :value-type face)
         :group 'qutebrowser)

(defcustom qutebrowser-default-open-target 'auto
  "The default open target for Qutebrowser."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Tab" tab)
                 (const :tag "Window" window)
                 (const :tag "Private Window" private-window))
  :group 'qutebrowser)

(defcustom qutebrowser-command-backend 'qutebrowser-ipc-send
  "The backend to use when sending commands to Qutebrowser."
  :type '(choice (const :tag "IPC" qutebrowser-ipc-send)
                 (const :tag "FIFO" qutebrowser-fifo-send)
                 (const :tag "Commandline" qutebrowser-commandline-send)
                 (function :tag "Custom command"))
  :risky t
  :group 'qutebrowser)

(defcustom qutebrowser-history-database
  "~/.local/share/qutebrowser/history.sqlite"
  "Path to the Qutebrowser history database."
  :type 'file
  :risky t
  :group 'qutebrowser)

(defcustom qutebrowser-history-exclusion-patterns
  '("https://www.google.%/search?%"
    "https://www.google.com/sorry/%"
    "https://scholar.google.com/scholar?%&q=%"
    "https://%youtube.com/results?%"
    "https://%perplexity.ai/search/%"
    "https://%/search?%"
    "https://%?search=%"
    "https://%/search/?%"
    "https://%/search_result?%"
    "https://www.finn.no/%/search.html?%"
    "https://www.finn.no/globalsearchlander?%"
    "https://%ebay.%/sch/%"
    "https://%amazon.%/s?%"
    "https://%duckduckgo.com/?%q=%")

  "URL patterns to exclude from the Qutebrowser history list.
The patterns are SQlite wildcard patterns, and will be used to build up
the WHERE clause of the database query.  See `qutebrowser--history' for
more details on how the query is built."
  :type '(repeat string)
  :group 'qutebrowser)

(defcustom qutebrowser-title-display-length 100
  "Max display length of Qutebrowser titles in completion lists."
  :type 'integer
  :group 'qutebrowser)

(defcustom qutebrowser-url-display-length 50
  "Max display length of Qutebrowser URLs in completion lists."
  :type 'integer
  :group 'qutebrowser)

(defcustom qutebrowser-history-order-by "last_atime DESC"
  "How to sort the history entries in the completion lists."
  :type '(choice
          (const :tag "Unsorted" nil)
          (const :tag "Recency" "last_atime DESC")
          (string :tag "Custom ORDER BY clause"))
  :risky t
  :group 'qutebrowser)

(defgroup qutebrowser-faces nil
  "Faces used by qutebrowser.el."
  :group 'qutebrowser
  :group 'faces)

(defface qutebrowser-title-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Qutebrowser titles."
  :group 'qutebrowser-faces)

(defvar qutebrowser-mode-enter-hook nil
  "Hook run after Qutebrowser enters a mode.")

(defvar qutebrowser-mode-leave-hook nil
  "Hook run after Qutebrowser leaves a mode.")

(defvar qutebrowser-new-window-hook nil
  "Hook run after a new window Qutebrowser window is created.")

(defvar qutebrowser-config-changed-hook nil
  "Hook run after Qutebrowser config is changed.")

(defvar qutebrowser-got-search-hook nil
  "Hook run after a search term is entered.")

(defvar qutebrowser-url-changed-hook nil
  "Hook run after Qutebrowser changes URL.")

(defun qutebrowser--history ()
  "Get the Qutebrowser history from the sqlite database."
  (let* ((db (sqlite-open qutebrowser-history-database))
         (where (format "WHERE url NOT LIKE '%s'"
                        (string-join qutebrowser-history-exclusion-patterns
                                     "' AND url NOT LIKE '")))
         (order (when qutebrowser-history-order-by
                  (format "ORDER BY %s" qutebrowser-history-order-by)))
         (query (format "SELECT url,substr(title,0,%d)
                         FROM CompletionHistory
                         %s
                         %s"
                        (1- qutebrowser-title-display-length)
                        where
                        order)))
    (sqlite-select db query)))

(defun qutebrowser--pseudo-annotate (row &optional buffer)
  "Create pseudo-annotated entries from each ROW.
This simulates marginalia annotations, but allows the text in the
annotation to be searchable.  Optionally embeds BUFFER as a text
property."
  (let* ((url (nth 0 row))
         (title (nth 1 row))
         (display-url (truncate-string-to-width url qutebrowser-url-display-length 0 ?\ ))
         (display-title (truncate-string-to-width title qutebrowser-title-display-length 0 ?\ )))
    (format "%s %s"
            (propertize display-url
                        'url url
                        'title title
                        'buffer buffer)
            (propertize display-title
                        'face 'qutebrowser-title-face))))

(defun qutebrowser--history-candidates ()
  "Lists completion candidates from Qutebrowser history.
Candidates contain the url, and a pseudo-annotation with the
website title, to allow searching based on either one."
  (let* ((history (qutebrowser--history)))
    (mapcar #'qutebrowser--pseudo-annotate
            history)))

(defun qutebrowser--target-to-flag (target)
  "Return the :open flag corresponding to TARGET."
  (pcase target
    ('window "-w")
    ('tab "-t")
    ('private-window "-p")
    ('auto "")))

(defun qutebrowser-launcher--internal (&optional initial target)
  "Internal dispatcher for the user-facing launcher commands.
INITIAL is the initial input for completion, and TARGET is where to open."
  (let* ((qutebrowser-default-open-target
          (or target qutebrowser-default-open-target))
         (res (consult--multi '(qutebrowser--exwm-buffer-source
                                qutebrowser--bookmark-source
                                qutebrowser--history-source)
                              :initial initial
                              :annotate nil
                              :sort nil))
         (plist (cdr res))
         (selected (car res)))
    ;; If none of the buffer sources handled it
    (unless (plist-get plist :match)
      (qutebrowser-open-url selected))))

;;;###autoload
(defun qutebrowser-launcher (&optional initial target)
  "Select a URL to open in Qutebrowser.
Set initial completion input to INITIAL.  Open the URL in TARGET or the
default target if nil."
  (interactive)
  (qutebrowser-launcher--internal initial target))

;;;###autoload
(defun qutebrowser-launcher-tab (&optional initial)
  "Select a URL to open in a new tab.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher--internal initial 'tab))

;;;###autoload
(defun qutebrowser-launcher-window (&optional initial)
  "Select a URL to open in a new window.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher--internal initial 'window))

;;;###autoload
(defun qutebrowser-launcher-private (&optional initial)
  "Select a URL to open in a private window.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher--internal initial 'private-window))


(defun qutebrowser--format-window-entry (buffer)
  "Format a `consult--multi' entry for BUFFER.
Expects the `buffer-name' of BUFFER to be propertized with a url field."
  (let* ((bufname (buffer-name buffer))
         (title (substring-no-properties bufname))
         (url (or (get-text-property 0 'url bufname) "")))
    (cons
     (qutebrowser--pseudo-annotate
      (list url title))
     buffer)))

(defun qutebrowser-exwm-p (&optional buffer)
  "Return t if BUFFER is a Qutebrowser EXWM buffer."
  (with-current-buffer (or buffer (current-buffer))
    (string-equal "qutebrowser"
                  exwm-class-name)))

(defun qutebrowser-propertize-buffer-name (window-title)
  "Propertize the buffer name of Qutebrowser buffer.
WINDOW-TITLE is the title of the Qutebrowser window, as reported by
`exwm-title'.  Expects the window title to be formatted in the following
way:

c.window.title_format = '{audio}{private}{current_title}{title_sep}{current_url}'

This function should be added to `exwm-update-title-hook'.  If you
already have set up a hook to update buffer names, the hook should be
modified so that it runs this function for Qutebrowser buffers.

The following is what I have in my own init.el:

  (defun exwm-update-title ()
    (if (string-equal \"qutebrowser\" exwm-class-name)
        (exwm-workspace-rename-buffer
         (qutebrowser-propertize-buffer-name exwm-title))
      (exwm-workspace-rename-buffer exwm-title)))
  (add-hook 'exwm-update-title-hook #'exwm-update-title)"

  (let ((mid (string-match " - https?://.*$"
                           window-title)))
    (if mid
        (let ((title (substring window-title 0 mid))
              (url (substring window-title (+ 3 mid))))
          (propertize title 'url url))
      window-title)))

(defvar qutebrowser--exwm-buffer-source
  (list :name "Qutebrowser buffers"
        :hidden nil
        :narrow ?q
        :history nil
        :category 'other
        :action #'switch-to-buffer
        :annotate nil
        :items
        (lambda () (mapcar #'qutebrowser--format-window-entry
                           (consult--buffer-query
                            :sort 'visibility
                            :predicate #'qutebrowser-exwm-p))))
  "`consult-buffer' source for open Qutebrowser windows.")

(defun qutebrowser-bookmark-p (bookmark)
  "Return t if BOOKMARK is a Qutebrowser bookmark."
  (eq 'qutebrowser-bookmark-jump
      (bookmark-get-handler bookmark)))

(defun qutebrowser-bookmarks-list ()
  "Return a list of Qutebrowser bookmarks."
  (seq-filter #'qutebrowser-bookmark-p
              (bookmark-all-names)))

(defvar qutebrowser--bookmark-source
  (list :name "Qutebrowser bookmarks"
        :hidden nil
        :narrow ?m
        :history nil
        :category 'other
        :face 'consult-bookmark
        :action #'qutebrowser-bookmark-jump
        :items #'qutebrowser-bookmarks-list)
  "`consult-buffer' source for Qutebrowser bookmarks.")


(defvar qutebrowser--history-source
  (list :name "Qutebrowser history"
        :hidden nil
        :narrow ?h
        :history nil
        :category 'buffer
        :annotate nil
        :action (lambda (entry)
                  (let ((url (or (get-text-property 0 'url entry)
                                 entry)))
                    (qutebrowser-open-url url)))
        :items #'qutebrowser--history-candidates)
  "`consult-buffer' source for Qutebrowser history.")

;; Prevent Prescient history from being clogged up by web pages.
(with-eval-after-load 'vertico-prescient
  (defun qutebrowser-advice-vertico-prescient (orig-fun &rest args)
    "Exclude Qutebrowser buffer names and URLs from prescient history."
    (let* ((selected-candidate
            (substring (minibuffer-contents-no-properties) 0 -1))
           (selected-buffer (get-buffer selected-candidate)))
      (unless (or (qutebrowser-exwm-p selected-buffer)
                  (string-match-p "^https?://" selected-candidate))
        (funcall orig-fun))))
  (advice-add 'vertico-prescient--remember-minibuffer-contents :around
              #'qutebrowser-advice-vertico-prescient))

(defvar qutebrowser-ipc-protocol-version 1
  "The protocol version for Qutebrowser IPC.")

(defun qutebrowser-ipc-socket-path ()
  "Return the path to Qutebrowser's IPC socket."
  (expand-file-name
   (format "qutebrowser/ipc-%s" (md5 (user-login-name)))
   (or (getenv "XDG_RUNTIME_DIR")
       (format "/run/user/%d" (user-real-uid)))))

(defun qutebrowser-ipc-send (&rest commands)
  "Send COMMANDS to Qutebrowser via IPC.
Falls back to sending over commandline if IPC fails."
  (condition-case err
      (let* ((socket-path (qutebrowser-ipc-socket-path))
             (data (json-encode `(("args" . ,commands)
                                  ("target_arg" . nil)
                                  ("protocol_version" . ,qutebrowser-ipc-protocol-version))))
             (process (make-network-process :name "qutebrowser-ipc"
                                            :family 'local
                                            :service socket-path
                                            :coding 'utf-8)))
        (process-send-string process (concat data "\n"))
        (delete-process process))
    (file-error
     (progn
       (message "Error connecting to Qutebrowser IPC socket: %s" (error-message-string err))
       (message "Starting new Qutebrowser instance.")
       (apply #'qutebrowser-commandline-send commands)))
    (error
     (message "Unexpected error in qutebrowser-ipc-send: %s" (error-message-string err)))))

(defun qutebrowser-commandline-send (&rest commands)
  "Send COMMANDS to Qutebrowser via commandline."
  (apply #'start-process "qutebrowser" nil "qutebrowser" commands))

(defvar qutebrowser-fifo nil
  "Holds the path of the Qutebrowser FIFO when called as a userscript.")

(defun qutebrowser-fifo-send (&rest commands)
  "Send COMMANDS to Qutebrowser via FIFO.
Expects to be called from Qutebrowser through a userscript that
let-binds the path to the Qutebrowser FIFO to the variable
`qutebrowser-fifo'."
  (dolist (cmd commands)
    (write-region (concat cmd "\n") nil qutebrowser-fifo t 'novisit)))

(defun qutebrowser-send-commands (&rest commands)
  "Send COMMANDS to Qutebrowser via the selected backend."
  (apply qutebrowser-command-backend commands))

(defun qutebrowser-open-url (url &optional target)
  "Open URL in Qutebrowser.
TARGET specifies where to open it, or `qutebrowser-default-open-target'
if nil."
  (let* ((target (or target qutebrowser-default-open-target))
         (flag (qutebrowser--target-to-flag target)))
    (qutebrowser-send-commands (format ":open %s %s" flag url))))

(defun qutebrowser-config-source (&optional config-file)
  "Source CONFIG-FILE in running Qutebrowser instance."
  (interactive)
  (qutebrowser-send-commands (concat ":config-source " config-file)))

(defun qutebrowser-execute-python (python-code)
  "Execute PYTHON-CODE in running Qutebrowser instance.
Creates a temporary file and sources it in Qutebrowser using the
:config-source command."
  (let ((temp-conf-file (make-temp-file "qutebrowser-temp-config"
                                        nil nil python-code)))
    (qutebrowser-config-source temp-conf-file)))

(defun qutebrowser-execute-js (js-code)
  "Execute JS-CODE in running Qutebrowser instance."
  (qutebrowser-send-commands (format ":jseval -w main %s" js-code)))

;;;###autoload
(define-minor-mode qutebrowser-exwm-mode
  "Minor mode for Qutebrowser buffers in EXWM."
  :lighter nil
  :global nil
  (if qutebrowser-exwm-mode
      (setq-local bookmark-make-record-function
                  #'qutebrowser-bookmark-make-record)
    (kill-local-variable 'bookmark-make-record-function)))

(defun qutebrowser-exwm-mode-maybe-enable ()
  "Enable `qutebrowser-exwm-mode' if the buffer is a Qutebrowser buffer."
  (when (qutebrowser-exwm-p)
    (qutebrowser-exwm-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-qutebrowser-exwm-mode
  qutebrowser-exwm-mode
  qutebrowser-exwm-mode-maybe-enable
  (if global-qutebrowser-exwm-mode
      (add-hook 'exwm-manage-finish-hook #'qutebrowser-exwm-mode-maybe-enable)
    (remove-hook 'exwm-manage-finish-hook #'qutebrowser-exwm-mode-maybe-enable)))


(defun qutebrowser-bookmark-make-record ()
  "Make a bookmark record for Qutebrowser buffers."
  `(,(buffer-name)
    (handler . qutebrowser-bookmark-jump)
    (url . ,(get-text-property 0 'url (buffer-name)))))

(defun qutebrowser-bookmark-jump (bookmark)
  "Jump to a Qutebrowser BOOKMARK."
  (let ((url (bookmark-prop-get bookmark 'url)))
    (qutebrowser-open-url url)))

(defun qutebrowser-theme-export ()
  "Export selected Emacs faces to Qutebrowser theme format."
  (interactive)
  (with-temp-file "~/.config/qutebrowser/emacs_theme.py"
    (insert "# Qutebrowser theme exported from Emacs\n\n")
    (dolist (mapping qutebrowser-theme-export-face-mappings)
      (let* ((qute-face (symbol-name (car mapping)))
             (emacs-face (cdr mapping))
             (is-fg (string-match-p "\\.fg$" qute-face))
             (attribute (if is-fg :foreground :background))
             (color (face-attribute emacs-face attribute nil 'default))
             (hex-color (apply #'color-rgb-to-hex
                               (append (color-name-to-rgb color) '(2)))))
        (insert (format "c.colors.%s = '%s'\n" qute-face hex-color))))))

(defun qutebrowser-theme-export-and-apply (&rest _)
  "Export and apply theme to running Qutebrowser instance."
  (qutebrowser-theme-export)
  ;; TODO only if qutebrowser is running
  (qutebrowser-config-source "~/.config/qutebrowser/emacs_theme.py"))

;;;###autoload
(define-minor-mode qutebrowser-theme-export-mode
  "Minor mode to automatically export Emacs theme to Qutebrowser."
  :lighter nil
  :global t
  (if qutebrowser-theme-export-mode
      (advice-add 'enable-theme :after #'qutebrowser-theme-export-and-apply)
    (advice-remove 'enable-theme #'qutebrowser-theme-export-and-apply)))

(defun qutebrowser-fake-keys--escape (text)
  "Escape any special characters from TEXT to be sent to :fake-keys."
  (apply #'concat
   (mapcar (lambda (chr)
             (pcase chr
               (?< "<less>")
               (?> "<greater>")
               (?\" "\\\"")
               (?\' "'")
               (?\\ "\\\\")
               (_ (char-to-string chr))))
           text)))

(defun qutebrowser-fake-keys--raw (raw-keys)
  "Send RAW-KEYS without escaping special characters."
  (qutebrowser-send-commands (format ":fake-key %s" raw-keys)))

(defun qutebrowser-fake-keys (text)
  "Send TEXT as input to Qutebrowser."
  (let* ((escaped-text (qutebrowser-fake-keys--escape text)))
    (funcall #'qutebrowser-fake-keys--raw (format "\"%s\"" escaped-text))))

(defun qutebrowser-pass--find-matching (pattern)
  "Return list of password-store entries matching PATTERN."
  (cl-remove-if-not
   (lambda (entry)
     (string-match-p pattern entry))
   (password-store-list)))

;;;###autoload
(defun qutebrowser-pass (url)
  "Autofill username and password matching URL."
  (let* ((domain (url-domain (url-generic-parse-url url)))
         (pass-entries (qutebrowser-pass--find-matching domain))
         (selected (completing-read "Select: " pass-entries))
         (username (car (last (string-split selected "/"))))
         (password (password-store-get selected)))
    (qutebrowser-fake-keys username)
    (qutebrowser-fake-keys--raw "<Tab>")
    (qutebrowser-fake-keys password)
    (qutebrowser-fake-keys--raw "<Return>")))

;;;###autoload
(defun qutebrowser-pass-username-only (url)
  "Autofill username matching URL."
  (let* ((domain (url-domain (url-generic-parse-url url)))
         (pass-entries (qutebrowser-pass--find-matching domain))
         (selected (completing-read "Select: " pass-entries))
         (username (car (last (string-split selected "/")))))
    (qutebrowser-fake-keys username)
    (qutebrowser-fake-keys--raw "<Return>")))

;;;###autoload
(defun qutebrowser-pass-password-only (url)
  "Autofill password matching URL."
  (let* ((domain (url-domain (url-generic-parse-url url)))
         (pass-entries (qutebrowser-pass--find-matching domain))
         (selected (completing-read "Select: " pass-entries))
         (password (password-store-get selected)))
    (qutebrowser-fake-keys password)
    (qutebrowser-fake-keys--raw "<Return>")))

(defun qutebrowser--signal-mode-enter (mode)
  (run-hooks 'qutebrowser-mode-enter-hook))

(defun qutebrowser--signal-mode-leave (mode)
  (run-hooks 'qutebrowser-mode-leave-hook))

(defun qutebrowser--signal-new-window (win-id)
  (run-hooks 'qutebrowser-new-window-hook))

;; This triggers ~300 times (maybe once per line?)
(defun qutebrowser--signal-config-changed ()
  (run-hooks 'qutebrowser-config-changed-hook))

(defun qutebrowser--signal-url-changed (win-id url)
  (run-hooks 'qutebrowser-url-search-hook))

(defvar qutebrowser-current-search nil)

(defun qutebrowser--signal-got-search (search)
  (setq qutebrowser-current-search search)
  (run-hooks 'qutebrowser-got-search-hook))

(defun qutebrowser--get-process-pid ()
  "Return a list of PIDs for Qutebrowser processes."
  (cl-remove-if-not
   (lambda (pid)
     (let* ((attrs (process-attributes pid))
            (cmd (alist-get 'comm attrs)))
       (string= ".qutebrowser-re" cmd)))
   (list-system-processes)))

(defun qutebrowser--get-process-attribute (attr)
  (mapcar (lambda (pid)
            (alist-get attr (process-attributes pid)))
          (qutebrowser--get-process-pid)))

(defun qutebrowser--get-process-uptime ()
  "Return uptime in seconds of Qutebrowser process."
  (mapcar (lambda (pid)
            (time-convert (alist-get 'etime (process-attributes pid))
                          'integer))
          (qutebrowser--get-process-pid)))

(provide 'qutebrowser)

;;; qutebrowser.el ends here
