;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

(defun open-scratch-in-new-tab ()
  "Open a new tab with a *scratch* buffer."
  (interactive)
  (tab-new)
  (switch-to-buffer "*scratch*"))

(defun split-and-balance-windows-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (select-window (next-window)))

(defun split-and-balance-windows-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (select-window (next-window)))

(defun close-window-or-tab ()
  (interactive)
  (if (one-window-p)
      (tab-close)
    (progn
      (delete-window)
      (balance-windows))
    ))

(defun my-list-windows ()
  "List all windows in the current tab along with their widths."
  (interactive)
  (let ((window-info '()))
    (walk-windows
     (lambda (w)
       (push (format "%s (width: %d)" (buffer-name (window-buffer w)) (window-width w)) window-info))
     nil t)
    (message "Windows in current tab: %s" (mapconcat 'identity window-info ", "))))

(defun window-is-maximized ()
  "Check if any window in the current tab has a width under 16 characters."
  (cl-some (lambda (w) (< (window-width w) 16))
           (window-list)))

(defun toggle-maximize-window ()
  "Toggle the maximization state of the current window."
  (interactive)
  (if (window-is-maximized)
      (balance-windows)    ; If the window is maximized, balance the windows.
      (maximize-window)))  ; If the window is not maximized, maximize it.

(defun move-and-maybe-maximize (move-fn)
  "Move using the lambda function MOVE-FN and maximize if the window is already maximized."
  (funcall move-fn)
  (when (window-is-maximized)
    (maximize-window)))

(map! :map global-map
      "s-S-<right>" #'tab-bar-move-tab
      "s-S-<left>" #'tab-bar-move-tab-backward
      "s-<right>" #'tab-bar-switch-to-next-tab
      "s-<left>" #'tab-bar-switch-to-prev-tab
      "s-t" #'open-scratch-in-new-tab
      "s-w" #'tab-bar-close-tab
      "s-h" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-left))))
      "s-l" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-right))))
      "s-k" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-up))))
      "s-j" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-down))))
      "s-d" #'split-and-balance-windows-vertically
      "s-D" #'split-and-balance-windows-horizontally
      "s-w" #'close-window-or-tab
      "s-[" #'previous-buffer
      "s-]" #'next-buffer
      "s-K" #'toggle-maximize-window
      "s-," (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      "<backtab>" #'neotree-collapse-all
      "s-." #'neotree-hidden-file-toggle
      "C-/" #'swiper
      )

(map! :leader
      :desc "Show DOING items" "d d" #'org-doing-list
      :desc "Open Emacs configuration todo list" "d e" #'org-emacs-todo-list
      :desc "Find files in homedir" "f j" (lambda () (interactive) (counsel-find-file "~"))
      :desc "Find files in homedir" "f k" #'counsel-fzf
      :desc "Neotree change root to homedir" "f K" (lambda () (interactive) (neotree-dir "~"))
      "f h" #'neotree-find
      "f n" #'neotree-toggle
      )

(setq leetcode-prefer-language "java")

(use-package! expand-region
  :bind ("M-k" . er/expand-region)
  :bind ("M-j" . er/contract-region)
  )

(after! org
  (setq org-todo-keyword-faces
        '(("INBOX" . "#1E90FF")
          ("DOING" . "#FF8C00")
          ("NEXT" . "#32CD32")
          ("BUG" . "#EE4B2B")
          )))

(defun org-emacs-todo-list ()
    (interactive)
  (find-file "~/org/emacs.org"))

(defun org-doing-list ()
  (interactive)
  (let ((buffer-name "*Org Agenda*"))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name)
      (let ((org-agenda-files (org-agenda-files)))
        (org-todo-list "DOING")))))

(setq neo-hidden-regexp-list '("^\\."))

(setq neo-smart-open t)

