;; Theme
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Here are some functions/macros that will help you configure Doom.
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

;; Load config files in settings subdir.
(load! "lib/inbox.el")

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

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
          ("IDEA" . "#9B30FF")
          )))

(setq neo-hidden-regexp-list '("^\\."))

(setq neo-smart-open t)

(after! org
  (add-to-list 'org-capture-templates
               '("i" "Inbox item" entry
                 (file+headline "~/org/inbox.org" "Inbox")
                 "** INBOX %?\n"))
  (add-to-list 'org-capture-templates
               '("d" "Inbox item" entry
                 (file+headline "~/org/inbox.org" "Inbox")
                 "** DOING %?\n"))
  (add-to-list 'org-capture-templates
               '("n" "Inbox item" entry
                 (file+headline "~/org/inbox.org" "Inbox")
                 "** NEXT %?\n"))
  )

(after! org-agenda
  (map! :map org-agenda-mode-map
        "<escape>" #'org-agenda-exit))

(after! magit
  (map! :map magit-mode-map
        "<escape>" #'magit-mode-bury-buffer))

;; Disable the system clipboard
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

;; Bind cmd+v to the paste function
(global-set-key (kbd "s-v") 'paste-from-system-clipboard)

;; Bind cmd+c to the copy function
(global-set-key (kbd "s-c") 'copy-region-to-system-clipboard)

;; SPC keybindings.
(map! :leader
      :desc "Show DOING items" "d d" (lambda () (interactive) (org-todo-list "DOING"))
      :desc "Show NEXT items" "d n" (lambda () (interactive) (org-todo-list "NEXT"))
      :desc "Open Emacs configuration todo list" "d e" #'org-emacs-todo-list
      "d i" (lambda () (interactive) (find-file "~/org/inbox.org"))
      :desc "Find files in homedir" "f j" (lambda () (interactive) (counsel-find-file "~"))
      :desc "Find files in homedir" "f k" #'counsel-fzf
      :desc "Neotree change root to homedir" "f K" (lambda () (interactive) (neotree-dir "~"))
      "f h" #'neotree-find
      "f n" #'neotree-toggle
      :desc "Capture note to inbox as INBOX" "d i" (lambda () (interactive) (org-capture nil "i"))
      :desc "Capture note to inbox as DOING" "d D" (lambda () (interactive) (org-capture nil "d"))
      :desc "Capture note to inbox as NEXT" "d N" (lambda () (interactive) (org-capture nil "n"))
      "d f" #'org-refile
      :desc "Edit emacs config" "d ," (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :desc "Dired" "SPC" #'dired
      :desc "Magit commit -m 'Update'" "g k" #'magit-commit-update
      )

;; Non-SPC key bindings.
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
