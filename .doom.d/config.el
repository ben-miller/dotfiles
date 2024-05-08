;; Load config files in settings subdir.
(load! "lib/inbox.el")
(load! "lib/org-config.el")

;; Theme.
(setq doom-theme 'doom-one-light)

;; Disable line numbers.
(setq display-line-numbers-type nil)

;; Org directory.
(setq org-directory "~/org/")

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

(setq leetcode-prefer-language "java")

(use-package! expand-region
  :bind ("M-k" . er/expand-region)
  :bind ("M-j" . er/contract-region)
  )

(setq neo-hidden-regexp-list '("^\\."))

(setq neo-smart-open t)

(after! magit
  (map! :map magit-mode-map
        "<escape>" #'magit-mode-bury-buffer))

;; Disable the system clipboard
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

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
      "s-v" #'paste-from-system-clipboard
      "s-c" #'copy-region-to-system-clipboard
      )
