;; Turn off mouse interface early in startup to avoid momentary display
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Keep emacs Custom-settings in separate file
;;(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
;;(load custom-file)

;; Load some stuff instantly on startup
(require 'custom-init-file)
(require 'appearance)
(require 'sane-defaults)

;; Require and load a nice color-theme
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;  '(progn
;;     (color-theme-initialize)
;;     (color-theme-hober)))

;; Load modes when idle
;; Collaboration
(require 'rudel-loaddefs)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Show undo history as a tree
(require 'undo-tree)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)

;; Save point position between sessions
(require 'saveplace)
(eval-after-load "saveplace"
  '(progn
     (setq-default save-place t)
     (setq save-place-file (expand-file-name ".places" dotfiles-dir))))

;; Fill column indicator
(require 'fill-column-indicator)
(eval-after-load "fill-column-indicator"
  '(progn
     (setq fci-rule-color "#111122")
     (fci-mode 1)))


;; Ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-s") 'ace-jump-mode)

;; Browse kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Mark multiple
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(require 'mark-more-like-this)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)

;; YaSnippet
(require 'yasnippet)
(setq yas/snippet-dirs "~/.emacs.d/snippets")
(yas/global-mode 1)

;; Zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Magit
(require 'magit)

;; js2 mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Apache mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; Buffer functions
(require 'buffer-defuns)
(add-hook 'before-save-hook 'cleanup-buffer-safe)
(global-set-key (kbd "C-c C-b") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'create-scratch-buffer)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Some settings
(require 'my-misc)
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key (kbd "M-S-<down>") 'move-line-down)
(global-set-key (kbd "M-S-<up>") 'move-line-up)

;; Zoom frame
(require 'zoom-frm)
(global-set-key (kbd "C-x C-+") 'zoom-in)
(global-set-key (kbd "C-x C--") 'zoom-out)
(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)
