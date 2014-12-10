;; Turn off mouse interface early in startup to avoid momentary display
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Append /usr/local/bin to make emacs find aspell on a mac.
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Append /usr/texbin/pdflatex to make emacs find pdflatex on a mac.
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))

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


;; Confirm exit
(defun confirm-exit-emacs ()
        "ask for confirmation before exiting emacs"
        (interactive)
        (if (yes-or-no-p "Are you sure you want to exit? ")
                (save-buffers-kill-emacs)))

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

;; Keep emacs Custom-settings in separate file
;;(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
;;(load custom-file)

;; (require 'custom-init-file)
(require 'appearance)
(require 'sane-defaults)

;; Require and load a nice color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

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
(define-key global-map (kbd "M-s") 'ace-jump-mode)

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
;; fix some org-mode + yasnippet conflicts:
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; Zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; Ido mode
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
;; (add-hook 'before-save-hook 'cleanup-buffer-safe)
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

;; graphviz-dot-mode
(require 'graphviz-dot-mode)

;; Mac fix: move Meta-key to 'cmd'
(if (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))
(if (boundp 'ns-option-modifier)
    (setq ns-option-modifier nil))

;; Easy references in orgmode
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Binding for fill-region
(global-set-key (kbd "C-c f") 'fill-region)

;; Org export to latex command
;; (setq org-latex-to-pdf-process (list "latexmk -pdf -f %f"))
;; (setq org-latex-to-pdf-process '("pdflatex %f && bibtex %b && pdflatex %f && pdflatex %f"))
;; HARD CODED FILENAME FOR BIBTEX == NOT GOOD!
(setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex report" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f"))
(setq org-export-pdf-remove-logfiles nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote smtpmail-send-it))
 '(org-export-latex-classes (quote (("article" "\\documentclass[11pt]{article}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("beamer" "\\documentclass{beamer}" org-beamer-sectioning) ("exjobb" "\\documentclass[11pt]{report}" ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection*{%s}" . "\\subsection*{%s}") ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")))))
 '(tab-stop-list (quote (4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; fix some org-mode + yasnippet conflicts:
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("php"    . "\\.php\\."))
)

;; Tab width 4 !
(setq tab-width 4)
(setq default-tab-width 4)
>>>>>>> 60a7d220b5ca5796624e5c4eecbba6486cf36ae8
