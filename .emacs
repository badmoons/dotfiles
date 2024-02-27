(pixel-scroll-precision-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)
(tool-bar-mode 0)
(cua-mode t)
(scroll-bar-mode 0)
(menu-bar-mode 0)

(use-package move-text
  :config
  (move-text-default-bindings))
(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; pretty
;; (use-package beacon
;;   :init (beacon-mode 1))
(require 'rainbow-delimiters)
(require 'which-key) (which-key-mode 1)
(require 'golden-ratio) (golden-ratio-mode t)

;; set theme
(require 'doom-themes)
(if (display-graphic-p)
    (load-theme 'doom-dracula 'yes)
  )

;; easier ui
;; (require 'ido) (ido-mode t) ;;
;; (require 'smex) (smex-initialize)
   ;; smex keybindings
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-x") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package helm-mode
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (helm-mode 1))

(use-package which-key
  :config
    (which-key-mode))

;; yasnippet config. It works somehow, even though I don't understand `use-package`
(use-package yasnippet
  :init
  (require 'yasnippet)
  (yas-global-mode 1)
  :config
  (add-to-list 'load-path
	       "~/.emacs.d/plugings/yasnippet"))

;; change mode to another mode (kind of). It is needed for tree-sitter. It changes automatically launched mode from one to another, tree-sitter for example
(add-to-list 'major-mode-remap-alist
	     '(python-mode . python-ts-mode)
	     '(c-mode . c-ts-mode))

;; 80 character column
(setq display-fill-column-indicator-column 80)
(setq markdown-fontify-code-blocks-natively t)

;; remove starting screen
(setq inhibit-startup-screen t)

;; MOVE BACKUPS WHERE THEY BELONG
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; Associate a mode with a file
(add-to-list 'auto-mode-alist '("\\.lpr\\'" . opascal-mode))

;; Function defenitions
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; Keybindings
(global-set-key [f5] 'revert-buffer-no-confirm)
(add-hook 'prog-mode-hook   (lambda () (local-set-key [f9] 'compile)))
(add-hook 'dired-mode-hook  (lambda () (local-set-key [f9] 'compile)))

;; Hooks
(add-hook `prog-mode-hook `display-fill-column-indicator-mode)
(add-hook `prog-mode-hook `prettify-symbols-mode)
(add-hook `prog-mode-hook `eglot-ensure) ;; lsp for programming languages
(add-hook `prog-mode-hook `rainbow-delimiters-mode) ;; Better paren highlighting for programming languages.
(add-hook `after-init-hook `global-company-mode) ;; Company

;; Multiple cursros
(require 'multiple-cursors)

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; set 'exec-path' to match shell PATH autmatically
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell."

  (interactive) 
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


;; Ligatures. Fng finally. :/ :: -> <->
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(package-selected-packages
   '(magit helm yasnippet which-key rainbow-delimiters multiple-cursors move-text golden-ratio-scroll-screen golden-ratio dumb-jump doom-themes company beacon)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
