;; elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
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

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(elpaca-wait)

;; end elpaca

(tool-bar-mode 0)
;; (cua-mode t)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(windmove-default-keybindings)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq windmove-wrap-around 't)

;; installed packages

;; (use-package jinx :ensure t :demand t)
(use-package rust-mode :ensure t :demand t)
(use-package markdown-mode :ensure t :demand t)
(use-package transient :ensure t)
;; (use-package magit :ensure t :demand t)
(use-package rainbow-delimiters :ensure t :demand t)

(use-package all-the-icons :ensure t :demand t
  :if (display-graphic-p))
(use-package all-the-icons-dired :ensure t :demand t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package golden-ratio :ensure t :demand t
  :config (setq golden-ratio-auto-scale t)
  :init (golden-ratio-mode 1))
(use-package golden-ratio-scroll-screen :ensure t :demand t)
(use-package company :ensure t :demand t)
;; (use-package beacon :ensure t :demand t
;;   :config
;;   (setq beacon-size 70)
;;   :init (beacon-mode 1))

(use-package projectile :ensure t :demand t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :init (projectile-mode t))

(use-package multiple-cursors :ensure t :demand t
  :init (require 'multiple-cursors)
  ;; When you have an active region that spans multiple lines, the
  ;; following will add a cursor to each line:
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  ;; When you want to add multiple cursors not based on continuous
  ;; lines, but based on keywords in the buffer, use:
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package doom-themes :ensure t :demand t
  :init
  ;; set theme
  (if (display-graphic-p)
      (load-theme 'doom-Iosvkem'yes)))

(use-package move-text
  :ensure t :demand t
  :config
  (move-text-default-bindings))
(use-package dumb-jump
  :ensure t :demand t
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package helm
  :ensure t :demand t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (helm-mode 1))

(use-package which-key
  :ensure t :demand t
  :config
  (which-key-mode))

;; yasnippet config.
(use-package yasnippet
  :ensure t :demand t
  :init
  (require 'yasnippet)
  (yas-global-mode 1)
  :config
  (add-to-list 'load-path
	       "~/.emacs.d/plugins/yasnippet"))

(use-package flycheck :ensure t :demand t
  :config
  (global-flycheck-mode)
  )

(use-package lsp-mode :ensure t :demand t
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (python-ts-mode . lsp)
	 (csharp-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t :demand t
  :commands lsp-ui-mode)
(use-package helm-lsp :ensure t :demand t
  :commands helm-lsp-workspace-symbol)

;;(require 'which-key) (which-key-mode 1)
(global-whitespace-mode 1)

;; end installed packages

;; change mode to another mode (kind of).
;; It is needed for tree-sitter. It changes automatically
;; launched mode from one to another, tree-sitter for example
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

;; Function definitions
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; Keybindings
(global-set-key [f5] 'revert-buffer-no-confirm)
(global-set-key "\M-o" 'other-window)
(add-hook 'prog-mode-hook   (lambda () (local-set-key [f9] 'compile)))
(add-hook 'dired-mode-hook  (lambda () (local-set-key [f9] 'compile)))

;; Hooks
;; (add-hook 'elpaca-after-init-hook #'global-jinx-mode)

(add-hook `prog-mode-hook `display-fill-column-indicator-mode)
;; (add-hook `prog-mode-hook `prettify-symbols-mode) ;; cringe symbols for words
;; (add-hook `prog-mode-hook `eglot-ensure) ;; lsp for programming languages
(add-hook `prog-mode-hook `rainbow-delimiters-mode) ;; Better paren
    ;; highlighting for programming languages.
(add-hook `elpaca-after-init-hook `global-company-mode) ;; Company


;; set 'exec-path' to match shell PATH automatically
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


;; Ligatures.. :/ :: -> <->
(when (window-system)
  (set-frame-font "Fira Code"))
;;(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;               (36 . ".\\(?:>\\)")
;;               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;               (48 . ".\\(?:x[a-zA-Z]\\)")
;;               (58 . ".\\(?:::\\|[:=]\\)")
;;               (59 . ".\\(?:;;\\|;\\)")
;;               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;               (91 . ".\\(?:]\\)")
;;               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;               (94 . ".\\(?:=\\)")
;;               (119 . ".\\(?:ww\\)")
;;               (123 . ".\\(?:-\\)")
;;               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;               )
;;             ))
;;  (dolist (char-regexp alist)
;;    (set-char-table-range composition-function-table (car char-regexp)
;;                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(package-initialize)
(put 'downcase-region 'disabled nil)
;;(custom-set-variables
;; ;; custom-set-variables was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(custom-safe-themes
;;   '("b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
;; '(package-selected-packages
;;   '(jinx scala-ts-mode markdown-mode magit helm yasnippet which-key rainbow-delimiters multiple-cursors move-text golden-ratio-scroll-screen golden-ratio dumb-jump doom-themes company beacon)))
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" default))
 '(package-selected-packages
   '(eglot yasnippet which-key scala-ts-mode rainbow-delimiters projectile multiple-cursors move-text markdown-mode magit jinx helm golden-ratio-scroll-screen golden-ratio dumb-jump doom-themes company beacon)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; jinx-languages: "en_US"
;; End:
