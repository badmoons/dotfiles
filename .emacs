;; My emacs config.

(menu-bar-mode 0)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'ido) (ido-mode t)
;; remove starting screen
(setq org-html-validation-link nil)
(setq inhibit-startup-screen t)
(require 'golden-ratio) (golden-ratio-mode t)
(require 'rainbow-delimiters) (rainbow-delimiters-mode t)

;; TREE-SITTER

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)))

;; MOVE BACKUPS WHERE THEY BELONG
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; Hooks
(add-hook `prog-mode-hook `eglot-ensure) ;; LSP for programming langs
(add-hook `prog-mode-hook `rainbow-delimiters-mode)
(add-hook `after-init-hook `global-company-mode) ;; Company

(global-set-key (kbd "M-x") 'smex) ;; Smex
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
;;(when (window-system)
;;  (set-frame-font "Fira Code"))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" "f25f174e4e3dbccfcb468b8123454b3c61ba94a7ae0a870905141b050ad94b8f" default))
 '(dired-listing-switches "-alh")
 '(dired-use-ls-dired t)
 '(display-line-numbers-type 'visual)
 '(font-use-system-font t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(rainbow-delimiters golden-ratio smex htmlize which-key rust-mode magit company multiple-cursors eglot markdown-mode dracula-theme))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tool-bar-position 'left))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Unifont" :foundry "PfEd" :slant normal :weight medium :height 98 :width normal)))))
(put 'upcase-region 'disabled nil)
