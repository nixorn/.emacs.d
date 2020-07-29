;; General settings

(require 'package)

(require 'macros)

(global-set-key "\C-z" 'undo)
(global-set-key (kbd "M-g") 'rgrep)
(global-set-key (kbd "\C-x i") 'previous-multiframe-window)
(setq tramp-default-method "ssh")
(global-set-key (kbd "C-M-?") 'xref-find-references)
(global-set-key (kbd "C-M-x C-M-f") 'find-file-at-point)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
(setq-default column-number-mode t)
(show-paren-mode t)

(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(global-set-key (kbd "C-x C-r") 'replace-last-sexp)


;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Haskell

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(add-hook 'haskell-mode-hook #'haskell-collapse-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(global-set-key "\C-t" 'haskell-hide-toggle)
(setq haskell-compile-cabal-build-command "stack install --test")
;; (setq haskell-compile-cabal-build-command "nix-shell --command 'cabal build' --attr env release.nix")
;; (setq haskell-compile-cabal-build-command "NIXPKGS_ALLOW_BROKEN=1 nix-build")
(setq-default show-trailing-whitespace t)
(setq-default haskell-tags-on-save t)
(setq-default tags-revert-without-query t)

;;;;;;;;;;;

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(dhall-format-at-save nil)
 '(dhall-type-check-inactivity-timeout 5000)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (cargo php-mode typescript-mode paredit web-beautify multiple-cursors dhall-mode git-gutter magit lsp-haskell)))
 '(sql-connection-alist
   (quote
    (("local"
      (sql-product
       (quote postgres))
      (sql-user "nixorn")
      (sql-database "getshoptv_source")
      (sql-server "localhost")))))
 '(tab-width 2)
 '(typescript-indent-level 2)
 '(version-control (quote never)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))

;; disable the menu bar
(menu-bar-mode -1)
;; disable the scrollbar
(toggle-scroll-bar -1)
;; disable the toolbar
(tool-bar-mode -1)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; gutter
(global-git-gutter-mode +1)


;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(put 'upcase-region 'disabled nil)
(require 'ox-md)


;; Dhall
(add-hook 'dhall-mode-hook
      (lambda ()
        (make-local-variable 'indent-tabs-mode)
				(setq indent-tabs-mode nil)))


;; Paredit

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'dhall-mode-hook       #'enable-paredit-mode)
(add-hook 'haskell-mode-hook       #'enable-paredit-mode)
(add-hook 'rust-mode-hook       #'enable-paredit-mode)
(add-hook 'python-mode-hook       #'enable-paredit-mode)
(global-set-key "{" 'paredit-open-curly)


;; SQL beytify

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "pg_format" nil t)))
    ;; change sqlbeautify to anbt-sql-formatter if you
    ;;ended up using the ruby gem

(defun sql-beautify-buffer ()
 "Beautify SQL in buffer."
 (interactive)
 (sql-beautify-region (point-min) (point-max)))
(put 'downcase-region 'disabled nil)

;; split windows side-by-side
(setq split-height-threshold nil)
(setq split-width-threshold 70)
