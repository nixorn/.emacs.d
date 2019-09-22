;; General settings

(require 'package)

(require 'macros)

(global-set-key "\C-z" 'undo)
(global-set-key (kbd "C-M-x C-M-f") 'find-file-at-point)
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


(setq haskell-compile-cabal-build-command "stack install --test --pedantic")
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
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (web-beautify multiple-cursors dhall-mode git-gutter magit lsp-haskell)))
 '(sql-connection-alist
   (quote
    (("local"
      (sql-product
       (quote postgres))
      (sql-user "nixorn")
      (sql-database "getshoptv_source")
      (sql-server "localhost")))))
 '(tab-width 2))





(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
