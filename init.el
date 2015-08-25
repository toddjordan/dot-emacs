(require 'package)
(setq package-archives '(("marmalade" . "https://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(better-defaults
                      paredit
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      magit-popup
                      scpaste
                      auto-complete
                      clojure-mode
                      rainbow-delimiters
                      yasnippet
                      ac-nrepl
                      ac-emmet
                      ac-js2
                      js-comint
                      js2-mode
                      js2-refactor
                      web-beautify
                      less-css-mode
                      scss-mode
                      flycheck
                      web-mode
                      web-beautify
                      coffee-mode
                      less-css-mode
                      scss-mode
                      feature-mode
                      cider
                      cider-decompile
                      cider-eval-sexp-fu
                      cider-profile
                      cider-spy
                      clojurescript-mode
                      company
                      emmet-mode
                      markdown-mode
                      zenburn-theme
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; ido mode customization
;; disables find file from looking outside current dir
(setq ido-auto-merge-work-directories-length -1)


(defun set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
    (progn
      ;; use 120 char wide window for largeish displays
      ;; and smaller 80 column windows for smaller displays
      ;; pick whatever numbers make sense for you
      (if (> (x-display-pixel-width) 1500)
          (add-to-list 'default-frame-alist (cons 'width 200))
        (add-to-list 'default-frame-alist (cons 'width 80)))
      ;; for the height, subtract a couple hundred pixels
      ;; from the screen height (for panels, menubars and
      ;; whatnot), then divide by the height of a char to
      ;; get the height we want
      (add-to-list 'default-frame-alist 
                   (cons 'height (/ (- (x-display-pixel-height) 200)
                                    (frame-char-height)))))))

(set-frame-size-according-to-resolution)


(menu-bar-mode)
(tool-bar-mode)
(when (display-graphic-p) (scroll-bar-mode))
(when (not (display-graphic-p))(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode))
(when (display-graphic-p) (x-focus-frame nil))
(global-linum-mode 1)
(global-prettify-symbols-mode +1)
(global-set-key [backspace] 'delete-backward-char)


;; (setq magit-last-seen-setup-instructions "1.4.0")
;; (setq magit-highlight-whitespace nil)
(global-set-key (kbd "C-c g") 'magit-status)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(custom-safe-themes
   (quote
    ("2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default)))
 '(inhibit-startup-screen t)
 '(magit-commit-arguments nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (load-theme 'tsdh-dark)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn)

;;; NeoTree
(add-to-list 'load-path "/Users/jordanto/dev/emacs/emacs-neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; (defun neotree-project-dir ()
;;   "Open NeoTree using the git root."
;;   (interactive)
;;   (let ((project-dir (ffip-project-root))
;;         (file-name (buffer-file-name)))
;;     (if project-dir
;;         (progn
;;           (neotree-dir project-dir)
;;           (neotree-find file-name))
;;       (message "Could not find git project root."))))
;; (define-key map (kbd "C-c C-p") 'neotree-project-dir)

;;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; feature-mode
(setq feature-default-languate "fi")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;; js-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js2-highlight-level 3)
;; (setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-missing-semi-one-line-override t)
(setq js2-strict-missing-semi-warning nil)


;; ;; tern auto complete
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; js turn function into f
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))

;;; paredit
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'js2-mode-hook 'my-paredit-nonlisp) ;use with the above function
(add-hook 'coffee-mode-hook 'my-paredit-nonlisp)
(electric-pair-mode)

;;; jshint with flycheck
(require 'flycheck)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))

;;; coffeescript
(add-hook 'coffee-mode-hook
          (lambda () (flycheck-mode t)))


;;; auto complete mod
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
(ac-config-default)


;;; js refactor
(require 'js2-refactor)

;;; paredit mode
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for web mode."
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

)
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; My Coffee AC Source
(add-to-list 'load-path "/Users/jordanto/dev/emacs/ac-coffee")
(require 'ac-coffee)

(eval-after-load 'js
  '(define-key js-mode-map "{" 'paredit-open-curly))
(eval-after-load 'js
  '(define-key js-mode-map "}" 'paredit-close-curly-and-newline))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; CIDER

(require 'cider-mode)

;;(global-company-mode)
;;(add-hook 'after-init-hook 'global-company-mode)

(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)

;;(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-stacktrace-default-filters '(java repl tooling dup))
;;(setq cider-repl-display-in-current-window t)
(setq cider-switch-to-repl-command #'cider-switch-to-current-repl-buffer)

;; switch current buffer into repl
(setq cider-repl-display-in-current-window t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

(show-paren-mode 1)

;; popup contextual docs
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Clojure Coding Standards

(require 'whitespace)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 84)

;; indent hiccup, expectations,
(define-clojure-indent
  (expect 'defun)
  (expect-let 'defun)
  (given 'defun)
  (context 1)
  (freeze-time 1)
  (redef-state 1)
  (from-each 1)
  (component 'defun)
  (div 'defun)
  (span 'defun)
  (form 'defun)
  (a 'defun)
  (ul 'defun)
  (li 'defun)
  (input 'defun)
  (h1 'defun)
  (h2 'defun)
  (h3 'defun)
  (h4 'defun)
  (h5 'defun)
  (h6 'defun)
  (button 'defun)
  (textarea 'defun))

(setq-default fill-column 80)

;; SCSS
(require 'scss-mode)
(setq scss-sass-command "node-sass")
(setq-default scss-compile-at-save nil)

;; Emmet
(require 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'html-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)
