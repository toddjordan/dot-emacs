(require 'package)
(setq package-archives '(("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
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
                      ac-cider
                      ac-nrepl
                      ac-emmet
                      ac-js2
                      js-comint
                      js2-mode
                      js2-refactor
                      json-mode
                      magit
                      web-beautify
                      less-css-mode
                      scss-mode
                      flycheck
                      web-mode
                      web-beautify
                      coffee-mode
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
                      editorconfig
                      find-file-in-project
                      ember-mode
                      projectile
                      nyan-mode
                      flx-ido
                      toggle-quotes
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

;; General Config
(column-number-mode)
(menu-bar-mode)
(tool-bar-mode)
(global-hl-line-mode)
(desktop-save-mode 1)
(when (display-graphic-p) (scroll-bar-mode))
(when (not (display-graphic-p))(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode))
(when (display-graphic-p) (x-focus-frame nil))
(global-linum-mode 1)
(global-prettify-symbols-mode +1)
(global-set-key [backspace] 'delete-backward-char)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(setq tab-width 4)
(setq ispell-program-name "/usr/local/bin/aspell")

;; increase/decrease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; save backups to system temp, and disable lock files
;; this is so ember-cli/watchman does not get messed up
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(add-hook 'ibuffer-hook
          '(lambda ()
             (ibuffer-vc-set-filter-groups-by-vc-root)
             (ibuffer-auto-mode 1)))
(setq ibuffer-default-sorting-mode 'major-mode)

(require 'yasnippet)
(yas-reload-all)

;; Magit config
;; (setq magit-last-seen-setup-instructions "1.4.0")
;; (setq magit-highlight-whitespace nil)
(global-set-key (kbd "C-c g") 'magit-status)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(cider-auto-select-test-report-buffer t)
 '(cider-test-show-report-on-success t)
 '(custom-safe-themes
   (quote
    ("0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "c4465c56ee0cac519dd6ab6249c7fd5bb2c7f7f78ba2875d28a50d3c20a59473" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default)))
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(magit-commit-arguments nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-startup-truncated nil)
 '(safe-local-variable-values
   (quote
    ((web-mode-css-indent-offset . 4)
     (web-mode-code-indent-offset . 4)
     (web-mode-markup-indent-offset . 4))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (load-theme 'tsdh-dark)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn)

;;;ido
(ido-mode 1)
(ido-everywhere 1)

;;; Find File in Project
(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project-by-selected" nil t)
(autoload 'find-directory-in-project-by-selected "find-directory-in-project-by-selected" nil t)

;;; Ergonomics
;; While typing, I want to upcase/downcase/capitalize words I just typed (especially since I've mapped caps lock to Ctl ;-)
;; M-u
(defun upcase-word--upcase-word-advice (count)
  (unless (and (looking-back "\\b") (not (= (point) (line-end-position))))
    (backward-word)))
(advice-add 'upcase-word :before #'upcase-word--upcase-word-advice)

;; M-l
(defun downcase-word--downcase-word-advice (count)
  (unless (and (looking-back "\\b") (not (= (point) (line-end-position))))
    (backward-word)))
(advice-add 'downcase-word :before #'downcase-word--downcase-word-advice)

;; M-c
(defun capitalize-word--capitalize-word-advice (count)
    (unless (and (looking-back "\\b") (not (= (point) (line-end-position))))
      (backward-word)))
(advice-add 'capitalize-word :before #'capitalize-word--capitalize-word-advice)

;;; Org Mode
(add-hook 'org-mode-hook 'yas-minor-mode)
(setq org-agenda-files (list "~/dev/notebook/"
                             "~/dev/notebook/teams"
                             "~/dev/notebook/presentations"
                             "~/dev/notebook/projects"
                             "~/dev/notebook/notes"
                             "~/dev/notebook/writing"
                             "~/dev/notebook/events"))
(setq org-tag-alist '(("projects" . ?p)
                      ("notes" . ?n)
                      ("purecloud" . ?c)
                      ("presentations" . ?r)
                      ("oss" . ?o)
                      ("teams" . ?t)
                      ("ember" . ?e)))

;;; NeoTree
(add-to-list 'load-path "/Users/jordanto/dev/emacs/emacs-neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

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

;;; nyan-mode
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)
(setq nyan-wavy-trail t)

;;; flx ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;; projectile
(require 'projectile)
(projectile-mode)

;;; feature-mode
(setq feature-default-languate "fi")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;; js-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js2-mode-hook 'yas-minor-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js2-highlight-level 3)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-missing-semi-one-line-override t)
(setq js2-strict-missing-semi-warning nil)

(add-hook 'json-mode-hook (lambda ()
                           (make-local-variable 'js-indent-level)
                           (setq js-indent-level 2)))


;; js turn function into f
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))
;;; tern
(add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

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
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; eslint --fix on save
(eval-after-load 'js2-mode
       '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;;; ember-mode
(add-to-list 'load-path "~/.emacs.d/ember-mode/")
(require 'ember-mode)
(add-hook 'js-mode-hook (lambda () (ember-mode t)))
(add-hook 'web-mode-hook (lambda () (ember-mode t)))

;;; coffeescript
(setq coffee-tab-width 4)
(add-hook 'coffee-mode-hook
          (lambda () (flycheck-mode t)))

;;; auto complete mod
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
(ac-config-default)

;;; js refactor
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

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
;; (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;; CIDER
(require 'cider-mode)

;; (add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;(global-company-mode)
;;(add-hook 'after-init-hook 'global-company-mode)

(setq nrepl-log-messages t)
;; (setq nrepl-hide-special-buffers t)

;; (setq cider-repl-pop-to-buffer-on-connect nil)
;; (setq cider-show-error-buffer 'except-in-repl)
;; (setq cider-stacktrace-default-filters '(java repl tooling dup))
(setq cider-switch-to-repl-command #'cider-switch-to-current-repl-buffer)
(setq cider-test-show-report t)
(cider-auto-test-mode 1)

;; switch current buffer into repl
(setq cider-repl-display-in-current-window t)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; (add-hook 'cider-mode-hook #'company-mode)
;; (add-hook 'cider-repl-mode-hook #'company-mode)

(show-paren-mode 1)

;; popup contextual docs
;; (eval-after-load "cider"
;;   '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; Clojure Coding Standards
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

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

(setq-default fill-column 200)

;; SCSS
(require 'scss-mode)
(setq scss-sass-command "node-sass")
(setq-default scss-compile-at-save nil)
(setq css-indent-offset 2)

;; Emmet
(require 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'html-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'scss-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)

;; whitespace
(require 'whitespace)
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'js2-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'coffee-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'scss-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 160)
(setq whitespace-action '(auto-cleanup))
(put 'erase-buffer 'disabled nil)

;; toggle-quotes
(require 'toggle-quotes)
(global-set-key (kbd "C-'") 'toggle-quotes)
