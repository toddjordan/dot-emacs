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
                      ox-gfm
                      tide
                      editorconfig
                      helm-projectile
                      company-tern
		      neotree
		      all-the-icons
		      smex
		      eslint-fix
		      git-timemachine
		      ox-reveal
		      prettier-js
		      vlf
		      powerline
		      diminish
		      ibuffer-vc))

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

;; hide stuff from modeline
(require 'diminish)
(add-to-list 'emacs-startup-hook
             (lambda ()
	       (diminish 'company-mode)
	       (diminish 'editorconfig-mode)
	       (diminish 'eldoc-mode)
	       (diminish 'paredit-mode)
	       (diminish 'whitespace-mode)
	       (diminish 'tern-mode)
	       (diminish 'ember-mode)
	       (diminish 'auto-complete-mode)))

;; General Config
(column-number-mode)
(menu-bar-mode)
(tool-bar-mode)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222")
(set-face-attribute 'region nil :background "#666")
;;; desktop save mode: save your open buffers so when you close and ropen
;;;(desktop-save-mode 1)
(when (display-graphic-p) (scroll-bar-mode))
(when (not (display-graphic-p))(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode))
(when (display-graphic-p) (x-focus-frame nil))

;;; support mouse functions in iterm
(defvar alternating-scroll-down-next t)
(defvar alternating-scroll-up-next t)

(defun alternating-scroll-down-line ()
  (interactive "@")
    (when alternating-scroll-down-next
;      (run-hook-with-args 'window-scroll-functions )
      (scroll-down-line))
    (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

(defun alternating-scroll-up-line ()
  (interactive "@")
    (when alternating-scroll-up-next
;      (run-hook-with-args 'window-scroll-functions)
      (scroll-up-line))
    (setq alternating-scroll-up-next (not alternating-scroll-up-next)))

(when (not (display-graphic-p))
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
	(setq mouse-wheel-follow-mouse 't)
	
	(global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
	(global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)

)


(global-linum-mode 1)
(global-prettify-symbols-mode +1)
(global-set-key [backspace] 'delete-backward-char)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(setq tab-width 2)
(setq ispell-program-name "/usr/local/bin/aspell")
(global-set-key (kbd "C-c j") 'just-one-space)

;; increase/decrease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(set-face-attribute 'default nil :font "Menlo 16")

;; save backups to system temp, and disable lock files
;; this is so ember-cli/watchman does not get messed up
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List Buffers." t)
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

;; (load-theme 'tsdh-dark)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

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
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-directory "~/dev/notebook")
;;(add-to-list 'org-file-apps '(directory . emacs))
(setq org-agenda-files (list "~/dev/notebook/"
                             "~/dev/notebook/teams"
                             "~/dev/notebook/presentations"
                             "~/dev/notebook/projects"
                             "~/dev/notebook/notes"
                             "~/dev/notebook/writing"
                             "~/dev/notebook/events"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
	(org-agenda-files :maxlevel . 3)))
(setq org-tag-alist '(("projects" . ?p)
                      ("notes" . ?n)
                      ("purecloud" . ?c)
		      ("q2" . ?q)
                      ("presentations" . ?r)
                      ("oss" . ?o)
                      ("teams" . ?t)
                      ("ember" . ?e)))

;; Org Capture
(setq org-default-notes-file (concat org-directory "/notes/notes.org"))
;; C-c c is for capture, and that's good enough for me
(define-key global-map "\C-cc" 'org-capture)
;; force UTF-8
(setq org-export-coding-system 'utf-8)

(setq org-capture-templates
      '(("t" "Todo list item"
	 entry (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\n %i\n %a")
       ("j" "Journal Entry"
	entry (file+datetree "~/dev/notebook/notes/journal.org")
	(file "~/.emacs.d/org-templates/journal.orgcaptmpl"))
       ("b" "Tidbit: quote, zinger, one-liner or textlet"
	entry (file+headline org-default-notes-file "Tidbits")
	(file "~/.emacs.d/org-templates/tidbits.orgcaptmpl")))
)

;; load github markdown export
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;; NeoTree
(add-to-list 'load-path "/Users/jordanto/dev/emacs/emacs-neotree")
(require 'neotree)
(require 'all-the-icons)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

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
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; powerline
(require 'powerline)
(powerline-default-theme)

;;; helm smex
(require 'helm-smex)
(global-set-key [remap execute-extended-command] #'helm-smex)
(global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)

;;; nyan-mode
;; (require 'nyan-mode)
;; (nyan-mode)
;; (nyan-start-animation)
;; (setq nyan-wavy-trail t)

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

;;; helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;;; helm-projectile
;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)

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

;; (add-hook 'json-mode-hook (lambda ()
;;                            (make-local-variable 'js-indent-level)
;;                            (setq js-indent-level 2)))


;; js turn function into f
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))
;;; tern
(require 'company)
(require 'company-tern)
(add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs")
(add-to-list 'company-backends 'company-tern)
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode t)
                           (company-mode t)))

;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))

;;; paredit
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'js2-mode-hook 'my-paredit-nonlisp) ;use with the above function
(add-hook 'coffee-mode-hook 'my-paredit-nonlisp)
(add-hook 'json-mode-hook 'my-paredit-nonlisp)
(electric-pair-mode)

;;; jshint with flycheck
(require 'flycheck)
(global-flycheck-mode)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(add-hook 'js2-mode-hook
          (lambda ()
	    (flycheck-select-checker 'javascript-eslint)
	    (flycheck-mode t)
	    (add-hook 'after-save-hook 'eslint-fix nil t)
	    ))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


;; flycheck-color-mode-line
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(add-to-list 'display-buffer-alist
	     `(,(rx bos "*Flycheck errors*" eos)
	       (display-buffer-reuse-window
		display-buffer-in-side-window)
	       (side            . bottom)
	       (reusable-frames . visible)
	       (window-height   . 0.25)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



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

;;; prettier
(require 'prettier-js)
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;;; Typescript (Tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
	(prettier-js-mode +1)
	)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook 'my-paredit-nonlisp)

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

;;; web beautify
(require 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook (lambda () (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;; (eval-after-load 'json-mode
;;   '(add-hook 'json-mode-hook (lambda () (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook (lambda () (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook (lambda () (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; (eval-after-load 'web-mode
;;   '(add-hook 'web-mode-hook (lambda () (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

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
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
;; I want this for jsx but not hbs
;;(flycheck-add-mode 'javascript-eslint 'web-mode)

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
;; (add-to-list 'load-path "/Users/jordanto/dev/emacs/ac-coffee")
;; (require 'ac-coffee)

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

(autoload 'gfm-mode "gfm-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

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

(setq-default fill-column 100)

;; SCSS
(require 'scss-mode)
(setq scss-sass-command "node-sass")
(setq-default scss-compile-at-save nil)
(setq css-indent-offset 2)

;; Show hex color
(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-07-04"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef[:digit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'scss-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)
(add-hook 'web-mode-hook 'xah-syntax-color-hex)

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
(add-hook 'markdown-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 160)
(setq whitespace-action '(auto-cleanup))
(put 'erase-buffer 'disabled nil)

;; editorconfig
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editorconfig)
(editorconfig-mode 1)

;; toggle-quotes
(require 'toggle-quotes)
(global-set-key (kbd "C-'") 'toggle-quotes)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" default)))
 '(org-cycle-level-faces t)
 '(org-fontify-whole-heading-line nil)
 '(package-selected-packages
   (quote
    (ibuffer-vc diminish flycheck-color-mode-line powerline all-the-icons all-the-icons-dired vlf prettier-js ox-reveal git-timemachine eslint-fix helm-smex zenburn-theme web-mode web-beautify toggle-quotes tide scss-mode scpaste rainbow-delimiters paredit ox-gfm nyan-mode neotree markdown-mode magit less-css-mode json-mode js2-refactor js-comint ido-ubiquitous idle-highlight-mode helm-projectile flx-ido find-file-in-project feature-mode exec-path-from-shell ember-mode editorconfig company-tern coffee-mode clojurescript-mode cider-spy cider-profile cider-eval-sexp-fu cider-decompile better-defaults ac-nrepl ac-js2 ac-emmet ac-cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-header ((t (:background "#2B2B2B" :foreground "#F0DFAF" :box (:line-width -1 :style released-button) :underline nil :weight bold :height 1.5))))
 '(org-level-1 ((t (:foreground "#DFAF8F" :height 1.5))))
 '(org-level-2 ((t (:foreground "#BFEBBF" :height 1.4))))
 '(org-level-3 ((t (:foreground "#7CB8BB" :height 1.3))))
 '(org-level-4 ((t (:foreground "#D0BF8F" :height 1.2))))
 '(org-level-5 ((t (:foreground "#93E0E3" :height 1.1)))))

;;; I like to kill and buffer and delete it at the same time
(defun delete-file-and-buffer ()
  "Kill the current buffer and delete the file being visited."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      	(progn
	  (delete-file filename)
	  (message "Deleted file %s" filename)
	  (kill-buffer)))))
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

;;; Instant access to init.el
(defun find-user-init-file ()
  "Edit init.el"
  (interactive)
  (find-file-other-frame user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)
