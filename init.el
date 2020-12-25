;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

(exec-path-from-shell-initialize)

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
;; バイトコンパイル時のワーニングを出ないようにする
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))
    (setq byte-compile-warnings '(not cl-functions obsolete)))) ;; (require 'cl) を見逃す

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))

  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

(leaf leaf-keywords
  :ensure t
  :init
  ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
  (leaf hydra :ensure t)
  (leaf el-get :ensure t)
  ;;(leaf blackout :ensure t)

  :config
  ;; initialize leaf-keywords.el
  (leaf-keywords-init))

(leaf leaf
  :config
  (leaf leaf-convert
    :ensure t
    :config (leaf use-package :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))))

(leaf leaf-convert
  :config
  (leaf cus-edit
    :custom ((custom-file . "~/.emacs.d/custom.el"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;##############################################################################
;;### Python3 settings
;;##############################################################################
(leaf python
  :mode ("\\.py\\'"))

(leaf lsp-mode
  :commands lsp lsp/python-mode-hook lsp-deferred
  :hook (python-mode-hook
         (python-mode-hook . lsp/python-mode-hook))
  :config
  (with-eval-after-load 'lsp-mode
    (dolist (dir
             '("[/\\\\]\\.venv$" "[/\\\\]\\.mypy_cache$" "[/\\\\]__pycache__$"))
      (push dir lsp-file-watch-ignored))
    (defvar lsp-auto-configure t)
    (defvar lsp-enable-completion-at-point t)
    (defvar lsp-enable-imenu nil)
    (defvar lsp-enable-xref t)
    (defvar lsp-diagnostics-provider :flycheck)
    (defvar lsp-eldoc-enable-hover t)
    (defvar lsp-eldoc-render-all nil)
    (defvar lsp-headerline-breadcrumb-enable t)
    (defvar lsp-headerline-breadcrumb-segments '(project file symbols))
    (defvar lsp-enable-snippet t)
    (setq dap-ui-menu-items nil)
    (defun lsp/python-mode-hook nil
      (when (fboundp 'company-mode)
        (setq company-minimum-prefix-length 1
              company-idle-delay 0.0)))))

(leaf lsp-pyright
  :preface
  (defun lsp-pyright/python-mode-hook nil
    (require 'lsp-pyright)
    (when (fboundp 'flycheck-mode)
      (setq flycheck-disabled-checkers '(python-mypy))))

  :commands lsp-pyright/python-mode-hook
  :hook ((python-mode-hook . lsp-pyright/python-mode-hook)))

(leaf lsp-ui
  :after lsp-mode
  :bind ((lsp-ui-mode-map
          ("M-." . lsp-ui-peek-find-definitions)
          ("M-?" . lsp-ui-peek-find-references)))
  :hook (lsp-mode-hook)
  :config
  (with-eval-after-load 'lsp-ui
    (setq lsp-ui-peek-enable t)
    (setq lsp-ui-peek-always-show t)
    (setq lsp-ui-sideline-show-diagnostics t)
    (setq lsp-ui-sideline-show-code-actions t)))

(leaf imenu-list
  :ensure t
  :require t)

(leaf company
  :init
  (global-company-mode t)
  :require t)

(leaf flycheck
  :init
  (global-flycheck-mode)
  :require t)

;;##############################################################################
;;### Python3 settings anaconda-mode, LSP
;;##############################################################################

;; フォント
(add-to-list 'default-frame-alist
                       '(font . "Source Han Code JP-13"))

;;; テーマ
(leaf doom-themes
  :custom
  ((doom-themes-enable-italic . t)
   (doom-themes-enable-bold . t))
  :custom-face
  (doom-modeline-bar . '((t (:background "#6272a4"))))
  :require t
  :config
  (load-theme 'doom-palenight t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;; モードライン
;; (leaf doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
(leaf nyan-mode
  :hook (after-init-hook))

(leaf doom-modeline
  :hook (after-init-hook)
  :custom ((doom-modeline-buffer-file-name-style quote truncate-with-project)
           (doom-modeline-icon . t)
           (doom-modeline-major-mode-icon)
           (doom-modeline-minor-modes)
           (python-shell-interpreter . "python3"))
  :config
  (with-eval-after-load 'doom-modeline
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))

;;; ダッシュボード
(leaf dashboard
  :ensure t
  :require t
  :custom((dashboard-startup-banner . "~/.emacs.d/elpa/dashboard-20201030.913/banners/1.txt"))
  ;;:custom((dashboard-start-banner . 1))
  :config
  (dashboard-setup-startup-hook))

;;; 日本語入力の設定
(leaf skk
  :ensure ddskk
  :bind (("C-x C-j" . skk-mode)
           ("C-x j" . skk-auto-fill-mode)
           ("C-x C-t" . skk-tutorial))
  :custom ((default-input-method . "japanese-skk"))
  :config
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t))
(leaf ddskk-posframe
  :custom ((ddskk-posframe-border-width . 2))
  :custom-face ((ddskk-posframe . '((t (:background "#e16b8c"))))
                (ddskk-posframe-border . '((t (:background "#d0104c"))))))

;;; GUI設定
(progn
  (global-display-line-numbers-mode)
  ;; テーマはdeeper-blueを使用している。これに合わせた色を選んだつもり
  (set-face-attribute 'line-number nil
                      :foreground "DarkOliveGreen"
                      :background "#131521")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "gold"))

(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)

(leaf highlight-indent-guides
  :ensure t
  :hook (prog-mode-hook yaml-mode-hook)
  :custom ((highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-method quote character))
  :config
  (with-eval-after-load 'highlight-indent-guides
    (if (fboundp 'diminish)
        (diminish 'highlight-indent-guides-mode)))) ; column

;;(leaf hiwin)

;; neotree
(leaf neotree
  :config
  (eval-and-compile
    (defun neotree-projectile-toggle nil
      (interactive)
      (let ((project-dir (ignore-errors
                           (projectile-project-root)))
            (file-name (buffer-file-name))
            ;;(neo-smart-open t)
            )
        (if (and
             (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
            (neotree-hide)
          (progn
            (neotree-show)
            (if project-dir
                (neotree-dir project-dir))
            (if file-name
                (neotree-find file-name)))))))

  (with-eval-after-load 'projectile
    (funcall
     (or
      (get 'neo-theme 'custom-set)
      #'set-default)
     'neo-theme 'nerd2)
    (unless (fboundp 'neotree-projectile-toggle)
      (autoload #'neotree-projectile-toggle "neotree" nil t))
    (unless (fboundp 'neotree-show)
      (autoload #'neotree-show "neotree" nil t))
    (unless (fboundp 'neotree-hide)
      (autoload #'neotree-hide "neotree" nil t))
    (unless (fboundp 'neotree-dir)
      (autoload #'neotree-dir "neotree" nil t))
    (unless (fboundp 'neotree-find)
      (autoload #'neotree-find "neotree" nil t))
    (bind-keys :package neotree
               ("<f9>" . neotree-projectile-toggle))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;######################################
;; dart-lsp
;;######################################
(leaf leaf-convert
  :config
  (condition-case nil
      (require 'use-package)
    (file-error
     (require 'package)
     (add-to-list 'package-archives
                  '("melpa" . "http://melpa.org/packages/"))
     (package-initialize)
     (package-refresh-contents)
     (package-install 'use-package)
     (require 'use-package))))

(leaf lsp-mode
  :ensure t
  :require t)

(leaf lsp-dart
  :ensure t
  :commands lsp
  :hook ((dart-mode-hook . lsp)))

;; Optional packages
(leaf projectile
  :ensure t
  :require t) ;; project management
(leaf yasnippet
  :ensure t
  :require t
  :config
  (yas-global-mode)) ;; snipets
;;(use-package lsp-ui :ensure t) ;; UI for LSP
;;(use-package company :ensure t) ;; Auto-complete

;; Optional Flutter packages
(leaf hover
  :ensure t
  :require t) ;; run app from desktop without emulator


(leaf cus-start
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom ((user-full-name . "behike56")
           (user-mail-address . "living.tech.dancing.code@gmail.com")
           (user-login-name . "begike56")
           (create-lockfiles)
           (debug-on-error . t)
           (init-file-debug . t)
           (frame-resize-pixelwise . t)
           (enable-recursive-minibuffers . t)
           (history-length . 1000)
           (history-delete-duplicates . t)
           (scroll-preserve-screen-position . t)
           (scroll-conservatively . 100)
           (mouse-wheel-scroll-amount quote
                                      (1
                                       ((control)
                                        . 5)))
           (ring-bell-function quote ignore)
           (text-quoting-style quote straight)
           (truncate-lines . t)
           (scroll-bar-mode)
           (indent-tabs-mode))
  :config
  (with-eval-after-load 'cus-start
    (defalias 'yes-or-no-p 'y-or-n-p)
    (keyboard-translate 8 127)))

(eval-and-compile
  (leaf bytecomp
    :doc "compilation of Lisp code into byte code"
    :tag "builtin" "lisp"
    :custom (byte-compile-warnings quote
                                   (cl-functions))))

(leaf autorevert
  :commands global-auto-revert-mode
  :custom ((auto-revert-interval . 0.3)
           (auto-revert-check-vc-info . t))
  :config
  (global-auto-revert-mode 1))

(leaf delsel
  :commands delete-selection-mode
  :config
  (delete-selection-mode 1))

(leaf which-key
  :ensure t
  :hook (after-init-hook)
  :config
  (with-eval-after-load 'which-key
    (if (fboundp 'diminish)
        (diminish 'which-key-mode))))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook))

;; (leaf paren
;;   :hook (after-init-hook)
;;   :commands show-paren-mode
;;   :custom ((show-paren-delay . 0.1))
;;   :config
;;   (show-paren-mode 1))
(leaf paren
  :ensure nil
  :commands show-paren-mode
  :hook ((after-init-hook . show-paren-mode))
  :custom ((show-paren-style quote mixed)
           (show-paren-when-point-inside-paren . t)
           (show-paren-when-point-in-periphery . t))
  :custom-face ((show-paren-match quote
                                  ((nil
                                    (:background "#44475a" :foreground "#f1fa8c"))))))

(leaf files
  :custom ((auto-save-timeout . 15)
           (auto-save-interval . 60)
           (auto-save-file-name-transforms quote
                                           ((".*" "~/.emacs.d/backup/" t)))
           (backup-directory-alist quote
                                   ((".*" . "~/.emacs.d/backup")
                                    ("^/\\(\\(?:\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:[a-zA-Z0-9]*:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?|\\)+\\)?\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:[a-zA-Z0-9]*:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?:\\([^\n]*\\'\\)")))
           (version-control . t)
           (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  ;;:blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
                                      (swiper . ivy--regex-plus)))
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    ;;:blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf ivy-rich
  :doc "More friendly display transformer for ivy."
  :req "emacs-24.5" "ivy-0.8.0"
  :tag "ivy" "emacs>=24.5"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :commands (prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
            (prescient-save-file . ,(locate-user-emacs-file "prescient")))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf flycheck
  :commands global-flycheck-mode flycheck-next-error flycheck-previous-error
  :config
  (when (version<= "24.3" emacs-version)
    (leaf-handler-package flycheck flycheck nil)
    (leaf-keys
     (("M-n" . flycheck-next-error)
      ("M-p" . flycheck-previous-error)))
    (global-flycheck-mode 1)))

(leaf company
  :commands global-company-mode company-filter-candidates company-select-next company-select-previous company-complete-selection
  :config
  (when (version<= "24.3" emacs-version)
    (leaf-handler-package company company nil)
    (leaf-keys
     ((company-active-map :package company
                          ("M-n")
                          ("M-p")
                          ("C-s" . company-filter-candidates)
                          ("C-n" . company-select-next)
                          ("C-p" . company-select-previous)
                          ("<tab>" . company-complete-selection))
      (company-search-map :package company
                          ("C-n" . company-select-next)
                          ("C-p" . company-select-previous))))
    (customize-set-variable 'company-idle-delay 0 "Customized with leaf in company block")
    (customize-set-variable 'company-minimum-prefix-length 1 "Customized with leaf in company block")
    (customize-set-variable 'company-transformers
                            '(company-sort-by-occurrence)
                            "Customized with leaf in company block")
    (global-company-mode 1)
    ;; (with-eval-after-load 'company
    ;;  (blackout 'company-mode nil))
    ))

(leaf company-c-headers
  :defvar company-backends
  :config
  (when (version<= "24.1" emacs-version)
    (leaf-handler-package company-c-headers company-c-headers nil)
    (eval-after-load 'company
      '(progn
         (add-to-list 'company-backends 'company-c-headers)))))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
